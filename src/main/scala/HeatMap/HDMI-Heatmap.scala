package hdmi.heatmap

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.{FixedPoint, IO}
import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import hdmi._
import hdmi.frameBuffer._
import hdmi.preproc._

// HDMIHeatMap parameters
case class HDMIHeatMapParameters[T <: Data: Real: BinaryRepresentation] (
  fftprocParams  : HDMIProcFFT2DParams,
  frameBufferParams : FrameBuffer2DParameters
)

// HDMIHeatMap Addresses
case class HDMIHeatMapAddresses (
  scalerAddress   : AddressSet
)

// HDMIHeatMap IO
class HDMIHeatMapIO extends Bundle {
    // tmds output ports
    val clk_p  = Output(Bool())
    val clk_n  = Output(Bool())
    val data_p = Output(UInt(3.W))
    val data_n = Output(UInt(3.W))

    // Reset and clock
    val clk_pixel  = Input(Clock())
    val clk_serdes = Input(Clock())
    val reset_hdmi = Input(Bool())
}

class HDMIHeatMap[T <: Data: Real: BinaryRepresentation](params: HDMIHeatMapParameters[T], address: HDMIHeatMapAddresses, beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val fft2Dproc  = LazyModule(new HDMIProcFFT2D(params.fftprocParams, 2){
    // IO
    def makeCustomIO(): HDMIProcFFT2DIO = {
        val io2: HDMIProcFFT2DIO = IO(io.cloneType)
        io2.suggestName("io")
        io2 <> io
        io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
  })
  val scaler = LazyModule(new AXI4ScalerBlockBlock(address.scalerAddress, beatBytes = 4){
    // IO
    def makeCustomIO(): ScalerBlockIO = {
        val io2: ScalerBlockIO = IO(io.cloneType)
        io2.suggestName("io")
        io2 <> io
        io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
  })
  val asyncQueue = LazyModule(new AsyncQueueModule())

  // connect nodes
  fft2Dproc.inNode := asyncQueue.streamNode

  // define mem
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  scaler.mem.get := bus.node

  lazy val module = new LazyModuleImp(this) {
      // IO
      val io = IO(new HDMIHeatMapIO)

      // Wires
      val vsync   = Wire(Bool())
      val hsync   = Wire(Bool())
      val pixel_x = Wire(UInt(16.W))
      val pixel_y = Wire(UInt(16.W))
      val video_active = Wire(Bool())
      val video_data   = Wire(UInt(24.W))

      // Connect fft2Dproc clock and reset
      fft2Dproc.module.clock := io.clk_pixel
      fft2Dproc.module.reset := io.reset_hdmi

      // video timing
      val timeGen = Module(new TimingGenerator(params = TimingGeneratorParams()))
      timeGen.clock := io.clk_pixel
      timeGen.reset := io.reset_hdmi
      video_active  := timeGen.io.video_active
      hsync   := timeGen.io.hsync
      vsync   := timeGen.io.vsync
      pixel_x := timeGen.io.pixel_x
      pixel_y := timeGen.io.pixel_y

      // tmds signaling
      val rgb2tmds = Module(new RGB2tmds)
      rgb2tmds.rst          := io.reset_hdmi
      rgb2tmds.pixelclock   := io.clk_pixel
      rgb2tmds.serialclock  := io.clk_serdes
      rgb2tmds.video_data   := video_data
      rgb2tmds.video_active := video_active
      rgb2tmds.hsync        := hsync
      rgb2tmds.vsync        := vsync
      io.clk_p  := rgb2tmds.clk_p
      io.clk_n  := rgb2tmds.clk_n
      io.data_p := rgb2tmds.data_p
      io.data_n := rgb2tmds.data_n

      withClockAndReset(io.clk_pixel, io.reset_hdmi){
        val frameBuffer = Module(new FrameBuffer2D(params.frameBufferParams, log2Ceil(128)))

        // async
        asyncQueue.module.clock2 := io.clk_pixel
        asyncQueue.module.reset2 := io.reset_hdmi

        // frameBuffer
        frameBuffer.clock      := io.clk_pixel
        frameBuffer.reset      := io.reset_hdmi
        frameBuffer.io.pixel_x := pixel_x
        frameBuffer.io.pixel_y := pixel_y
        frameBuffer.io.scaler_y := 1.U //TODO: FIX THIS
        frameBuffer.io.video_active := video_active
        frameBuffer.io.inData := fft2Dproc.ioBlock.o_data
        if (params.fftprocParams.memParams.divideMem) {
          fft2Dproc.ioBlock.i_addr_x.get := frameBuffer.io.i_addr_x
          fft2Dproc.ioBlock.i_addr_y.get := frameBuffer.io.i_addr_y
        }
        else {
          fft2Dproc.ioBlock.i_addr_x.get := Cat(frameBuffer.io.i_addr_y, frameBuffer.io.i_addr_x)
        }

        fft2Dproc.ioBlock.i_max := scaler.ioBlock.o_scalerData
        frameBuffer.io.i_scaler := scaler.ioBlock.o_scalerAxis

        video_data := frameBuffer.io.rgb
      }
    }
}

trait HDMIHeatMapPins extends HDMIHeatMap[FixedPoint] {
  def beatBytes: Int = 4

  // Generate AXI4 slave output
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  asyncQueue.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioInNode
  val in0 = InModuleBody { ioInNode.makeIO() }
}


class HDMIHeatMapParams(rangeFFT: Int = 512, dopplerFFT: Int = 256) {
  val params : HDMIHeatMapParameters[FixedPoint] = HDMIHeatMapParameters (
    fftprocParams = HDMIProcFFT2DParams (
      memParams = Mem2DBlockParams (
          dim1 = rangeFFT,
          dim2 = dopplerFFT,
          blockRamDim = 2048,
          divideMem = false,
      ),
      pipe = true,
    ),
    frameBufferParams = FrameBuffer2DParameters(
      logo = false,
      imageParams = (new HD1080p2D).params,
      beatBytes = 4
    )
  )
}

class HDMIHeatMapAddr(startAddress: BigInt = 0x0000) {
  val addresses = HDMIHeatMapAddresses (
    scalerAddress = AddressSet(startAddress, 0xFF)
  )
}

object HDMIHeatMapApp extends App
{
  val params = (new HDMIHeatMapParams).params
  val address = (new HDMIHeatMapAddr(0x0000)).addresses
  val lazyDut = LazyModule(new HDMIHeatMap(params, address, 4) with HDMIHeatMapPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/HDMIHeatMap"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

