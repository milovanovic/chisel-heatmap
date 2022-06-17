package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.IO

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// HDMIProcFFT2D parameters
case class HDMIProcFFT2DParams(
    memParams : Mem2DBlockParams,
    pipe : Boolean,
)

// HDMIProcFFT2D Bundle
class HDMIProcFFT2DIO(params: Mem2DBlockParams) extends Bundle {
    val i_max    = Input(UInt(5.W))
    val o_data   = Output(UInt(24.W))
    val i_addr_x = if (params.divideMem) Some(Input(UInt(log2Ceil(params.blockRamDim).W))) else Some(Input(UInt(log2Ceil(params.dim1 * params.dim2).W)))
    val i_addr_y = if (params.divideMem) Some(Input(UInt(log2Ceil(params.dim1 * params.dim2 / params.blockRamDim).W))) else Some(Input(UInt(0.W)))

    override def cloneType: this.type = HDMIProcFFT2DIO(params).asInstanceOf[this.type]
}
object HDMIProcFFT2DIO {
  def apply(params: Mem2DBlockParams): HDMIProcFFT2DIO = new HDMIProcFFT2DIO(params)
}

class HDMIProcFFT2D(params: HDMIProcFFT2DParams, beatBytes: Int) extends LazyModule()(Parameters.empty){
    val inNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
    // IO
    lazy val io = Wire(new HDMIProcFFT2DIO(params.memParams))

    // Modules
    val mem2d = LazyModule(new Mem2DBlock(params.memParams, beatBytes) with Mem2DBlockStandaloneBlock)
    val s2rgb = LazyModule(new Scaler2RGBBlock(beatBytes) with Scaler2RGBBlockPins)

    lazy val module = new LazyModuleImp(this) {
        // IOs
        val (in, _) = inNode.in(0)

        in.ready := true.B
        mem2d.ioBlock.i_en   := in.valid
        mem2d.ioBlock.i_data := in.bits.data
        mem2d.ioBlock.i_addr_x.get := io.i_addr_x.get
        mem2d.ioBlock.i_addr_y.get := io.i_addr_y.get

        if (params.pipe) s2rgb.ioBlock.i_data := RegNext(mem2d.ioBlock.o_data, 0.U)
        else s2rgb.ioBlock.i_data := mem2d.ioBlock.o_data

        
        if (params.pipe) io.o_data := RegNext(s2rgb.ioBlock.o_data, 0.U)
        else io.o_data := s2rgb.ioBlock.o_data
        
        // // Data scaler
        s2rgb.ioBlock.i_max := io.i_max
    }
}

trait HDMIProcFFT2DStandaloneBlock extends HDMIProcFFT2D{
    def beatBytes: Int = 2

    // Node
    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    inNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
    val in = InModuleBody { ioInNode.makeIO() }

    // IO
    def makeCustomIO(): HDMIProcFFT2DIO = {
        val io2: HDMIProcFFT2DIO = IO(io.cloneType)
        io2.suggestName("io")
        io2 <> io
        io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
}

object HDMIProcFFT2DApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = HDMIProcFFT2DParams (
    memParams = Mem2DBlockParams (
        dim1 = 512,
        dim2 = 256,
        blockRamDim = 2048,
        divideMem = false,
    ),
    pipe = true,
  )

  val lazyDut = LazyModule(new HDMIProcFFT2D(params, 2) with HDMIProcFFT2DStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/HDMIProcFFT2D"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}