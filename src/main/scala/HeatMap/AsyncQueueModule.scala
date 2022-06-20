package hdmi.heatmap

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

// TODO : Set reset2 to asynchronous reset

class AsyncQueueModule(xilinxAFIFO: Boolean = false) extends LazyModule()(Parameters.empty) {
    val streamNode = AXI4StreamIdentityNode()

    lazy val module = new LazyModuleImp(this) {
        val (in, _ )  = streamNode.in(0)
        val (out, _ ) = streamNode.out(0)

        // Inputs
        val clock2 = IO(Input(Clock())) 
        val reset2 = IO(Input(Bool()))

        if (xilinxAFIFO) {
            val asyncQueue0 = Module(new AFIFO_SMALL)
            asyncQueue0.io.s_axis_tdata  := in.bits.data
            asyncQueue0.io.s_axis_tvalid := in.valid
            in.ready := asyncQueue0.io.s_axis_tready
            asyncQueue0.io.s_axis_aclk := clock
            asyncQueue0.io.s_axis_aresetn := !reset.asBool()

            asyncQueue0.io.m_axis_aclk   := clock2
            asyncQueue0.io.m_axis_tready := out.ready
            out.bits.data := asyncQueue0.io.m_axis_tdata
            out.valid     := asyncQueue0.io.m_axis_tvalid
        }
        else {
            // Async queue
            val asyncQueue0 = Module(new AsyncQueue(chiselTypeOf(in.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))
            // Connect inputs
            asyncQueue0.io.enq <> in
            asyncQueue0.io.enq_clock := clock
            asyncQueue0.io.enq_reset := reset.asBool()

            out.bits.data := asyncQueue0.io.deq.bits.data
            out.bits.last := asyncQueue0.io.deq.bits.last
            asyncQueue0.io.deq.ready := out.ready
            out.valid := asyncQueue0.io.deq.valid
            asyncQueue0.io.deq_clock := clock2
            asyncQueue0.io.deq_reset := reset2.asBool()
        }
        
    }
}

trait AsyncQueueModuleStandaloneBlock extends AsyncQueueModule {
    def beatBytes: Int = 2

    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

    ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode

    val in = InModuleBody { ioInNode.makeIO() }
    val out = InModuleBody { ioOutNode.makeIO() }
}


object AsyncQueueModuleApp extends App
{
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AsyncQueueModule() with AsyncQueueModuleStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/AsyncQueueModule"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}