package hdmi.heatmap

import chisel3._
import chisel3.experimental._

object booleanToVerilogVectorParam extends (Boolean => RawParam) {
  def apply(b : Boolean) : RawParam =  if(b) RawParam("1") else RawParam("0")
}

object booleanToVerilogStringParam extends (Boolean => StringParam) {
  def apply(b : Boolean) : StringParam = if(b) StringParam("""TRUE""") else StringParam("""FALSE""")
}

// Async FIFO
class AFIFO_SMALL extends BlackBox {
    val io = IO(new Bundle {
        val s_axis_aresetn = Input(Bool())
        val s_axis_aclk    = Input(Clock())
        val s_axis_tvalid  = Input(Bool())
        val s_axis_tready  = Output(Bool())
        val s_axis_tdata   = Input(UInt(16.W))
        val m_axis_aclk    = Input(Clock())
        val m_axis_tvalid  = Output(Bool())
        val m_axis_tready  = Input(Bool())
        val m_axis_tdata   = Output(UInt(16.W))
    })
}