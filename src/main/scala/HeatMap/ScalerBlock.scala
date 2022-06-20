// SPDX-License-Identifier: Apache-2.0

package hdmi.heatmap 

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.IO
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._


// ScalerBlock Bundle
class ScalerBlockIO extends Bundle {
    val o_scalerData = Output(UInt(5.W))
    val o_scalerAxis = Output(UInt(10.W))
}

abstract class ScalerBlock(beatBytes: Int) extends LazyModule()(Parameters.empty) with HasCSR {
    // IO
    lazy val io = Wire(new ScalerBlockIO)

    lazy val module = new LazyModuleImp(this) {

        // Control registers
        val r_scaleData = RegInit(0.U(5.W))
        val r_scaleAxis = RegInit(0.U(10.W))
        io.o_scalerData := r_scaleData
        io.o_scalerAxis := r_scaleAxis

        // Define register fields
        val fields = Seq(RegField(r_scaleData.getWidth, r_scaleData, RegFieldDesc(name = "r_scaleData", desc = "Register used to set the data scaler")),
                         RegField(r_scaleAxis.getWidth, r_scaleAxis, RegFieldDesc(name = "r_scaleAxis", desc = "Register used to set the axis scaler")))
                         
        // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
    }
}

class AXI4ScalerBlockBlock(address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends ScalerBlock(beatBytes) {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
  override def regmap(mapping: (Int, Seq[RegField])*): Unit = mem.get.regmap(mapping:_*)
}


trait AXI4ScalerBlockPins extends AXI4ScalerBlockBlock {
    def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    val ioMem = mem.map { m => {
        val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

        m :=
        BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
        ioMemNode

        val ioMem = InModuleBody { ioMemNode.makeIO() }
        ioMem
    }}

    // IO
    def makeCustomIO(): ScalerBlockIO = {
        val io2: ScalerBlockIO = IO(io.cloneType)
        io2.suggestName("io")
        io2 <> io
        io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
}


object ScalerBlockApp extends App
{
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4ScalerBlockBlock(AddressSet(0x00, 0xF), beatBytes = 4) with AXI4ScalerBlockPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/ScalerBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}