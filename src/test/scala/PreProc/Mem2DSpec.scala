// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// SPEC
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class Mem2DSpec extends AnyFlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // Mem2D
  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  it should "test Mem2D functionality with divided memory" in {
    val params = Mem2DParams(
      dim1 = 64,
      dim2 = 16,
      blockRamDim = 128,
      divideMem = true,
    )
    val beats = 2
    val lazyDut = LazyModule(new Mem2D(params) with Mem2DStandaloneBlock{override def beatBytes = beats})
    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/Mem2D", "--top-name", "Mem2D")) {
      c => new Mem2DTester(lazyDut, params, beats, true)
    } should be (true)
  }

  it should "test Mem2D functionality without divided memory" in {
    val params = Mem2DParams(
      dim1 = 64,
      dim2 = 16,
      blockRamDim = 128,
      divideMem = false,
    )
    val beats = 2
    val lazyDut = LazyModule(new Mem2D(params) with Mem2DStandaloneBlock{override def beatBytes = beats})
    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/Mem2D", "--top-name", "Mem2D")) {
      c => new Mem2DTester(lazyDut, params, beats, true)
    } should be (true)
  }


}