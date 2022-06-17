// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// SPEC
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class HDMIProcFFT2DSpec extends AnyFlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // HDMIProcFFT2D
  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  it should "test HDMIProcFFT2D functionality with divided memory without pipe" in {
    val params = HDMIProcFFT2DParams (
      memParams = Mem2DBlockParams (
          dim1 = 64,
          dim2 = 16,
          blockRamDim = 128,
          divideMem = true,
      ),
      pipe = false,
    )
    val beats = 2
    val lazyDut = LazyModule(new HDMIProcFFT2D(params, beats) with HDMIProcFFT2DStandaloneBlock{override def beatBytes = beats})
    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/HDMIProcFFT2D", "--top-name", "HDMIProcFFT2D")) {
      c => new HDMIProcFFT2DTester(lazyDut, params, beats, true)
    } should be (true)
  }

  it should "test HDMIProcFFT2D functionality with divided memory with pipe" in {
    val params = HDMIProcFFT2DParams (
      memParams = Mem2DBlockParams (
          dim1 = 64,
          dim2 = 16,
          blockRamDim = 128,
          divideMem = true,
      ),
      pipe = true,
    )
    val beats = 2
    val lazyDut = LazyModule(new HDMIProcFFT2D(params, beats) with HDMIProcFFT2DStandaloneBlock{override def beatBytes = beats})
    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/HDMIProcFFT2D", "--top-name", "HDMIProcFFT2D")) {
      c => new HDMIProcFFT2DTester(lazyDut, params, beats, true)
    } should be (true)
  }

  // it should "test HDMIProcFFT2D functionality without divided memory" in {
  //   val params = HDMIProcFFT2DParams(
  //     dim1 = 64,
  //     dim2 = 32,
  //     blockRamDim = 128,
  //     divideMem = false,
  //   )
  //   val beats = 2
  //   val lazyDut = LazyModule(new HDMIProcFFT2D(params) with HDMIProcFFT2DStandaloneBlock{override def beatBytes = beats})
  //   dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/HDMIProcFFT2D", "--top-name", "HDMIProcFFT2D")) {
  //     c => new HDMIProcFFT2DTester(lazyDut, params, beats, true)
  //   } should be (true)
  // }
}