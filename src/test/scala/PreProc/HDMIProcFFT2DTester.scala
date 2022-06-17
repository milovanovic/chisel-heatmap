// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._

import dsptools._

import scala.util.Random

class HDMIProcFFT2DTester
(
  dut: HDMIProcFFT2D with HDMIProcFFT2DStandaloneBlock,
  params: HDMIProcFFT2DParams,
  beatBytes: Int,
  silentFail: Boolean = false
) extends DspTester(dut.module) with AXI4StreamModel{
    
  def seqToString(c: Seq[Double]): String = "[" + c.mkString(", ") + "]"

  val mod = dut.module

  val dataSize = params.memParams.dim1*params.memParams.dim2 // Data size

  // Data and expected data
  var expectedData = Seq[Int]()
  var data = Seq[Int]()

  // data
  for (i <- 0 until dataSize) {
    data = data :+ Random.nextInt((1 << beatBytes*8) - 1)
    
  }
  if (params.memParams.divideMem) {
    for (i <- 0 until params.memParams.dim1 * params.memParams.dim2 / params.memParams.blockRamDim) {
      for (j <- 0 until params.memParams.blockRamDim) {
        expectedData = expectedData :+ data(j*(params.memParams.dim1 * params.memParams.dim2 / params.memParams.blockRamDim) + i)
      }
    }
  }
  else {
    expectedData = data
  }
  if (params.pipe) expectedData = Seq(data(0), data(0)) ++ expectedData
  else expectedData = Seq(data(0)) ++ expectedData

  // Write data
  while(!data.isEmpty) {
    poke(dut.in.valid, 1)//Random.nextInt(2))
    if (peek(dut.in.valid) == true && peek(dut.in.ready) == true) {
      poke(dut.in.bits.data, data.head)
      data = data.tail
    }
    step(1)
  }
  poke(dut.in.valid, 0)//Random.nextInt(2))
  

  // Read data
  var addr_x = 0
  var addr_y = 0
  

  while(!expectedData.isEmpty) {
    if(params.memParams.divideMem){
      poke(dut.ioBlock.i_addr_x.get, addr_x)
      poke(dut.ioBlock.i_addr_y.get, addr_y)
    }
    else {
      poke(dut.ioBlock.i_addr_x.get, addr_x)
    }
    // Expect data
    expect(dut.ioBlock.o_data, Utils.scaler2rgb(expectedData.head, beatBytes*8))
    expectedData = expectedData.tail
    
    // increase counters
    if(params.memParams.divideMem){
      if (addr_x < params.memParams.blockRamDim-1) {
        addr_x = addr_x + 1
      }
      else {
        addr_x = 0
        if (addr_y < params.memParams.dim1 * params.memParams.dim2 / params.memParams.blockRamDim - 1) {
          addr_y = addr_y + 1
        }
        else {
          addr_y = 0
        }
      }
    }
    else{
      if (addr_x < params.memParams.dim1*params.memParams.dim2-1) {
        addr_x = addr_x + 1
      }
      else {
        addr_x = 0
      }
    }
    // Step
    step(1)
  }
  stepToCompletion(maxCycles = 10*dataSize, silentFail = silentFail)
}

