// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._

import dsptools._

import scala.util.Random

class Mem2DTester
(
  dut: Mem2D with Mem2DStandaloneBlock,
  params: Mem2DParams,
  beatBytes: Int,
  silentFail: Boolean = false
) extends DspTester(dut.module) with AXI4StreamModel{
    
  def seqToString(c: Seq[Double]): String = "[" + c.mkString(", ") + "]"

  val mod = dut.module

  val dataSize = params.dim1*params.dim2 // Data size

  // Data and expected data
  var expectedData = Seq[Int]()
  var data = Seq[Int]()

  // data
  for (i <- 0 until dataSize) {
    data = data :+ Random.nextInt((1 << beatBytes*8) - 1)
    
  }
  if (params.divideMem) {
    for (i <- 0 until params.dim1 * params.dim2 / params.blockRamDim) {
      for (j <- 0 until params.blockRamDim) {
        expectedData = expectedData :+ data(j*(params.dim1 * params.dim2 / params.blockRamDim) + i)
      }
    }
  }
  else {
    for (i <- 0 until params.dim2) {
      for (j <- 0 until params.dim1) {
        expectedData = expectedData :+ data(j*(params.dim2) + i)
      }
    }
  }

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
    if(params.divideMem){
      poke(dut.ioBlock.i_addr_x.get, addr_x)
      poke(dut.ioBlock.i_addr_y.get, addr_y)
    }
    else {
      poke(dut.ioBlock.i_addr_x.get, addr_x)
    }
    poke(dut.out.ready, 1)//Random.nextInt(2))
    if (peek(dut.out.ready) == true && peek(dut.out.valid) == true) {
      expect(dut.out.bits.data, expectedData.head)
      expectedData = expectedData.tail
    }
    // increase counters
    if(peek(dut.out.ready) == true) {
      if(params.divideMem){
        if (addr_x < params.blockRamDim-1) {
          addr_x = addr_x + 1
        }
        else {
          addr_x = 0
          if (addr_y < params.dim1 * params.dim2 / params.blockRamDim - 1) {
            addr_y = addr_y + 1
          }
          else {
            addr_y = 0
          }
        }
      }
      else{
        if (addr_x < params.dim1*params.dim2-1) {
          addr_x = addr_x + 1
        }
        else {
          addr_x = 0
        }
      }
    }
    step(1)
  }
  stepToCompletion(maxCycles = 10*dataSize, silentFail = silentFail)
}

