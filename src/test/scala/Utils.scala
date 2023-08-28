
//some common test utils here
import Stages._
import spinal.core.sim._
import spinal.sim._
import spinal.core._
import spinal.lib.sim.StreamReadyRandomizer

import scala.util.Random

object Utils {
  //operator the register
  def writeRegfile(dut:Regfile,index: Int, data: BigInt) = {
    dut.io.write #= true
    dut.io.data #= data
    dut.io.rs0 #= 0
    dut.io.rs1 #= 0
    dut.io.rd #= index
    dut.clockDomain.waitSampling()
  }
  def readRegfile(dut:Regfile,rs0: Int,rs1 : Int) = {
    dut.io.write #= false
    dut.io.data #= 0
    dut.io.rs0 #= rs0
    dut.io.rs1 #= rs1
    dut.io.rd #= 0
    dut.clockDomain.waitSampling()
  }
  def loopWrite(dut:Regfile,iter:Int = 32,random:Boolean = false): Unit = {
    for(idx <- 0 until iter){
      val data = if(random) Random.nextInt(1000) else idx
      writeRegfile(dut,idx,data)
    }
    dut.clockDomain.waitSampling()  //write the last data
  }

  //set instruction in decode stages
  def setInstruction(dut:Decode,instruction: BigInt) = {
      dut.io.inInst.valid #= true
      dut.io.inInst.instruction #= instruction
      dut.io.Internalflush #= false
      dut.clockDomain.waitSampling()
  }


}
