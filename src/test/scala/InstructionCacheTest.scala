
//monitor the instruction cache about the flush/read/fill

import Memory._
import BCore.Misc.Config.instructionCacheConfig
import BCore.RiscvCoreConfig
import CoreSim._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import DefineSim.SpinalSim
import DefineSim._
import DefineSim.StramFlowSim._


class InstructionCacheTest extends AnyFunSuite{
  var compiled:SimCompiled[InstructionCache] = null
  test("compile"){
    compiled = SpinalSim().compile{
      val dut = new InstructionCache()(instructionCacheConfig)
      dut
    }
  }


  //use the  Axi Memorysim
//  test("testBench"){
//    //first get the mem data / then cpu fetch the data / last test the flush bus
//
//    compiled.doSim(seed = 42){
//      dut =>
//        dut.clockDomain.forkStimulus(10)
//
//        def setMemBusValue(payload: InstructionCacheMemCmd): Boolean = {
//
//          payload.address #=
//
//          if (dut.io.instructionSignal.cmd.valid.toBoolean && dut.io.instructionSignal.cmd.ready.toBoolean) {
//            number += 1
//          }
//          true
//        }






//
//    }
//  }





}
