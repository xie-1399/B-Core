
//monitor the instruction cache about the flush/read/fill

import Memory._
import BCore.Misc.Config.instructionCacheConfig
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
}
