import BCore.RiscvCoreConfig
import CoreSim.{ExcuteSim, FetchMemory}
import DefineSim.SpinalSim
import DefineSim.SpinalSim.addSimPublic
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.SimCompiled
import Stages._
import spinal.core.sim._
import DefineSim.SIMCFG
import Utils._
import spinal.lib.sim.StreamReadyRandomizer

class ExcuteTest extends AnyFunSuite{

  test("excute with decode"){
   SIMCFG(gtkFirst = true).compile{
     val comfig = new RiscvCoreConfig()
     val dut = new ExcuteSim()(comfig)
     addSimPublic(List(dut.decode.io,dut.excute.io,dut.regfile.io))
     dut
   }.doSim{
     dut =>
       dut.clockDomain.forkStimulus(10)
       def initial() = {
         dut.excute.io.Internalflush #= false
         dut.excute.io.pendingHalt #= false
         dut.clockDomain.waitSampling(5)
       }
       initial()
       //first write the regfile with loop
       loopWrite(dut.regfile)
       StreamReadyRandomizer(dut.excute.io.outInst,dut.clockDomain)


       setInstruction(dut.decode,0x00408093)
       setInstruction(dut.decode,0x00720133)
       setInstruction(dut.decode,0x0823a023)
   }
  }




}
