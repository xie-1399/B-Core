
//test the decode unit
import BCore.DecodeInfo.OP1
import BCore.RiscvCoreConfig
import Stages._
import CoreSim._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import DefineSim.SpinalSim._
import DefineSim.SpinalSim
import DefineSim.SIMCFG
import spinal.lib.sim.StreamReadyRandomizer


class DecodeTest extends AnyFunSuite{
  var compiled:SimCompiled[instructionctrl] = null
  test("compile"){
    compiled = SpinalSim().compile{
      val dut = new instructionctrl
      dut.ctrl.simPublic()
      dut
    }
  }

  //a online page:https://hggshiwo.github.io/rimulator/index.html
  test("instructionCtrl Test"){
    compiled.doSim(seed = 42) {
      dut =>
        //Todo add more instruction
        dut.clockDomain.forkStimulus(10)
        def generateCode(opretion:String): BigInt = {
          val code = opretion match {
            case "ADD" => 0x003080b3
            case "SUB" => 0x403100b3
            case "SW" => 0x00152223
            case "LD" => 0x00452083
            case "ADDI" => 0x00408093
            case "AUIPC" => 0x3adeaa17
            case "LUI" => 0x004649b7
            case "JALR" => 0x064404e7
            case _ => 0
          }
          code
        }
        //just a simple test it
        val codeName = List[String]("ADD","SUB","SW","LD","ADDI","AUIPC","LUI","JALR")
        for (idx <- 0 until codeName.length){
          val code = generateCode(codeName(idx))
          dut.io.instruction #= code
          dut.clockDomain.waitSampling()
        }
    }
  }
}

// another way to use the sim situation
class DecodeSimTest extends AnyFunSuite{
  test("only sim the decode"){
    SIMCFG(gtkFirst = true).compile{
      val config = RiscvCoreConfig()
      val dut = new DecodeSim()(config)
      addSimPublic(List(dut.decode.io.Internalflush,dut.regfile.io,dut.decode.io))
      dut.regfile.regfile.simPublic()
      dut
    }.doSim{
      dut =>
        dut.clockDomain.forkStimulus(10)
        dut.decode.io.Internalflush #= false  //no flush signal
        dut.clockDomain.waitSampling(5)
        //write some data in the reg file
        def write(index: Int, data: BigInt) = {
          dut.decode.io.inInst.valid #= false
          dut.regfile.io.write #= true
          dut.regfile.io.data #= data
          dut.regfile.io.rs0 #= 0
          dut.regfile.io.rs1 #= 0
          dut.regfile.io.rd #= index
          dut.clockDomain.waitSampling()
        }

        def SetInstruction(instruction:BigInt,aluOp0:Int,aluOp1:Int,iter : Int = 10,assertion:() => Unit = null,
                           withMemory:Boolean = false) = {
          for(idx <- 0 until iter){
            dut.regfile.io.write #= false
            dut.decode.io.inInst.valid #= true
            dut.decode.io.inInst.instruction #= instruction
            dut.decode.io.Internalflush #= false
            dut.clockDomain.waitSampling()
            if (dut.decode.io.inInst.valid.toBoolean && dut.decode.io.inInst.ready.toBoolean) {
              assert(dut.decode.io.decodeOutput.alu_op0.toBigInt == aluOp0)
              assert(dut.decode.io.decodeOutput.alu_op1.toBigInt == aluOp1)
              if (withMemory) assert(dut.decode.io.decodeOutput.ctrl.useMemory.toBoolean)
            }
          }
        }
        for (idx <- 0 until 32) {
          write(idx, idx)
        }
        dut.clockDomain.waitSampling(5)
        StreamReadyRandomizer(dut.decode.io.decodeOutput,dut.clockDomain)
        //one very simple instruction (add x1,x2,x3)
        SetInstruction(0x00408093,1,4)  //addi x1,x1,4
        SetInstruction(0x00720133,4,7)  //add x2,x4,x7
        SetInstruction(0x0823a023,7,2, withMemory = true)  // sw x2,0x80,x7（sw rs2 -> M(rs1 + offset)）
    }
  }


  //using the utils instead
  test("sim the decode with fetch") {
    SIMCFG(gtkFirst = true).compile {
      val config = RiscvCoreConfig(addressWidth = 6,pcWidth = 6,startAddress = 0x00000000)
      val dut = new DecodeWithFetch()(config)
      dut
    }.doSim {
      dut =>
        dut.clockDomain.forkStimulus(10)
    }
  }


}