package Build

/* the build pipeline can fetch the instruction from the itcm using io port */
import DefineBus.SimpleBus._
import DefineSim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import DefineSim.SpinalSim._

class FetchSim extends AnyFunSuite {

  test("fetch from the itcm with random halt it") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new pipelineCore(coreParameters())
      addSimPublic(List(dut.pipeline.onFetch.fetchOut))
      dut
    }.doSim {
      dut =>
        dut.clockDomain.forkStimulus(10)
        var index = 0
        dut.clockDomain.waitSamplingWhere{
          dut.io.haltCpu #= Random.nextInt(10) > 4 // Random halt it to test
          if(dut.pipeline.onFetch.fetchOut.valid.toBoolean && dut.pipeline.onFetch.fetchOut.ready.toBoolean){
            index += 1
            if(index == 1){
              Logger.logout(content = s"${Logger.HexStringWithWidth(dut.pipeline.onFetch.fetchOut.pc.toLong.toHexString,width = 8)}\t ${Logger.HexStringWithWidth(dut.pipeline.onFetch.fetchOut.inst.toLong.toHexString,8)}" ,project = "FetchSim",clear = true,showproject = true)
            }
            else {
              Logger.logout(content = s"${Logger.HexStringWithWidth(dut.pipeline.onFetch.fetchOut.pc.toLong.toHexString,width = 8)}\t ${Logger.HexStringWithWidth(dut.pipeline.onFetch.fetchOut.inst.toLong.toHexString,8)}")
            }

          }
          dut.pipeline.onFetch.fetchOut.pc.toLong.toHexString === "100005bc"
        }

    }
  }
}
