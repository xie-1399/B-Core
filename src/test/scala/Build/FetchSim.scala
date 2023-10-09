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

  test("fetch from the itcm") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new pipelineCore(coreParameters())
      dut
    }.doSim {
      dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.haltCpu #= false
        dut.clockDomain.waitSampling(200)
        dut.io.haltCpu #= true
        dut.clockDomain.waitSampling(10)
    }
  }
}
