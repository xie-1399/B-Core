package CoreSim
import BCore.RiscvCoreConfig
import DefineSim.SpinalSim.PrefixComponent
import spinal.core._
import Stages._

class ExcuteSim(implicit p:RiscvCoreConfig) extends PrefixComponent{
  //decode and excute
  val decode = new Decode()(p)
  val excute = new Excute()(p)
  val regfile = new Regfile()

  regfile.io <> decode.io.regfileIO
  decode.io.decodeOutput >-> excute.io.inInst
  decode.io.Internalflush := excute.io.Externalflush
}
