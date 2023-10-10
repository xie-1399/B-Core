package Build

import DefineMem.MemOperation
import DefineSim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.lib._

class ITCM(p:coreParameters) extends PrefixComponent{

  /* it's only readable*/
  val io = new Bundle{
    val ioRequest = in Bool()
    val addr = in UInt(log2Up(p.TCMDepth) bits)
    val enable = in Bool()
    val inst = master(Flow(Bits(p.instructionWidth bits)))
  }

  /* with a simple hex file init */
  val tcm = MemOperation(Bits(p.instructionWidth / 4 bits),p.TCMDepth,initHex = "src/test/resources/add.hex",hexoffset = 0x80000000l)
  io.inst.payload := tcm.readSync(io.addr + 3,enable = io.ioRequest && io.enable) ## tcm.readSync(io.addr + 2,enable = io.ioRequest && io.enable) ##
    tcm.readSync(io.addr + 1,enable = io.ioRequest && io.enable) ## tcm.readSync(io.addr,enable = io.ioRequest && io.enable)
  val requestOut = RegInit(False).clearWhen(!io.ioRequest || !io.enable)
  when(io.ioRequest && io.enable){
    requestOut := True
  }
  io.inst.valid := requestOut

}
