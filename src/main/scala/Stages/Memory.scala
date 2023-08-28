package Stages

import BCore.DecodeInfo._
import BCore.RiscvCoreConfig
import DefineSim.SpinalSim._
import spinal.core._
import spinal.lib._
//memory stage will pending the read cmd

case class MemoryOutput()(implicit p:RiscvCoreConfig) extends Bundle{
  val pc = UInt(p.pcWidth bits)
  val instruction = Bits(32 bits)
  val ctrl = InstructionCtrl()
  val result = Bits(32 bits)
  val unalignedMemoryAccessException = Bool()
  val regFileAddress = UInt(5 bits)
  val pcPlus4 = UInt(32 bits)
  val needMemRsp = Bool()
  val dCmdAddress = UInt(p.addressWidth bits)
}

class Memory(implicit p:RiscvCoreConfig) extends PrefixComponent{
  val io = new Bundle{
    val inInst = slave Stream (ExcuteOutPut()(p))
    val pcLoad = master Flow(UInt(p.pcWidth bits))
    val pendingHalt = out Bool()
    val dCmd = slave Stream CoreDataCmd()
  }

  val halt = False
  val throwIt = False
  val pc_sel = io.inInst.pc_sel

  //pending the cmd



}
