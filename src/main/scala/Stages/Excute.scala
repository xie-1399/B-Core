package Stages

import BCore.DecodeInfo._
import BCore.{Alu, Compare, RiscvCoreConfig}
import DefineSim.SpinalSim.PrefixComponent
import spinal.core._
import spinal.lib._

//this stage will use the Alu to compute the results

//send the dataCmd request to the memory

case class CoreDataCmd()(implicit p:RiscvCoreConfig) extends Bundle with IMasterSlave {
  val wr = Bool()
  val address = UInt(p.addressWidth bits)
  val data = Bits(32 bits)
  val size = UInt(2 bits)  //control the data size

  override def asMaster() = {
    out(wr,address,data,size)
  }
}

case class ExcuteOutPut(implicit p:RiscvCoreConfig) extends Bundle{
  //Todo add Branch prediction
  val pc = UInt(p.pcWidth bits)
  val instruction = Bits(32 bits)
  val ctrl = InstructionCtrl()
  val br = new Bundle{val eq,ltx = Bool()} //branch type
  val src1 = Bits(32 bits)
  val result = Bits(32 bits)
  val adder = UInt(32 bits)
  val unalignedMemoryAccessException = Bool()
  val needMemRsp = Bool()
  val pc_sel = PC()  //pc jump type
  val dCmdAddress = UInt(p.addressWidth bits)
}


class Excute(implicit p:RiscvCoreConfig) extends PrefixComponent{
  val io = new Bundle{
    val inInst = slave Stream DecodeOutput()(p)   //get the decode output
    val outInst = master Stream ExcuteOutPut()(p)
    val Externalflush = out Bool()
    val Internalflush = in Bool()
    val pendingHalt = in Bool()
    val dCmd = master Stream CoreDataCmd()
  }

  val throwIt = False
  val halt = False
  val ctrl = io.inInst.ctrl
  val imm = IMM(io.inInst.instrcution)

  //branch calculation
  val br = new Area {
    val signed = BR.isSignComp(ctrl.br) //whether sign or unsign
    val compare = new Compare(32)
    compare.io.src0 := io.inInst.src0
    compare.io.src1 := io.inInst.src1
    compare.io.sign := signed
    val ltx = compare.io.ltx
    val eq = compare.io.eq

    val pc_sel = io.inInst.ctrl.br.mux[PC.C](//jump pc type
      default -> PC.INC,
      BR.NE -> Mux(!eq,PC.BRA,PC.INC),
      BR.EQ -> Mux(eq,PC.BRA,PC.INC),
      (BR.GE,BR.GEU) -> Mux(!ltx,PC.BRA,PC.INC),
      (BR.LT,BR.LTU) -> Mux(ltx,PC.BRA,PC.INC),
      BR.J -> PC.J,
      BR.JR -> PC.JR
    )
  }

  val alu = new Alu
  alu.io.func := ctrl.alu
  alu.io.doSub := io.inInst.doSub  //not Add
  alu.io.src0 := io.inInst.alu_op0
  alu.io.src1 := io.inInst.alu_op1

  //set output signal res
  io.outInst.arbitrationFrom(io.inInst.throwWhen(throwIt).haltWhen(halt))
  io.outInst.pc := io.inInst.pc
  io.outInst.instruction := io.inInst.instrcution
  io.outInst.ctrl := ctrl
  io.outInst.br.eq := br.eq
  io.outInst.br.ltx := br.ltx
  io.outInst.pc_sel := br.pc_sel
  io.outInst.src1 := io.inInst.src1
  io.outInst.result := alu.io.result
  io.outInst.adder := alu.io.adder
  io.outInst.needMemRsp := io.inInst.ctrl.memoryOption === MemoryOp.LOAD && io.inInst.ctrl.useMemory
  io.outInst.dCmdAddress := io.dCmd.address
  //how the unaligned access happen
  io.outInst.unalignedMemoryAccessException := io.inInst.ctrl.useMemory && io.inInst.ctrl.mask.mux(
    default -> False,
    Mask.H -> io.dCmd.address(0),
    Mask.W -> (io.dCmd.address(0) || io.dCmd.address(1)) //using ()
  )

  //send the cmd to the memory
  io.dCmd.valid := io.inInst.valid && io.inInst.ctrl.useMemory && !io.outInst.unalignedMemoryAccessException && !halt && !throwIt && io.outInst.ready
  io.dCmd.wr := io.inInst.ctrl.memoryOption === MemoryOp.STORE
  io.dCmd.address := alu.io.adder
  io.dCmd.data := io.inInst.src1
  io.dCmd.size := io.inInst.ctrl.mask.mux(
    default -> U(2),
    Mask.B -> U(0),
    Mask.H -> U(1)
  )

  //pending read cmd halt from Memory
  when(io.pendingHalt){
    halt := True
  }

  io.Externalflush := io.Internalflush
  when(io.Internalflush || io.Externalflush){
    throwIt := True
  }
}
