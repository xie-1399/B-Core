package Build

import DefineSim.SpinalSim._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._


/*
  * use the pipeline lib to create five stages core
  * Todo fix the fetch cycles if you want to pass pc and inst to the decode

*/

case class coreParameters(resetValue:BigInt = 0x10000000l,
                          withRVC:Boolean = false,
                          TCMDepth:Int = 16384
                         ){
  def Xlen = 32
  def instructionWidth = 32
}

case class FetchCmd(p:coreParameters) extends Bundle{
  val pc = UInt(p.Xlen bits)
  val io = Bool()  /* whether is Io request */
}

case class FetchRsp(p:coreParameters) extends Bundle{
  val pc = UInt(p.Xlen bits)
  val instruction = Bits(p.instructionWidth bits)
}

case class FetchBus(p:coreParameters) extends Bundle with IMasterSlave {
  val fetchCmd = Stream(FetchCmd(p))
  val fetchRsp = Flow(FetchRsp(p))
  override def asMaster(): Unit = {
    master(fetchCmd)
    slave(fetchRsp)
  }
}

case class FetchOut(p:coreParameters) extends Bundle {
    val pc = UInt(p.Xlen bits)
    val inst = Bits(p.instructionWidth bits)
}

class pipelineCore(p:coreParameters) extends PrefixComponent{
  import p._
  val io = new Bundle {
    val haltCpu = in Bool()
  }



  val pipeline = new Pipeline {
    val fetch = new Stage().setName("Fetch")
    val decode = new Stage().setName("Decode")
    val excute = new Stage().setName("Excute")
    val memory = new Stage().setName("Memory")
    val writeback = new Stage().setName("WriteBack")

    val PC = Stageable(UInt(Xlen bits))
    val Inst = Stageable(Bits(instructionWidth bits))
    val halt = Stageable(Bool())

    import Connection._

    connect(fetch, decode)(M2S())
    connect(decode, excute)(M2S())
    connect(excute, memory)(M2S())
    connect(memory, writeback)(M2S())

    val onFetch = new Area {
      /* send the PC Value and get the instruction rsp*/
      import fetch._
      val reset = RegInit(False)
      reset := True
      val pc = Reg(UInt(Xlen bits)).init(resetValue)
      halt := io.haltCpu
      fetch.haltWhen(halt)

      val pcNext = if(withRVC) pc + 2 else pc + 4
      val inc = False

      pc := Mux(inc,pcNext,pc)

      val fetchRequest = FetchBus(p)
      fetchRequest.fetchCmd.valid := reset
      fetchRequest.fetchCmd.io := False
      fetchRequest.fetchCmd.pc := pc
      when(fetchRequest.fetchCmd.pc(31 downto 28) === 0x1){
        fetchRequest.fetchCmd.io := True
      }

      val itcm = new ITCM(p)
      itcm.io.ioRequest := fetchRequest.fetchCmd.io
      itcm.io.addr := fetchRequest.fetchCmd.pc.resized
      fetchRequest.fetchRsp.valid := itcm.io.inst.valid
      fetchRequest.fetchRsp.instruction := itcm.io.inst.payload
      fetchRequest.fetchRsp.pc := RegNext(fetchRequest.fetchCmd.pc)

      when(fetchRequest.fetchRsp.valid) {
        inc := True
      }
      val fetchOut = Stream(FetchOut(p))
      fetchOut.valid := fetchRequest.fetchRsp.fire && reset && !halt
      fetchOut.inst := fetchRequest.fetchRsp.instruction
      fetchOut.pc := fetchRequest.fetchRsp.pc

      when(halt) {
        fetchRequest.fetchCmd.valid := False
        inc := False
      }
      PC := pc
    }

    val onDecode = new Area {
      /* get the instruction and decode it using common module */
      import decode._
      val DecodeIn = Stream(FetchOut(p))
      onFetch.fetchOut >-> DecodeIn
      DecodeIn.ready := True
      decode.haltWhen(halt)
    }
  }

  pipeline.build()
}

object pipelineCore extends App{
  val rtl = new RtlConfig().GenRTL(top = new pipelineCore(coreParameters()))
}