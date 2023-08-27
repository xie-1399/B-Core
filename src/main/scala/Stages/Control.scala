package Stages
import spinal.core._
import spinal.lib._
//the component control the pipeline flush signal and other signals

//Todo whether to flush the pipeline

class Control extends Component {
  val io = new Bundle{
    val flush = slave Flow (flushSignal())
  }
  
  io.flush.stageFlush := B"00000"
  when(io.flush.valid){
    switch(io.flush.stageNum){
      is(0){
        io.flush.stageFlush := B"00001"
      }
      is(1){
        io.flush.stageFlush := B"00010"
      }
      is(2){
        io.flush.stageFlush := B"00100"
      }
      is(3){
        io.flush.stageFlush := B"01000"
      }
      is(4){
        io.flush.stageFlush := B"10000"
      }
      default{
        io.flush.stageFlush := B"00000"
      }
    }

  }
}

case class flushSignal() extends Bundle with IMasterSlave {
  val stageNum = UInt(3 bits)
  val stageFlush = Bits(5 bits)
  override def asMaster(): Unit = {
    in(stageFlush)
    out(stageNum)
  }
}

case class flushPipe() extends Bundle with IMasterSlave{
  val ExternalFlush = Bool()
  val InternalFlush = Bool()

  override def asMaster(): Unit = {
    in(InternalFlush)
    out(ExternalFlush)
  }
}