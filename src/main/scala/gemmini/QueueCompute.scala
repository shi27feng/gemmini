
package gemmini

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile.RoCCCommand

import GemminiISA._
import Util._


class QueueCompute (cmd_t: RoCCCommand, nEntries: Int, local_addr_t: LocalAddr, block_rows: Int, block_cols: Int) extends Module {
  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(log2Up(nEntries).W)))

    val issue = new Bundle {
      val st = new ROBIssue(cmd_t, nEntries)
      val ex = new ROBIssue(cmd_t, nEntries)
    }
    val busy = Output(Bool())

  })

  class Entry extends Bundle {

    val is_config = Bool()

    val op1 = UDValid(local_addr_t.cloneType)
    val op2 = UDValid(local_addr_t.cloneType)

    val dst = UDValid(new Bundle { //dest spad address
      val start = local_addr_t.cloneType
      def end = local_addr_t.cloneType
    })

    val counter = UInt(32.W)

    val needs_to_preload = Bool()
    val needs_to_store = Bool()

    val issued = Bool()

    val complete_on_issue = Bool() //can delete after issue

    val cmd = cmd_t.cloneType //macro command

    val deps = Vec(nEntries, Bool()) //other command dependent on
    def ready(dummy: Int = 0): Bool = !deps.reduce(_ || _) //no dependency, can run
  }

  val entries = Reg(Vec(nEntries, UDValid(new Entry)))
  //when do we use Module(class), Bundle (struct), Vec?

  val entry_that_is_ready = MuxCase(entries.last, entries.map(e => (e.bits.op1 >= counter) -> e))
  val entry_that_is_ready_valid = entries.map(e => e.bits.op1 >= counter).reduce(_ || _)

  val empty = !entries.map(_.valid).reduce(_ || _)
  val full = entries.map(_.valid).reduce(_ && _)

  io.issue.ex := DontCare
  io.issue.ex.valid := entry_that_is_ready_valid
  io.issue.ex.cmd.inst.funct := Mux(entry_that_is_ready.needs_to_preload, PRELOAD_CMD, COMPUTE_AND_FLIP_CMD)

  when (io.issue.ex.fire()) {
    entry_that_is_ready.bits.needs_to_preload := ~entry_that_is_ready.bits.needs_to_preload
    //counter
  }

  io.issue.st := DontCare
  io.issue.st.valid := entry_that_is_ready_valid && entry_that_is_ready.bits.needs_to_store && entry_that_is_ready.bits.counter == last_value
  io.issue.st.cmd.inst.funct := STORE_CMD
  io.issue.st.cmd.rs1 := dram_addr + entry_that_is_ready.counter

  /*
  issue execution command from whichever queue entry that is ready (address < spad_pointer)
  if there are multiple entries ready, then oldest one gets priority
  after all counter has increment -> issue store command -> retire
   */

}
