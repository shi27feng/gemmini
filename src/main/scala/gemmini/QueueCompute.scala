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

    val issue = new ROBIssue(cmd_t, nEntries) //how to connect to ex + st?

    val busy = Output(Bool())

  })



}
