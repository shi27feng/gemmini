package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._

import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters

import GemminiISA._
import Util._

//loop unroller along k dimension

class LoopUnroller(block_size: Int)(implicit p: Parameters) extends Module {
  val iterator_bitwidth = 10
  val pad_bitwidth = 5
  val GARBAGE_ADDR = ~0.U(32.W)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
  })

  object State extends ChiselEnum {
    val idle, preload, compute = Value
  }
  import State._

  val state = RegInit(idle)

  val cmd = Queue(io.in)

  //val i = RegInit(0.U(16.W))
  //val j = RegInit(0.U(16.W))
  val k = RegInit(0.U)

  //I, J, K
  val max_i = cmd.bits.rs2(iterator_bitwidth-1, 0)
  val max_j = cmd.bits.rs2(iterator_bitwidth * 2 - 1, iterator_bitwidth)
  val max_k = cmd.bits.rs2(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)

  //pad I, J, K
  val pad_i = cmd.bits.rs2(pad_bitwidth + iterator_bitwidth*3 , iterator_bitwidth*3 + 1)
  val pad_j = cmd.bits.rs2(pad_bitwidth*2 + iterator_bitwidth*3 , iterator_bitwidth*3 + 1 + pad_bitwidth)
  val pad_k = cmd.bits.rs2(pad_bitwidth*3 + iterator_bitwidth*3 , iterator_bitwidth*3 + 1 + pad_bitwidth*2)

  //val last_iteration = i === max_i - 1.U && j === max_j - 1.U && k === max_k - 1.U
  //val last_i = i === max_i - 1.U
  //val last_j = j === max_j - 1.U
  val last_k = k === max_k - 1.U

  //val bias = cmd.bits.rs2(iterator_bitwidth * 3)
  val transpose = cmd.bits.rs2(iterator_bitwidth * 3)

  val a_start = 0.U(32.W)//cmd.bits.rs1(31, 0)
  val b_start = cmd.bits.rs1(63, 32)
  val a_mvin = cmd.bits.rs1(31, 16)
  val b_mvin = cmd.bits.rs1(15, 0)
  val c_start = (3.U << 30).asUInt()
  val d_start = (1.U << 31).asUInt()

  // TODO get rid of the x * max_y multiplications here
  val a_addr = a_start + a_mvin * max_k + k * block_size.U
  //val b_addr = b_start + b_mvin + k * max_j * block_size.U //untransposed
  val b_addr = Mux(transpose, b_start + b_mvin * max_k + k * block_size.U, b_start + b_mvin + k * max_j * block_size.U)
  val c_addr = c_start + a_mvin * max_j + b_mvin//(i * max_j + j) * block_size.U
  //val d_addr = d_start + (i * max_j + j) * block_size.U

  val is_loop = cmd.bits.inst.funct === LOOP_WS

  val pre_addr = b_addr//Mux(i === 0.U, b_addr, GARBAGE_ADDR)
  val out_addr = c_addr //Mux(bias || k =/= 0.U, c_addr, d_addr)


  val A_cols = Mux(last_k, block_size.U - pad_k, block_size.U)
  val A_rows = pad_i//Mux(last_i, block_size.U - pad_i, block_size.U)
  val B_cols = pad_j//Mux(last_j, block_size.U - pad_j, block_size.U)
  val B_rows = A_cols
  val C_cols = B_cols
  val C_rows = A_rows

  //gemmini_extended_preload(BD, C, BD_cols, BD_rows, C_cols, C_rows) \
  //  ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, ((uint64_t)(BD_rows) << (ADDR_LEN + 16)) | ((uint64_t)(BD_cols) << ADDR_LEN) | (uint64_t)(BD), ((uint64_t)(C_rows) << (ADDR_LEN + 16)) | ((uint64_t)(C_cols) << ADDR_LEN) | (uint64_t)(C), k_PRELOAD)
  val preload_cmd = Wire(new RoCCCommand)
  preload_cmd := DontCare
  preload_cmd.inst.funct := PRELOAD_CMD
  preload_cmd.rs1 := ((B_rows << 48.U).asUInt() | (B_cols << 32.U).asUInt() | pre_addr)
  /*
  when(transpose){
    preload_cmd.rs1 := ((B_cols << 48.U).asUInt() | (B_rows << 32.U).asUInt() | pre_addr)
  }
   */
  preload_cmd.rs2 := ((C_rows << 48.U).asUInt() | (C_cols << 32.U).asUInt() | out_addr)

  //gemmini_extended_compute_preloaded(A, BD, A_cols, A_rows, BD_cols, BD_rows) \
  //  ROCC_INSTRUCTION_RS1_RS2(XCUSTOM_ACC, ((uint64_t)(A_rows) << (ADDR_LEN + 16)) | ((uint64_t)(A_cols) << ADDR_LEN) | (uint64_t)(A), ((uint64_t)(BD_rows) << (ADDR_LEN + 16)) | ((uint64_t)(BD_cols) << ADDR_LEN) | (uint64_t)(BD), k_COMPUTE_PRELOADED)
  val compute_cmd = Wire(new RoCCCommand())
  compute_cmd := DontCare
  compute_cmd.inst.funct := COMPUTE_AND_FLIP_CMD//Mux(i === 0.U, COMPUTE_AND_FLIP_CMD, COMPUTE_AND_STAY_CMD)
  compute_cmd.rs1 := ((A_rows << 48.U).asUInt() | (A_cols << 32.U).asUInt() | a_addr)
  compute_cmd.rs2 := ((block_size.U << 48.U).asUInt() | (block_size.U << 32.U).asUInt() |  GARBAGE_ADDR.asUInt())

  cmd.ready := false.B
  io.out.valid := cmd.valid
  io.out.bits := Mux(is_loop, Mux(state === compute, compute_cmd, preload_cmd), cmd.bits)

  def increment(): Unit = {
    val next_k = wrappingAdd(k, 1.U, max_k)
    //val next_i = wrappingAdd(i, 1.U, max_i)
    //val next_k = Mux(i === max_i - 1.U, wrappingAdd(k, 1.U, max_k), k)
    //al next_j = Mux(k === max_k - 1.U && i === max_i - 1.U, wrappingAdd(j, 1.U, max_j), j)

    //i := next_i
    k := next_k
    //j := next_j
  }

  when (cmd.valid) {
    when (is_loop && (state === idle || state === preload)) {
      when (io.out.fire()) {
        state := compute
      }
    }.elsewhen(is_loop && state === compute) {
      when (io.out.fire()) {
        increment()
        //state := Mux(last_iteration, idle, preload)
        state := Mux(last_k, idle, preload)
        cmd.ready := last_k//last_iteration
      }
    }.otherwise {
      cmd.ready := io.out.ready
    }
  }
}

object LoopUnroller {
  def apply(enq: ReadyValidIO[RoCCCommand], block_size: Int)(implicit p: Parameters): DecoupledIO[RoCCCommand] = {
    val lu = Module(new LoopUnroller(block_size))
    lu.io.in <> enq
    lu.io.out
  }
}
