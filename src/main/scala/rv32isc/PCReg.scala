package rv32isc

import chisel3._
import chisel3.util._

import config.Configs._

// - start the IO of PCReg Module
class PCRegIO extends Bundle {
    val addrOut = Output(UInt(ADDR_WIDTH.W))    // the addr of instr
    val ctrlJump = Input(Bool())                // if cur_instr is jump instr
    val ctrlBranch = Input(Bool())              // if cur_instr is branch_instr
    val resultBranch = Input(Bool())            // the result of branch, true indicates the success branch
    val addrTarget = Input(UInt(ADDR_WIDTH.W))  // the target addr of jump_instr or branch_instr(success_branch)
}
// - end

// - start the PCReg Module
class PCReg extends Module {
    val io = IO(new PCRegIO())

    val pcReg = RegInit(UInt(ADDR_WIDTH.W), START_ADDR.U)   // the reg of pc

    when(io.ctrlJump || (io.ctrlBranch && io.resultBranch)) {
        // when cur_instr is jump_instr or when cur_instr is branch_instr and the result_branch is true,
        // then next value of the pc is io.addrTarget
        pcReg := io.addrTarget
    } .otherwise {
        // otherwise pc add itself with 4(ADDR_BYTE_WIDTH)
        pcReg := pcReg + ADDR_BYTE_WIDTH.U
    }

    io.addrOut := pcReg     // every clock output the cur_pc_value
}
// - end