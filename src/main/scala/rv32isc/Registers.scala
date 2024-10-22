package rv32isc

import chisel3._ 
import chisel3.util._ 

import config.Configs._ 
import utils._ 

class RegistersIO extends Bundle {
    val ctrlRegWrite = Input(Bool())
    val ctrlJump = Input(Bool())
    val pc = Input(UInt(ADDR_WIDTH.W))
    val dataWrite = Input(UInt(DATA_WIDTH.W))
    val bundleReg = Flipped(new BundleReg())
    val dataRead1 = Output(UInt(DATA_WIDTH.W))
    val dataRead2 = Output(UInt(DATA_WIDTH.W))
}

class Registers extends Module {
    val io = IO(new RegistersIO())

    // registers
    val regs = Reg(Vec(REG_NUMS, UInt(DATA_WIDTH.W)))

    // regs[0] always is 0.U
    when(io.bundleReg.rs1 === 0.U) {
        io.dataRead1 := 0.U
    }
    when(io.bundleReg.rs2 === 0.U) {
        io.dataRead2 := 0.U
    }

    // other case
    io.dataRead1 := regs(io.bundleReg.rs1)
    io.dataRead2 := regs(io.bundleReg.rs2)

    // write to reg, io.ctrlRegWrite is valid and target reg rd is valid
    when(io.ctrlRegWrite && io.bundleReg.rd =/= 0.U) {
        when(io.ctrlJump) {
            // if io.ctrlJump is true, then store the next inst pc value in the rd
            regs(io.bundleReg.rd) := io.pc + INST_BYTE_WIDTH.U
        } .otherwise {
            // otherwise, write the input data to the rd
            regs(io.bundleReg.rd) := io.dataWrite
        }
    }
}