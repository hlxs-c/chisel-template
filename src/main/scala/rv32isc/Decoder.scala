package rv32isc

import chisel3._ 
import chisel3.util._ 

import config.Configs._ 
import utils.ALU_OP_TYPES._ 
import utils.LS_TYPE._ 
import utils._

class DecoderIO extends Bundle {
    val inst = Input(UInt(INST_WIDTH.W))
    val bundleCtrl = new BundleControl()
    val bundleReg = new BundleReg()
    val imm = Output(UInt(DATA_WIDTH.W))
}

class Decoder extends Module {
    val io = new DecoderIO()

    // three register, source reg: rs1, rs2; target reg: rd
    io.bundleReg.rs1 := io.inst(19,15)
    io.bundleReg.rs2 := io.inst(24, 20)
    io.bundleReg.rd := io.inst(11, 7)

    // five types of immediate
    val imm_i = Cat(Fill(20, io.inst(31)), io.inst(31, 20))     // io.inst(31, 20) is imm[11:0], inst[31] is the sign of imm
    val imm_s = Cat(Fill(20, io.inst(31)), io.inst(31, 25), io.inst(11, 7))
    val imm_b = Cat(Fill(20, io.inst(31)), io.inst(7), io.inst(30, 25), io.inst(11, 8), 0.U(1.W))
    val imm_u = Cat(io.inst(31, 12), Fill(12, 0.U))
    val imm_j = Cat(Fill(12, io.inst(31)), io.inst(31), io.inst(19, 12), io.inst(20), io.inst(30, 21), Fill(1, 0.U))
    
    // used by shift
    val imm_shamt = Cat(Fill(27, 0.U), io.inst(24, 20))

    // the output of imm
    val imm = WireDefault(0.U(32.W))

    // control sign
    val ctrlJump = WireDefault(false.B)
    val ctrlBranch = WireDefault(false.B)
    val ctrlRegWrite = WireDefault(false.B)
    val ctrlLoad = WireDefault(false.B)
    val ctrlStore = WireDefault(false.B)
    val ctrlALUSrc = WireDefault(false.B)
    val ctrlJAL = WireDefault(false.B)
    val ctrlOP = WireDefault(0.U(ALU_OP_TYPES_WIDTH.W))
    val ctrlSigned = WireDefault(true.B)
    val ctrlLSType = WireDefault(LS_W)

    // switch opcode and generate control sign
    switch(io.inst(6, 2)) {
        // U Type: LUI, AUIPC
        is("b01101".U, "b00101".U) {
            ctrlALUSrc := true.B
            ctrlOP := OP_ADD
            imm := imm_u
        }
        // J: JAL
        is("b11011".U) {
            ctrlALUSrc := true.B
            ctrlJump := true.B 
            ctrlOP := OP_ADD
            ctrlJAL := true.B
            imm := imm_j
        }
        // I: JALR
        // I: LB, LH, LW, LBU, LHU
        // I: ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI
        is("b11001".U, "b00000".U, "b00100".U) {
            ctrlALUSrc := true.B 
            
            when(io.inst(6, 2) === "b11001".U) { // JALR
                ctrlJump := true.B 
                ctrlOP := OP_ADD
                imm := imm_i
            } .elsewhen(io.inst(6, 2) === "b00000".U) { // LOAD
                ctrlLoad := true.B
                ctrlOP := OP_ADD
                imm := imm_i
                when(io.inst(14, 12) === "b100".U || io.inst(14, 12) === "b101".U) {
                    ctrlSigned := false.B
                }
                when(io.inst(14, 12) === "b100".U || io.inst(14, 12) === "b000".U) {
                    ctrlLSType := LS_B
                }
                when(io.inst(14, 12) === "b001".U || io.inst(14, 12) === "b101".U) {
                    ctrlLSType := LS_H
                }
            } .elsewhen(io.inst(6, 2) === "b00100".U && (io.inst(14, 12) === "b001".U || io.inst(14, 12) === "b101".U)) { // AL
                imm := imm_shamt
                switch(Cat(io.inst(30), io.inst(14, 12))) {
                    is("b0001".U) { // SLLI
                        ctrlOP := OP_SLL
                    }
                    is("b0101".U) { // SRLI
                        ctrlOP := OP_SRL
                    }
                    is("b1101".U) { // SRAI
                        ctrlOP := OP_SRA
                    }
                }
            } .otherwise {
                imm := imm_i
                switch(io.inst(14, 12)) {
                    is("b000".U) { // ADDI
                        ctrlOP := OP_ADD
                    }
                    is("b010".U) { // SLTI
                        ctrlOP := OP_LT
                    }
                    is("b011".U) { // SLTIU
                        ctrlOP := OP_LT
                        ctrlSigned := false.B
                    }
                    is("b100".U) { // XORI
                        ctrlOP := OP_XOR
                    }
                    is("b110".U) { // ORI
                        ctrlOP := OP_OR
                    }
                    is("b111".U) { // ANDI
                       ctrlOP := OP_AND 
                    }
                }
            }
        }
        // B: BEQ, BNE, BLT, BGE, BLTU, BGEU
        is("b11000".U) {
            ctrlALUSrc := false.B
            ctrlBranch := true.B
            ctrlRegWrite := false.B
            imm := imm_b

            switch(io.inst(14, 12)) {
                // BEQ
                is("b000".U) {
                    ctrlOP := OP_EQ
                }
                // BNE
                is("b001".U) {
                    ctrlOP := OP_NEQ
                }
                // BLT
                is("b100".U) {
                    ctrlOP := OP_LT
                }
                // BGE
                is("b101".U) {
                    ctrlOP := OP_GE
                }
                // BLTU
                is("b110".U) {
                    ctrlOP := OP_LT
                    ctrlSigned := false.B
                }
                // BGEU
                is("b111".U) {
                    ctrlOP := OP_GE
                    ctrlSigned := false.B
                }
            }
        }
        // S: SB, SH, SW
        is("b01000".U) {
            ctrlALUSrc := true.B
            ctrlStore := true.B 
            ctrlRegWrite := false.B
            ctrlOP := OP_ADD
            imm := imm_s
            when(io.inst(14, 12) === "b000".U) {
                ctrlLSType := LS_B
            } .elsewhen(io.inst(14, 12) === "b001".U) {
                ctrlLSType := LS_H
            }
        }
        // R: ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
        is("b01100".U) {
            switch(io.inst(14, 12)) {
                // ADD, SUB
                is("b000".U) {
                    when(io.inst(30)) {
                        ctrlOP := OP_SUB
                    } .otherwise {
                        ctrlOP := OP_ADD
                    }
                }
                // SLL
                is("b001".U) {
                    ctrlOP := OP_SLL
                }
                // SLT
                is("b010".U) {
                    ctrlOP := OP_LT
                }
                // SLTU
                is("b011".U) {
                    ctrlOP := OP_LT
                    ctrlSigned := false.B
                }
                // XOR
                is("b100".U) {
                    ctrlOP := OP_XOR
                }
                // SRL, SRA
                is("b101".U) {
                    when(io.inst(30)) {
                        ctrlOP := OP_SRA
                    } .otherwise {
                        ctrlOP := OP_SRL
                    }
                }
                // OR
                is("b110".U) {
                    ctrlOP := OP_OR
                }
                // AND
                is("b111".U) {
                    ctrlOP := OP_AND
                }
            }
        }
    }

    // connect control signal and imm (wire) to output
    io.bundleCtrl.ctrlALUSrc := ctrlALUSrc
    io.bundleCtrl.ctrlBranch := ctrlBranch
    io.bundleCtrl.ctrlJAL := ctrlJAL
    io.bundleCtrl.ctrlJump := ctrlJump
    io.bundleCtrl.ctrlLoad := ctrlLoad
    io.bundleCtrl.ctrlOP := ctrlOP
    io.bundleCtrl.ctrlRegWrite := ctrlRegWrite
    io.bundleCtrl.ctrlSigned := ctrlSigned
    io.bundleCtrl.ctrlStore := ctrlStore
    io.bundleCtrl.ctrlLSType := ctrlLSType
    io.imm := imm
}
