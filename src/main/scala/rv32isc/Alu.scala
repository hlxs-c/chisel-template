package rv32isc

import chisel3._ 
import chisel3.util._ 

import utils._ 
import utils.ALU_OP_TYPES._ 
import config.Configs._ 

class AluIO extends Bundle {
    val bundleAluControl = new BundleAluControl()
    val dataRead1 = Input(UInt(DATA_WIDTH.W))
    val dataRead2 = Input(UInt(DATA_WIDTH_H.W))
    val imm = Input(UInt(DATA_WIDTH.W))
    val pc = Input(UInt(ADDR_WIDTH.W))
    val resultBranch = Output(Bool())
    val resultAlu = Output(UInt(DATA_WIDTH.W))
}

class Alu extends Module {
    val io = IO(new AluIO())

    // the wire for output resultBranch and resultAlu
    val resultBranch = WireDefault(false.B)
    val resultAlu = WireDefault(0.U(DATA_WIDTH.W))

    // the wire for oprand
    val oprand1 = WireDefault(0.U(DATA_WIDTH.W))
    val oprand2 = WireDefault(0.U(DATA_WIDTH.W))

    oprand1 := Mux(io.bundleAluControl.ctrlJAL, io.pc, io.dataRead1)
    oprand2 := Mux(io.bundleAluControl.ctrlALUSrc, io.imm, io.dataRead2)

    // select op with io.bundleAluControl.ctrlOP
    switch(io.bundleAluControl.ctrlOP) {
        is(OP_NOP) {
            resultAlu := 0.U 
            resultBranch := false.B
        }
        is(OP_ADD) {
            resultAlu := oprand1 +& oprand2
        }
        is(OP_SUB) {
            resultAlu := oprand1 -& oprand2
        }
        is(OP_AND) {
            resultAlu := oprand1 & oprand2
        }
        is(OP_OR) {
            resultAlu := oprand1 | oprand2
        }
        is(OP_XOR) {
            resultAlu := oprand1 ^ oprand2
        }
        is(OP_SLL) {
            resultAlu := oprand1 << oprand2(4, 0)
        }
        is(OP_SRL) {
            resultAlu := oprand1 >> oprand2(4, 0)
        }
        is(OP_SRA) {
            resultAlu := (oprand1.asSInt >> oprand2(4, 0)).asUInt 
        }
        is(OP_EQ) {
            resultBranch := (oprand1.asSInt) === (oprand2.asSInt)
            resultAlu := io.pc +& io.imm
        }
        is(OP_NEQ) {
            resultBranch := (oprand1.asSInt) =/= (oprand2.asSInt)
            resultAlu := io.pc +& io.imm
        }
        is(OP_LT) {
            when(io.bundleAluControl.ctrlBranch) {
                when(io.bundleAluControl.ctrlSigned) {
                    resultBranch := (oprand1.asSInt) < (oprand2.asSInt)
                } .otherwise {
                    resultBranch := oprand1 < oprand2
                }
                resultAlu := io.pc +& io.imm
            } .otherwise {
                when(io.bundleAluControl.ctrlSigned) {
                    resultAlu := (oprand1.asSInt) < (oprand2.asSInt)
                } .otherwise {
                    resultAlu := oprand1 < oprand2
                }
            }
        }
        is(OP_GE) {
            when(io.bundleAluControl.ctrlSigned) {
                resultBranch := oprand1.asSInt >= oprand2.asSInt
            } .otherwise {
                resultBranch := oprand1 >= oprand2
            }
            resultAlu := io.pc +& io.imm
        }
    }

    io.resultAlu := resultAlu
    io.resultBranch := resultBranch
}