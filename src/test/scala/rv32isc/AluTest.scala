package rv32isc

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._ 
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import config.Configs._ 
import utils.ALU_OP_TYPES._ 

class AluTest extends AnyFlatSpec with Matchers {
    val OP_TYPE_LIST = Seq(
        OP_NOP,
        OP_ADD,
        OP_SUB,
        OP_AND,
        OP_OR,
        OP_XOR,
        OP_SLL,
        OP_SRL,
        OP_SRA,
        OP_EQ,
        OP_NEQ,
        OP_LT,
        OP_GE
    )

    val oprand_list = Seq.fill(10)((scala.util.Random.nextInt(100) + 1).toLong & 0x00ffffffffL)

    def alu(a: Long, b: Long, op: UInt, sign: Boolean): (Long, Boolean) = {
        op match {
            case OP_NOP => (0, false)
            case OP_ADD => (a + b, false)
            case OP_SUB => (a - b, false)
            case OP_AND => (a & b, false)
            case OP_OR => (a | b, false)
            case OP_XOR => (a ^ b, false)
            case OP_SLL => (a << (b & 0x000000001f), false)
            case OP_SRL => (a >>> (b & 0x000000001f), false)
            case OP_SRA => (a.toInt >> (b & 0x000000001f).toInt, false)
            case OP_EQ => (0, a == b)
            case OP_NEQ => (0, a != b)
            case OP_LT => {
                if(sign) {
                    (0, (a << 32) < (b << 32))
                } else {
                    (0, a < b)
                }
            }
            case OP_GE => {
                if(sign) {
                    (0, (a << 32) >= (b << 32))
                } else {
                    (0, a >= b)
                }
            }
            case _ => (0, false)
        }
    }

    def testFn(dut: Alu, a: Long, b: Long, op: UInt, sign: Boolean): Unit = {
        // normal test
        dut.io.bundleAluControl.ctrlBranch.poke(false.B)
        dut.io.bundleAluControl.ctrlALUSrc.poke(false.B)
        dut.io.bundleAluControl.ctrlJAL.poke(false.B)
        dut.io.bundleAluControl.ctrlOP.poke(op)
        dut.io.bundleAluControl.ctrlSigned.poke(sign)
        dut.io.pc.poke(0.U)
        dut.io.imm.poke(0.U)
        dut.io.dataRead1.poke(a.U)
        dut.io.dataRead2.poke(b.U)
        val (resultAlu, resultBranch) = alu(a, b, op, sign)
        dut.io.resultAlu.expect((resultAlu.toLong & 0x00ffffffffL).U)
        dut.io.resultBranch.expect(resultBranch.B)

        // JAL
        dut.io.bundleAluControl.ctrlALUSrc.poke(true.B)
        dut.io.bundleAluControl.ctrlJAL.poke(true.B)
        dut.io.pc.poke(a.U)
        dut.io.imm.poke(b.U)
        dut.io.dataRead1.poke(0.U)
        dut.io.dataRead2.poke(0.U)
        val (resultAluJAL, resultBranchJAL) = alu(a, b, op, sign)
        dut.io.resultAlu.expect((resultAluJAL.toLong & 0x00ffffffffL).U)
        dut.io.resultBranch.expect(resultBranchJAL.B)

        // IMM
        dut.io.bundleAluControl.ctrlALUSrc.poke(true.B)
        dut.io.bundleAluControl.ctrlJAL.poke(false.B)
        dut.io.pc.poke(0.U)
        dut.io.imm.poke(b.U)
        dut.io.dataRead1.poke(a.U)
        dut.io.dataRead2.poke(0.U)
        val (resultAluImm, resultBranchImm) = alu(a, b, op, sign)
        dut.io.resultAlu.expect((resultAluImm.toLong & 0x00ffffffffL).U)
        dut.io.resultBranch.expect(resultBranchImm.B)
    }

    def testFnMain(dut: Alu): Unit = {
        for(a <- oprand_list) {
            for(b <- oprand_list) {
                for(op <- OP_TYPE_LIST) {
                    testFn(dut, a, b, op, true)
                    testFn(dut, a, b, op, false)
                }
            }
        }
    }

    "Alu" should "pass" in {
        simulate(new Alu()) {
            dut => testFnMain(dut)
        }
    }
}
