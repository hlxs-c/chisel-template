package rv32isc

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._ 
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import config.Configs._

class PCRegTest extends AnyFlatSpec with Matchers {
    // generate ten random address to test
    val target_list = Seq.fill(10)(scala.util.Random.nextInt().toLong & 0x00ffffffffL)

    def testFn(dut: PCReg): Unit = {
        // init
        dut.io.ctrlBranch.poke(false.B)
        dut.io.resultBranch.poke(false.B)
        dut.io.ctrlJump.poke(false.B)
        dut.io.addrTarget.poke(START_ADDR)
        dut.io.addrOut.expect(START_ADDR)

        var addr: Long = START_ADDR

        // normal case(not jump instr or branch instr)
        for(target <- target_list) {
            dut.io.addrTarget.poke(target.U)
            addr += ADDR_BYTE_WIDTH
            dut.clock.step()
            dut.io.addrOut.expect(addr.U)
        }

        println("normal case test passed")

        // test jump
        dut.io.ctrlJump.poke(true.B)
        for(target <- target_list) {
            dut.io.addrTarget.poke(target.U)
            dut.clock.step()
            dut.io.addrOut.expect(target.U)
            addr = target
        }
        dut.io.ctrlJump.poke(false.B)

        println("jump case test passed")

        // test branch, branch is fail
        dut.io.ctrlBranch.poke(true.B)
        for(target <- target_list) {
            dut.io.addrTarget.poke(target.U)
            addr += ADDR_BYTE_WIDTH
            dut.clock.step()
            dut.io.addrOut.expect(addr.U)
        }
        
        println("branch fail test passed")

        // test branch, branch is true
        dut.io.resultBranch.poke(true)
        for(target <- target_list) {
            dut.io.addrTarget.poke(target.U)
            dut.clock.step()
            dut.io.addrOut.expect(target.U)
            addr = target
        }

        println("branch success test passed")
    }

    "PCReg" should "pass" in {
        simulate(new PCReg()) {
            dut => testFn(dut)
        }
    }
}

