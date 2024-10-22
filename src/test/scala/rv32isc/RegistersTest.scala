package rv32isc

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._ 
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import config.Configs._ 

class RegistersTest extends AnyFlatSpec with Matchers {
    // random data
    val oprand_list = Seq.fill(REG_NUMS)(scala.util.Random.nextInt().toLong & 0x00ffffffffL)

    def testFn(dut: Registers): Unit = {
        // init
        for(i <- 0 to REG_NUMS - 1) {
            dut.io.bundleReg.rs1.poke(i.U)
            dut.io.dataRead1.expect(0.U)
            dut.io.bundleReg.rs2.poke(i.U)
            dut.io.dataRead2.expect(0.U)
        }

        // write
        for(i <- 0 to REG_NUMS - 1) {
            dut.io.ctrlRegWrite.poke(true.B)
            dut.io.bundleReg.rd.poke(i.U)
            dut.io.dataWrite.poke(oprand_list(i))
            dut.clock.step()
        }

        // read
        for(i <- 0 to REG_NUMS - 1) {
            dut.io.bundleReg.rs1.poke(i.U)
            if(i == 0) {
                dut.io.dataRead1.expect(0.U)
            } else {
                dut.io.dataRead1.expect(oprand_list(i))
            }

            dut.io.bundleReg.rs2.poke(i.U)
            if(i == 0) {
                dut.io.dataRead2.expect(0.U)
            } else {
                dut.io.dataRead2.expect(oprand_list(i))
            }
        }
        
        // can't to write when ctrlRegWrite is false.B
        for(i <- 0 to REG_NUMS - 1) {
            dut.io.ctrlRegWrite.poke(false.B)
            dut.io.bundleReg.rd.poke(i.U)
            dut.io.dataWrite.poke(0.U)
            dut.clock.step()
        }

        // read again to test whether write is failed when ctrlRegWrite is false
        for(i <- 0 to REG_NUMS - 1) {
            dut.io.bundleReg.rs1.poke(i.U)
            if(i == 0) {
                dut.io.dataRead1.expect(0.U)
            } else {
                dut.io.dataRead1.expect(oprand_list(i))
            }

            dut.io.bundleReg.rs2.poke(i.U)
            if(i == 0) {
                dut.io.dataRead2.expect(0.U)
            } else {
                dut.io.dataRead2.expect(oprand_list(i))
            }
        }
    }

    "Registers" should "pass" in {
        simulate(new Registers()) {
            dut => testFn(dut)
        }
    }
}
