package rv32isc

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._ 
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import config.Configs._
import java.io.PrintWriter
import java.io.File

class MemInstTest extends AnyFlatSpec with Matchers {
    val inst_list = Seq.fill(MEM_INST_SIZE)(scala.util.Random.nextInt().toLong & 0x00ffffffffL)

    def genMemInstHex(): Unit = {
        val memFile = new File(
            System.getProperty("user.dir") + "/src/test/resources/MemInst.hex"
        )
        val memPrintWriter = new PrintWriter(memFile)
        for(i <- 0 to MEM_INST_SIZE - 1) {
            memPrintWriter.println(inst_list(i).toHexString)
        }
        memPrintWriter.close()
    }

    def testFn(dut: MemInst): Unit = {
        // read all inst and match inst_list
        for(i <- 0 to MEM_INST_SIZE - 1) {
            dut.io.addr.poke((i * INST_BYTE_WIDTH).U)
            dut.clock.step()
            // dut.io.inst.expect(inst_list(i).U)
        }
    }

    "MemInst" should "pass" in {
        // genMemInstHex()
        simulate(new MemInst()) {
            dut => testFn(dut)
        }
    }
}