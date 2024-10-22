package rv32isc

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._ 
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import config.Configs._ 
import utils.ALU_OP_TYPES._ 
import utils.LS_TYPE._ 
import dataclass.data

class MemDataTest extends AnyFlatSpec with  Matchers {
    val data_list = Seq.fill(MEM_DATA_SIZE)(scala.util.Random.nextInt().toLong & 0x00ffffffffL)

    def testFn(dut: MemData): Unit = {
        // init
        dut.io.bundleMemDataControl.ctrlLoad.poke(false.B)
        dut.io.bundleMemDataControl.ctrlStore.poke(false.B)
        dut.io.bundleMemDataControl.ctrlLSType.poke(LS_W)
        dut.io.bundleMemDataControl.ctrlSigned.poke(false.B)
        dut.io.dataStore.poke(0.U(DATA_WIDTH.W))
        dut.io.dataStore.poke(0.U(DATA_WIDTH.W))

        // other inst
        for(i <- 0 to MEM_DATA_SIZE - 1) {
            dut.io.resultAlu.poke((i * DATA_BYTE_WIDTH).U)
            dut.clock.step()
            dut.io.result.expect((i * DATA_BYTE_WIDTH).U)
        }

        println("other inst passed")

        // SW inst
        dut.io.bundleMemDataControl.ctrlStore.poke(true.B)
        for(i <- 0 to MEM_DATA_SIZE - 1) {
            dut.io.dataStore.poke(data_list(i))
            dut.io.resultAlu.poke((i * DATA_BYTE_WIDTH).U)
            dut.clock.step()
            dut.io.result.expect((i * DATA_BYTE_WIDTH).U)
        }

        println("SW inst passed")

        // LW inst
        dut.io.bundleMemDataControl.ctrlStore.poke(false.B)
        dut.io.bundleMemDataControl.ctrlLoad.poke(true.B)
        for(i <- 0 to MEM_DATA_SIZE - 1) {
            dut.io.dataStore.poke(0.U)
            dut.io.resultAlu.poke((i * DATA_BYTE_WIDTH).U)
            dut.clock.step()
            dut.io.result.expect(data_list(i))
        }

        println("LW inst passed")

        // SH inst, let lower 2 byte to zero
        dut.io.bundleMemDataControl.ctrlStore.poke(true.B)
        dut.io.bundleMemDataControl.ctrlLoad.poke(false.B)
        dut.io.bundleMemDataControl.ctrlLSType.poke(LS_H)
        for(i <- 0 to MEM_DATA_SIZE - 1) {
            dut.io.dataStore.poke(0)
            dut.io.resultAlu.poke((i * DATA_BYTE_WIDTH).U)
            dut.clock.step()
            dut.io.result.expect((i * DATA_BYTE_WIDTH).U)
        }

        println("SH inst passed")

        // LHU read
        dut.io.bundleMemDataControl.ctrlStore.poke(false.B)
        dut.io.bundleMemDataControl.ctrlLoad.poke(true.B)
        for(i <- 0 to MEM_DATA_SIZE - 1) {
            dut.io.dataStore.poke(0.U)
            dut.io.resultAlu.poke((i * DATA_BYTE_WIDTH).U)
            dut.clock.step()
            dut.io.result.expect(0.U)
        }

        println("LHU inst passed")
        
        dut.io.bundleMemDataControl.ctrlSigned.poke(false.B)
        // SB inst
        dut.io.bundleMemDataControl.ctrlStore.poke(true.B)
        dut.io.bundleMemDataControl.ctrlLoad.poke(false.B)
        dut.io.bundleMemDataControl.ctrlLSType.poke(LS_B)
        for(i <- 0 to MEM_DATA_SIZE - 1) {
            dut.io.dataStore.poke(data_list(i))
            dut.io.resultAlu.poke((i * DATA_BYTE_WIDTH).U)
            dut.clock.step()
            dut.io.result.expect((i * DATA_BYTE_WIDTH).U)
        }

        println("SB inst passed")

        // LBU read
        dut.io.bundleMemDataControl.ctrlStore.poke(false.B)
        dut.io.bundleMemDataControl.ctrlLoad.poke(true.B)
        for(i <- 0 to MEM_DATA_SIZE - 1) {
            dut.io.dataStore.poke(0.U)
            dut.io.resultAlu.poke((i * DATA_BYTE_WIDTH).U)
            dut.clock.step()
            dut.io.result.expect((data_list(i).toLong & 0x00000000ffL).U)
        }

        println("LBU inst passed")

        // LB read
        dut.io.bundleMemDataControl.ctrlSigned.poke(true.B)
        for (i <- 0 to MEM_DATA_SIZE - 1) {
            dut.io.dataStore.poke(0.U)
            dut.io.resultAlu.poke((i * DATA_BYTE_WIDTH).U) 
            if ((data_list(i).toLong & 0x0000000080L) == 0) {
                dut.io.result.expect((data_list(i).toLong & 0x00000000ffL).U)
            } else {
                dut.io.result.expect(((data_list(i).toLong & 0x00000000ffL) | 0x00ffffff00L).U)
            }
        }

        println("LB inst passed")
    }

    "MemData" should "pass" in {
        simulate(new MemData()) {
            dut => testFn(dut)
        }
    }
}