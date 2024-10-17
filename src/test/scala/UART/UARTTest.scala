package UART

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._ 
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class CommSpec extends AnyFlatSpec {
    behavior of "Comm"
    it should "do something" in {
        simulate(new Comm(2000, 80)) { dut =>
            for(i <- 0 until 4000) {
                dut.clock.step(1)
            }
        }
    }
}