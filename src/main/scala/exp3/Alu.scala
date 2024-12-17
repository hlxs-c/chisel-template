package exp3

import chisel3._
import chisel3.util._

class Alu extends Module {
    val io = IO(new Bundle {
        val a = Input(SInt(4.W))
        val b = Input(SInt(4.W))
        val op = Input(UInt(3.W))
        val res = Output(SInt(4.W))
    })

    val res = WireDefault(0.S(4.W))

    switch(io.op) {
        is(0.U) {
            res := io.a + io.b
        }
        is(1.U) {
            res := io.a - io.b
        }
        is(2.U) {
            res := ~io.a
        }
        is(3.U) {
            res := io.a & io.b
        }
        is(4.U) {
            res := io.a | io.b
        }
        is(5.U) {
            res := io.a ^ io.b
        }
        is(6.U) {
            when(io.a < io.b) {
                res := 1.S
            } otherwise {
                res := 0.S
            }
        }
        is(7.U) {
            when(io.a === io.b) {
                res := 1.S
            } otherwise {
                res := 0.S
            }
        }
    }

    io.res := res
}

object Alu extends App {
    emitVerilog(new Alu(), Array("--target-dir", "simulate/exp3/Alu_nvboard/vsrc"))
}