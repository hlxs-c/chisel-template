package exp2

import chisel3._
import chisel3.util._

// 共阴极数码管编码
class Bcd7seg extends Module {
    val io = IO(new Bundle {
        val input = Input(UInt(4.W))        // bcd encode
        val output = Output(UInt(7.W))
    })

    val out = WireDefault(0.U(7.W))

    switch(io.input) {
        is(0.U) {
            out := "b1000000".U
        }
        is(1.U) {
            out := "b1111001".U
        }
        is(2.U) {
            out := "b0100100".U
        }
        is(3.U) {
            out := "b0110000".U
        }
        is(4.U) {
            out := "b0011001".U
        }
        is(5.U) {
            out := "b0010010".U
        }
        is(6.U) {
            out := "b0000010".U
        }
        is(7.U) {
            out := "b1111000".U
        }
        is(8.U) {
            out := "b0000000".U
        }
        is(9.U) {
            out := "b0010000".U
        }
    }

    io.output := out
}

object Bcd7seg extends App {
    emitVerilog(new Bcd7seg(), Array("--target-dir", "simulate/exp2/Bcd7seg/"))
}