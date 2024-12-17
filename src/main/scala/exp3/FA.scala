package exp3

import chisel3._
import chisel3.util._

class FA extends Module {
    val io = IO(new Bundle {
        val a = Input(Bool())
        val b = Input(Bool())
        val cin = Input(Bool())
        val s = Output(Bool())
        val cout = Output(Bool())
    })

    io.s := io.a ^ io.b ^ io.cin
    io.cout := (io.a & io.b) + (io.a & io.cin) + (io.b & io.cin)
}

object FA extends App {
    emitVerilog(new FA(), Array("--target-dir", "simulate/exp3/Adder"))
}