package exp0

import chisel3._

class DoubleControlSwitch extends Module {
    val io = IO(new Bundle{
        val a = Input(Bool())
        val b = Input(Bool())
        val f = Output(Bool())
    })

    io.f := io.a ^ io.b
}

object DoubleControlSwitch extends App {
    emitVerilog(new DoubleControlSwitch(), Array("--target-dir", "generated"))
}