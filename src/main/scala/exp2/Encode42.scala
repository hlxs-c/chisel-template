package exp2

import chisel3._
import chisel3.util._

class Encode42 extends Module {
    val io = IO(new Bundle {
        val x = Input(UInt(4.W))
        val en = Input(Bool())
        val y = Output(UInt(2.W))
    })

    val y = WireDefault(0.U(2.W))

    when(io.en) {
        switch(io.x) {
            is(1.U) {
                y := 0.U
            }
            is(2.U) {
                y := 1.U
            }
            is(4.U) {
                y := 2.U
            }
            is(8.U) {
                y := 3.U
            }
        }
    } .otherwise {
        y := 0.U
    }

    io.y := y
}

object Encode42 extends App {
    emitVerilog(new Encode42(), Array("--target-dir", "simulate/exp2/Encode42"))
}