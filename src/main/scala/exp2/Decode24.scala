package exp2

import chisel3._
import chisel3.util._

class Decode24a extends Module {
    val io = IO(new Bundle {
        val x = Input(UInt(2.W))
        val en = Input(Bool())
        val y = Output(UInt(4.W))
    })

    val y = WireDefault(0.U(4.W))

    when(io.en) {
        switch(io.x) {
            is(0.U) {
                y := 1.U
            }
            is(1.U) {
                y := 2.U
            }
            is(2.U) {
                y := 4.U
            }
            is(3.U) {
                y := 8.U
            }
        }
    } .otherwise {
        y := 0.U
    }

    io.y := y
}

object Decode24a extends App {
    emitVerilog(new Decode24a(), Array("--target-dir", "simulate/exp2/Decode24/"))
}

class Decode24b extends Module {
    val io = IO(new Bundle {
        val x = Input(UInt(2.W))
        val en = Input(Bool())
        val y = Output(UInt(4.W))
    })

    val y = WireDefault(0.U(4.W))

    when(io.en) {
        y := 1.U << io.x
    } .otherwise {
        y := 0.U
    }

    io.y := y
}

object Decode24b extends App {
    emitVerilog(new Decode24b(), Array("--target-dir", "simulate/exp2/Decode24/"))
}