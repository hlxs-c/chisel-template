package exp2

import chisel3._ 
import chisel3.util._ 

class Encode42p extends Module {
    val io = IO(new Bundle{
        val x = Input(UInt(4.W))
        val en = Input(Bool())
        val y = Output(UInt(2.W))
    })

    val y = WireDefault(0.U(2.W))

    when(io.en) {
        for(i <- 0 until 4) {
            when(io.x(i)) {
                y := i.U
            }
        }
    } .otherwise {
        y := 0.U
    }

    io.y := y
}

object Encode42p extends App {
    emitVerilog(new Encode42p(), Array("--target-dir", "simulate/exp2/Encode42p/"))
}