package exp2

import chisel3._
import chisel3.util._

class Encode83p extends Module {
    val io = IO(new Bundle {
        val input = Input(UInt(8.W))
        val en = Input(Bool())
        val out = Output(UInt(3.W))
        val indicate = Output(Bool())
    })

    val out = WireDefault(0.U(3.W))
    
    when(io.en) {
        for(i <- 0 until 8) {
            when(io.input(i)) {
                out := i.U
            }
        }
    }.otherwise {
        out := 0.U
    }

    io.indicate := (io.input === 0.U)
    io.out := out
}

object Encode83p extends App {
    emitVerilog(new Encode83p(), Array("--target-dir", "simulate/exp2/Encode83p/vsrc"))
}

class Segtop extends Module {
    val io = IO(new Bundle {
        val input = Input(UInt(8.W))
        val en = Input(Bool())
        val seg = Output(UInt(7.W))
        val indicate = Output(Bool())
    })

    val encode83p = Module(new Encode83p())
    val bcd7seg = Module(new Bcd7seg())

    encode83p.io.input := io.input
    encode83p.io.en := io.en

    bcd7seg.io.input := Cat(0.U(1.W), encode83p.io.out)
    
    io.indicate := encode83p.io.indicate
    io.seg := bcd7seg.io.output
}

object Segtop extends App {
    emitVerilog(new Segtop(), Array("--target-dir", "simulate/exp2/SegEncode83p/vsrc"))
}