package exp1

import chisel3._
import chisel3.util._

class MuxKeyInternal1(nrKey: Int, keyLen: Int, dataLen: Int, hasDefault: Boolean, lut: Seq[(UInt, UInt)]) extends Module {
    val io = IO(new Bundle {
        val key = Input(UInt(keyLen.W))
        val defaultOut = Input(UInt(dataLen.W))
        val out = Output(UInt(dataLen.W))
    })

    val hit = Wire(Bool())
    val lutOut = Wire(UInt(dataLen.W))

    lutOut := 0.U
    hit := false.B

    for(i <- lut.indices) {
        when(io.key === lut(i)._1) {
            lutOut := lut(i)._2
            hit := true.B
        }
    }

    io.out := Mux(hit, lutOut, io.defaultOut)
}

class MuxKey1(nrKey: Int, keyLen: Int, dataLen: Int, lut: Seq[(UInt, UInt)]) extends Module {
    val io = IO(new Bundle {
        val key = Input(UInt(keyLen.W))
        val out = Output(UInt(dataLen.W))
    })

    val internal = Module(new MuxKeyInternal1(nrKey, keyLen, dataLen, false, lut))

    internal.io.key := io.key
    internal.io.defaultOut := 0.U
    io.out := internal.io.out
}

class MuxKeyWithDefault1(nrKey: Int, keyLen: Int, dataLen: Int, lut: Seq[(UInt, UInt)]) extends Module {
    val io = IO(new Bundle {
        val key = Input(UInt(keyLen.W))
        val defaultOut = Input(UInt(dataLen.W))
        val out = Output(UInt(dataLen.W))
    })

    val internal = Module(new MuxKeyInternal1(nrKey, keyLen, dataLen, true, lut))

    internal.io.key := io.key
    internal.io.defaultOut := io.defaultOut

    io.out := internal.io.out
}

class Mux21a extends Module {
    val io = IO(new Bundle {
        val data1 = Input(UInt(4.W))
        val data2 = Input(UInt(4.W))
        val sel = Input(Bool())
        val out = Output(UInt(4.W))
    })

    val lut = Seq(
        (0.U(1.W), io.data1),
        (1.U(1.W), io.data2)
    )

    val muxkey = Module(new MuxKey1(2, 1, 4, lut))

    muxkey.io.key := io.sel

    io.out := muxkey.io.out
}

object Mux21a extends App {
    emitVerilog(new Mux21a(), Array("--target-dir", "simulate/exp1/Mux21_chisel/vsrc"))
}