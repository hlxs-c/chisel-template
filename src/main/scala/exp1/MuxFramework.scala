package exp1

import chisel3._
import chisel3.util._

class Pair(keyLen: Int, dataLen: Int) extends Bundle {
    val key = UInt(keyLen.W)
    val data = UInt(dataLen.W)
}

class MuxKeyInternal(Nrkey: Int, keyLen: Int, dataLen: Int, hasDefault: Boolean) extends Module {
    val io = IO(new Bundle {
        val key = Input(UInt(keyLen.W))
        val defaultOut = Input(UInt(dataLen.W))
        val lut = Input(Vec(Nrkey, new Pair(keyLen, dataLen)))
        val out = Output(UInt(dataLen.W))
    })

    val hit = Wire(Bool())
    val lutOut = Wire(UInt(dataLen.W))

    lutOut := 0.U
    hit := false.B

    for(i <- 0 until Nrkey) {
        when(io.key === io.lut(i).key) {
            lutOut := io.lut(i).data
            hit := true.B
        }
    }

    io.out := Mux(hit, lutOut, io.defaultOut)
}

class MuxKey(Nrkey: Int, keyLen: Int, dataLen: Int) extends Module {
    val io = IO(new Bundle {
        val key = Input(UInt(keyLen.W))
        val lut = Input(Vec(Nrkey, new Pair(keyLen, dataLen)))
        val out = Output(UInt(dataLen.W))
    })

    val internal = Module(new MuxKeyInternal(Nrkey, keyLen, dataLen, false))
    internal.io.key := io.key
    internal.io.lut := io.lut
    internal.io.defaultOut := 0.U   // no default value
    io.out := internal.io.out
}

class MuxKeyWithDefault(Nrkey: Int, keyLen: Int, dataLen: Int) extends Module {
    val io = IO(new Bundle {
        val key = Input(UInt(keyLen.W))
        val defaultOut = Input(UInt(dataLen.W))
        val lut = Input(Vec(Nrkey, new Pair(keyLen, dataLen)))
        val out = Output(UInt(dataLen.W))
    })

    val internal = Module(new MuxKeyInternal(Nrkey, keyLen, dataLen, true))
    internal.io.key := io.key
    internal.io.lut := io.lut
    internal.io.defaultOut := io.defaultOut
    io.out := internal.io.out
}

class Mux21 extends Module {
    val io = IO(new Bundle {
        val data1 = Input(UInt(4.W))
        val data2 = Input(UInt(4.W))
        val sel = Input(Bool())
        val out = Output(UInt(4.W))
    })
    val muxkey = Module(new MuxKey(2, 1, 4))

    muxkey.io.key := io.sel

    // init lut(look-up table)
    val lut = Wire(Vec(2, new Pair(1, 4)))
    lut(0).key := 0.U
    lut(0).data := io.data1
    lut(1).key := 1.U
    lut(1).data := io.data2
    muxkey.io.lut := lut

    io.out := muxkey.io.out
}

object Mux21 extends App {
    emitVerilog(new Mux21(), Array("--target-dir", "simulate/exp1/Mux21_chisel/vsrc"))
}

class Mux41_2bit extends Module {
    val io = IO(new Bundle {
        val x0 = Input(UInt(2.W))
        val x1 = Input(UInt(2.W))
        val x2 = Input(UInt(2.W))
        val x3 = Input(UInt(2.W))
        val y = Input(UInt(2.W))
        val f = Output(UInt(2.W))
    })

    val muxkey = Module(new MuxKey(4, 2, 2))

    muxkey.io.key := io.y
    
    // init lut(look-up table)
    val lut = Wire(Vec(4, new Pair(2, 2)))
    lut(0).key := 0.U
    lut(0).data := io.x0
    lut(1).key := 1.U
    lut(1).data := io.x1
    lut(2).key := 2.U
    lut(2).data := io.x2
    lut(3).key := 3.U
    lut(3).data := io.x3
    muxkey.io.lut := lut

    io.f := muxkey.io.out
}

object Mux41_2bit extends App {
    emitVerilog(new Mux41_2bit(), Array("--target-dir", "simulate/exp1/Mux41_2bit_chisel/vsrc"))
}