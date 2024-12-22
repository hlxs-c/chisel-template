package exp6

import chisel3._ 
import chisel3.util._ 

// 十六进制显示在数码管
class Encoder extends Module {
    val io = IO(new Bundle {
        val hex = Input(UInt(4.W))
        val seg = Output(UInt(7.W))
    })

    io.seg := MuxLookup(io.hex, 0.U)(Seq(
        0.U -> "b1000000".U,
        1.U -> "b1111001".U,
        2.U -> "b0100100".U,
        3.U -> "b0110000".U,
        4.U -> "b0011001".U,
        5.U -> "b0010010".U,
        6.U -> "b0000010".U,
        7.U -> "b1111000".U,
        8.U -> "b0000000".U,
        9.U -> "b0010000".U,
        10.U -> "b0001000".U,
        11.U -> "b0000011".U,
        12.U -> "b1000110".U,
        13.U -> "b0100001".U,
        14.U -> "b0000110".U,
        15.U -> "b0001110".U
    ))
}

class HexSeg extends Module {
    val io = IO(new Bundle {
        val binary = Input(UInt(8.W))
        val seg1 = Output(UInt(7.W))
        val seg2 = Output(UInt(7.W))    
    })

    // instantiate Module, tip: must wrappint it in Module()
    val enc1 = Module(new Encoder())    // low 4 bit
    val enc2 = Module(new Encoder())    // high 4 bit

    enc1.io.hex := io.binary(3, 0)
    enc2.io.hex := io.binary(7, 4)

    io.seg1 := enc1.io.seg
    io.seg2 := enc2.io.seg
}