package exp6

import chisel3._
import chisel3.util._

class BucketShiftReg extends Module {
    var io = IO(new Bundle {
        val din = Input(UInt(8.W))
        val shamt = Input(UInt(3.W))
        val LorR = Input(Bool())
        val AorL = Input(Bool())
        val dout = Output(UInt(8.W))
    })

    val AorL = Wire(UInt(1.W))
    AorL := Mux(io.AorL, io.din(7), 0.U)    // io.AorL=0: logit shift, io.AorL=1: artichimic shift, high bit complement symbol bit(din(7))

    // 在Chisel中，不允许对一个硬件值的单独某些位进行赋值，所以这里使用Vec来保存中间值
    val layer1 = Wire(Vec(8, UInt(1.W)))
    val layer2 = Wire(Vec(8, UInt(1.W)))
    val layer3 = Wire(Vec(8, UInt(1.W)))

    val layer1Cnt = Cat(io.LorR.asUInt, io.shamt(0))    // io.LorR=0: right shift, io.LorR=1: left shift
    val layer2Cnt = Cat(io.LorR.asUInt, io.shamt(1))
    val layer3Cnt = Cat(io.LorR.asUInt, io.shamt(2))

    // ------------layer1 start------------------
    layer1(0) := MuxLookup(layer1Cnt, 0.U)(Seq(
        0.U -> io.din(0),
        1.U -> io.din(1),
        2.U -> io.din(0),
        3.U -> 0.U
    ))

    for(i <- 1 until 7) {
        layer1(i) := MuxLookup(layer1Cnt, 0.U)(Seq(
            0.U -> io.din(i),
            1.U -> io.din(i+1),
            2.U -> io.din(i),
            3.U -> io.din(i-1)
        ))
    }

    layer1(7) := MuxLookup(layer1Cnt, 0.U)(Seq(
        0.U -> io.din(7),
        1.U -> AorL,
        2.U -> io.din(7),
        3.U -> io.din(6)
    ))
    // ------------layer1 end------------------

    // ------------layer2 start------------------
    for(i <- 0 to 1) {
        layer2(i) := MuxLookup(layer2Cnt, 0.U)(Seq(
            0.U -> layer1(i),
            1.U -> layer1(i+2),
            2.U -> layer1(i),
            3.U -> 0.U
        ))
    }

    for(i <- 2 until 6) {
        layer2(i) := MuxLookup(layer2Cnt, 0.U)(Seq(
            0.U -> layer1(i),
            1.U -> layer1(i+2),
            2.U -> layer1(i),
            3.U -> layer1(i-2)
        ))
    }

    for(i <- 6 to 7) {
        layer2(i) := MuxLookup(layer2Cnt, 0.U)(Seq(
            0.U -> layer1(i),
            1.U -> AorL,
            2.U -> layer1(i),
            3.U -> layer1(i-2)
        ))
    }
    // ------------layer2 end------------------

    // ------------layer3 start------------------
    for(i <- 0 to 3) {
        layer3(i) := MuxLookup(layer3Cnt, 0.U)(Seq(
            0.U -> layer2(i),
            1.U -> layer2(i+4),
            2.U -> layer2(i),
            3.U -> 0.U
        ))
    }
    for(i <- 4 to 7) {
        layer3(i) := MuxLookup(layer3Cnt, 0.U)(Seq(
            0.U -> layer2(i),
            1.U -> AorL,
            2.U -> layer2(i),
            3.U -> layer2(i-4)
        ))
    }
    // ------------layer3 end------------------

    io.dout := layer3.asUInt
}

object BucketShiftReg extends App {
    emitVerilog(new BucketShiftReg(), Array("--target-dir", "simulate/exp6/BucketShiftReg/vsrc"))
}