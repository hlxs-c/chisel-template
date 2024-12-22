package exp6

import chisel3._
import chisel3.util._

class LinearFeedbackShiftReg extends Module {
    val io = IO(new Bundle {
        val dout = Output(UInt(8.W))
    })

    val shiftReg = RegInit(1.U)

    val newHighBit = WireDefault(0.U(1.W))

    when(shiftReg =/= 0.U) {
        newHighBit := shiftReg(4) ^ shiftReg(3) ^ shiftReg(2) ^ shiftReg(0)

        shiftReg := Cat(newHighBit, shiftReg(7, 1))
    } otherwise {
        shiftReg := 1.U
    }

    io.dout := shiftReg
}

object LinearFeedbackShiftReg extends App {
    emitVerilog(new LinearFeedbackShiftReg(), Array("--target-dir", "simulate/exp6/LFSR/vsrc"))
}