package exp0

import chisel3._
import chisel3.util._

class FlowingLight extends Module {
    val io = IO(new Bundle {
        val led = Output(UInt(16.W))
    })

    val ledReg = RegInit(1.U(16.W))
    val countReg = RegInit(0.U(16.W))

    countReg := Mux(countReg === 1000.U, 0.U, countReg + 1.U)

    when(countReg === 0.U) {
        ledReg := Cat(ledReg(14, 0), ledReg(15))
    }

    io.led := ledReg
}

object FlowingLight extends App {
    emitVerilog(new FlowingLight(), Array("--target-dir", "generated"))
}