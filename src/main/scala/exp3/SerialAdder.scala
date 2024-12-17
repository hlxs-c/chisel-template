package exp3

import chisel3._
import chisel3.util._ 

class SerialAdder(width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val output = Output(UInt(width.W))
        val cout = Output(Bool())
    })

    private val fas = Array.fill(width) {Module(new FA())}
    for(i <- 0 until width - 1) {
        fas(i+1).io.cin := fas(i).io.cout
        fas(i).io.a := io.a(i)
        fas(i).io.b := io.b(i)
    }
    fas(0).io.cin := 0.U
    fas(width-1).io.a := io.a(width-1)
    fas(width-1).io.b := io.b(width-1)

    io.cout := fas(width-1).io.cout
    
    val sum = Wire(Vec(width, Bool()))
    for(i <- 0 until width) {
        sum(i) := fas(i).io.s
    }
    io.output := sum.asUInt
}

object SerialAdder extends App {
    emitVerilog(new SerialAdder(4), Array("--target-dir", "simulate/exp3/Adder/"))
}
