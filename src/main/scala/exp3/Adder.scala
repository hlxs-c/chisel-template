package exp3

import chisel3._ 
import chisel3.util._

class Adder extends Module {
    val io = IO(new Bundle {
        val sub = Input(Bool())
        val A = Input(SInt(4.W))
        val B = Input(SInt(4.W))
        val carry = Output(Bool())
        val zero = Output(Bool())
        val overflow = Output(Bool())
        val result = Output(SInt(4.W))
    })

    val serialadder = Module(new SerialAdder(4))
    val result = Wire(SInt(4.W))
    val oprand1 = Wire(UInt(4.W))
    val oprand2 = Wire(UInt(4.W))

    oprand1 := io.A.asUInt
    oprand2 := Mux(io.sub, (~io.B + 1.S).asUInt, io.B.asUInt)

    serialadder.io.a := oprand1
    serialadder.io.b := oprand2
    
    result := serialadder.io.output.asSInt

    io.zero := (result === 0.S)
    io.overflow := (io.A(3) === io.B(3)) && (result(3) =/= io.A(3)) 
    io.carry := serialadder.io.cout
    io.result := result
}

object Adder extends App {
    emitVerilog(new Adder(), Array("--target-dir", "simulate/exp3/Adder"))
}