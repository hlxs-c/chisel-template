package exp6

import chisel3._
import chisel3.util._

class ShiftRegister extends  Module {
    val io = IO(new Bundle {
        val control = Input(UInt(3.W))
        val input = Input(SInt(1.W))
        val shiftRegSet = Input(SInt(8.W))
        val out = Output(SInt(8.W))
    })

    val shiftReg = Reg(SInt(8.W))
    
    
    switch(io.control) {
        is(0.U) {
            // 清零
            shiftReg := 0.S
        }
        is(1.U) {
            // 置数
            shiftReg := io.shiftRegSet
        }
        is(2.U) {
            // 逻辑右移，最高位补0,其余位为shiftReg(7, 1)
            shiftReg := Cat(0.S, shiftReg(7, 1)).asSInt
        }
        is(3.U) {
            // 逻辑左移
            shiftReg := shiftReg << 1
        }
        is(4.U) {
            // 算术右移
            shiftReg := shiftReg >> 1
        }
        is(5.U) {
            // 右移，且io.input作为新的最高位
            shiftReg := Cat(io.input, shiftReg(7,1)).asSInt
        }
        is(6.U) {
            // 循环右移，将右移移出的最低位shiftReg(0) 作为 新的最高位，其余位为 shiftReg(7, 1)
            shiftReg := Cat(shiftReg(0), shiftReg(7,1)).asSInt
        }
        is(7.U) {
            // 循环左移，将左移移出的最高位shiftReg(7) 作为 新的最低位，其余位为 shiftReg(6:0)
            shiftReg := Cat(shiftReg(6, 0), shiftReg(7)).asSInt
        }
    }

    io.out := shiftReg
}

object ShiftRegister extends App {
    emitVerilog(new ShiftRegister(), Array("--target-dir", "simulate/exp6/ShiftRegister/vsrc"))
}