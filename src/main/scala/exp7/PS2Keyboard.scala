package exp7

import chisel3._ 
import chisel3.util._ 

class PS2Keyboard extends Module {
    val io = IO(new Bundle {
        val ps2_clk = Input(UInt(1.W))
        val ps2_data = Input(UInt(1.W))
        val out = DecoupledIO(UInt(8.W))
    })

    val ps2_clk_sync =Reg(UInt(3.W))
    val buffer = Reg(Vec(10, UInt(1.W)))
    val sampling = Wire(Bool())
    val count = RegInit(0.U(4.W))

    ps2_clk_sync := Cat(ps2_clk_sync(1, 0), io.ps2_clk)
    sampling := ps2_clk_sync(2) & ~ps2_clk_sync(1)  // ps2_clk_sync(2)=1 and ps2_clk_sync(1)=0, which means neg edge

    when(sampling) {
        when(count < 10.U) {
            buffer(count) := io.ps2_data
            count := count + 1.U
        }.elsewhen(count === 10.U) {
            count := 0.U
        }
    }

    io.out.valid := count === 10.U
    io.out.bits := buffer.asUInt(8, 1)
}

object PS2Keyboard extends App {
    emitVerilog(new PS2Keyboard(), Array("--target-dir", "simulate/exp7/PS2Keyboard/vsrc"))
}