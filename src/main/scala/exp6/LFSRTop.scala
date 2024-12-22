package exp6

import chisel3._ 
import chisel3.util._ 

class LFSRTop extends Module {
    val io = IO(new Bundle {
        val seg1 = Output(UInt(7.W))
        val seg2 = Output(UInt(7.W))
    })

    // instantiate Module, tip: must wrapping it in Module()
    val lsfr = Module(new LinearFeedbackShiftReg())
    val bin2hexseg = Module(new HexSeg())
    
    // connect lsft out to bin2hexseg
    bin2hexseg.io.binary := lsfr.io.dout

    io.seg1 := bin2hexseg.io.seg1   // low 4 bit
    io.seg2 := bin2hexseg.io.seg2   // high 4 bit
}

object LFSRTop extends App {
    emitVerilog(new LFSRTop(), Array("--target-dir", "simulate/exp6/LFSR_LED/vsrc"))
}