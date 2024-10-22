package rv32isc

import chisel3._
import chisel3.util._

import config.Configs._ 
import chisel3.util.experimental.loadMemoryFromFile
import chisel3.util.experimental.loadMemoryFromFileInline

class MemInstIO extends Bundle {
    val addr = Input(UInt(ADDR_WIDTH.W))
    val inst = Output(UInt(INST_WIDTH.W))
}

class MemInst extends Module {
    val io = IO(new MemInstIO())

    // the memory of instr, can store MEM_INST_SIZE instr
    val mem = Mem(MEM_INST_SIZE, UInt(INST_WIDTH.W))

    loadMemoryFromFileInline(mem, "src/test/resources/MemInst.hex")

    io.inst := mem.read(io.addr >> INST_BYTE_WIDTH_LOG.U)

    printf("io.inst: %x\n", io.inst)
}