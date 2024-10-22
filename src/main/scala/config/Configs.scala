package config

import chisel3._
import chisel3.util._ 

object Configs {
    val ADDR_WIDTH = 32 // the width of address
    val ADDR_BYTE_WIDTH = ADDR_WIDTH / 8    // the width of address, indicates how many bytes of the width of address
    val DATA_WIDTH = 32 // the width of data
    val DATA_WIDTH_H = 16   // the width of half data word
    val DATA_WIDTH_B = 8    // the width of byte
    val START_ADDR: Long = 0x00000000   // the start address

    val INST_WIDTH = 32 //the width of instr
    val INST_BYTE_WIDTH = INST_WIDTH / 4
    val INST_BYTE_WIDTH_LOG = log2Ceil(INST_BYTE_WIDTH) 
    val MEM_INST_SIZE = 1024   // the size of inst_mem

    val DATA_BYTE_WIDTH = DATA_WIDTH / 8
    val DATA_BYTE_WIDTH_LOG = log2Ceil(DATA_BYTE_WIDTH)
    val MEM_DATA_SIZE = 1024    // the size of data_mem

    val REG_NUMS = 32   // the numbers of register
    val REG_NUMS_LOG = log2Ceil(REG_NUMS)   // the width of register index
}