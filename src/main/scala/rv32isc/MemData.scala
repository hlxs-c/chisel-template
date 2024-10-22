package rv32isc

import chisel3._ 
import chisel3.util._ 

import config.Configs._ 
import utils.ALU_OP_TYPES._ 
import utils.LS_TYPE._ 
import utils._ 
import dataclass.data

class MemDataIO extends Bundle {
    val bundleMemDataControl = new BundleMemDataControl()
    val resultAlu = Input(UInt(DATA_WIDTH.W))
    val dataStore = Input(UInt(DATA_WIDTH.W))
    val result = Output(UInt(DATA_WIDTH.W))
}

class MemData extends Module {
    val io = IO(new MemDataIO())

    // data mem
    val mem = Mem(MEM_DATA_SIZE, UInt(DATA_WIDTH.W))

    // the wire for output
    val result = WireDefault(0.U(DATA_WIDTH.W))

    // the read data from mem
    val dataLoad = WireDefault(0.U(DATA_WIDTH.W))

    // io.resultAlu is the address for data_mem
    val addr = io.resultAlu >> DATA_BYTE_WIDTH_LOG.U

    dataLoad := mem.read(addr)

    // store
    when(io.bundleMemDataControl.ctrlStore) {
        when(io.bundleMemDataControl.ctrlLSType === LS_W) {
            // modify the data word, 4 bytes
            mem.write(addr, io.dataStore)
        } .elsewhen(io.bundleMemDataControl.ctrlLSType === LS_H) {
            // modify the low 2 bytes
            mem.write(addr, Cat(dataLoad(31, 16), io.dataStore(15, 0)))
        } .otherwise {
            // modify the lowest 1 byte
            mem.write(addr, Cat(dataLoad(31, 8), io.dataStore(7, 0)))
        }
    }

    // load
    when (io.bundleMemDataControl.ctrlLoad) {
        when(io.bundleMemDataControl.ctrlLSType === LS_W) {
            result := dataLoad
        }.elsewhen(io.bundleMemDataControl.ctrlLSType === LS_H) {
            when (io.bundleMemDataControl.ctrlSigned) {
                result := Cat(Fill(16, dataLoad(15)), dataLoad(15, 0))
            } .otherwise {
                result := Cat(Fill(16, 0.U), dataLoad(15, 0))
            }
        }.otherwise {
            when (io.bundleMemDataControl.ctrlSigned) {
                result := Cat(Fill(24, dataLoad(7)), dataLoad(7, 0))
            } .otherwise {
                result := Cat(Fill(24, 0.U), dataLoad(7, 0))
            }
        } 
    } .otherwise {
        result := io.resultAlu
    }

    // output
    io.result := result
}