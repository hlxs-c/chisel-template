package fifo

import chisel3._
import chisel3.util._

// -start fifo_io
class FifoIO[T <: Data](private val gen: T) extends Bundle {
    val enq = Flipped(new DecoupledIO(gen))
    val deq = new DecoupledIO(gen)
}
// -end

// -start fifo_abstract
abstract class Fifo[T <: Data](gen: T, val depth: Int) extends Module {
    val io = IO(new FifoIO(gen))

    assert(depth > 0, "Number of buffer elements needs to be larger than 0")
}
// -end

// -start fifo_bubble
class BubbleFifo[T <: Data](gen: T, depth: Int) extends Fifo(gen: T, depth: Int) {

    private class Buffer() extends Module {
        val io = IO(new FifoIO(gen))

        val fullReg = RegInit(false.B)
        val dataReg = Reg(gen)

        when(fullReg) {
            when(io.deq.ready) {
                fullReg := false.B
            }
        }.otherwise {
            when(io.enq.valid) {
                dataReg := io.enq.bits
                fullReg := true.B
            }
        }

        io.enq.ready := !fullReg
        io.deq.valid := fullReg
        io.deq.bits := dataReg
    }

    private val buffers = Array.fill(depth) {Module(new Buffer())}
    for(i <- 0 until depth - 1) {
        buffers(i+1).io.enq <> buffers(i).io.deq
    }

    io.enq <> buffers(0).io.enq
    io.deq <> buffers(depth - 1).io.deq
}
// -end

// -start fifo_double_buffer
class DoubleBufferFifo[T <: Data](gen: T, depth: Int) extends Fifo(gen: T, depth: Int) {

    private class DoubleBuffer[T <: Data](gen: T) extends Module {
        val io = IO(new FifoIO(gen))

        object State extends ChiselEnum {
            val empty, one, two = Value
        }
        import State._

        val stateReg = RegInit(empty)
        val dataReg = Reg(gen)
        val shadowReg = Reg(gen)

        switch(stateReg) {
            is(empty) {
                when(io.enq.valid) {
                    dataReg := io.enq.bits
                    stateReg := one
                }
            }
            is(one) {
                when(io.deq.ready && !io.enq.valid) {
                    stateReg := empty
                }
                when(io.deq.ready && io.enq.valid) {
                    stateReg := one
                    dataReg := io.enq.bits
                }
                when(!io.deq.ready && io.enq.valid) {
                    stateReg := two
                    shadowReg := io.enq.bits
                }
            }
            is(two) {
                when(io.deq.ready) {
                    stateReg := one
                    dataReg := shadowReg
                }
            }
        }

        io.enq.ready := (stateReg === empty || stateReg === one)
        io.deq.valid := (stateReg === one || stateReg === two)
        io.deq.bits := dataReg
    }

    private val buffers = Array.fill((depth + 1) / 2) { Module(new DoubleBuffer(gen)) }
    for(i <- 0 until (depth + 1) / 2 - 1) {
        buffers(i+1).io.enq <> buffers(i).io.deq
    }

    io.enq <> buffers(0).io.enq
    io.deq <> buffers((depth + 1) / 2 - 1).io.deq
}
// -end

//- start fifo_reg_mem
class RegFifo[T <: Data](gen: T, depth: Int) extends Fifo(gen: T, depth: Int) {

  def counter(depth: Int, incr: Bool): (UInt, UInt) = {
    val cntReg = RegInit(0.U(log2Ceil(depth).W))
    val nextVal = Mux(cntReg === (depth - 1).U, 0.U, cntReg + 1.U)
    when(incr) {
      cntReg := nextVal
    }
    (cntReg, nextVal)
  }

  // the register based memory
  val memReg = Reg(Vec(depth, gen))

  val incrRead = WireDefault(false.B)
  val incrWrite = WireDefault(false.B)
  val (readPtr, nextRead) = counter(depth, incrRead)
  val (writePtr, nextWrite) = counter(depth, incrWrite)

  val emptyReg = RegInit(true.B)
  val fullReg = RegInit(false.B)

  val op = io.enq.valid ## io.deq.ready
  val doWrite = WireDefault(false.B)

  switch(op) {
    is("b00".U) {}
    is("b01".U) { // read
      when(!emptyReg) {
        fullReg := false.B
        emptyReg := nextRead === writePtr
        incrRead := true.B
      }
    }
    is("b10".U) { // write
      when(!fullReg) {
        doWrite := true.B
        emptyReg := false.B
        fullReg := nextWrite === readPtr
        incrWrite := true.B
      }
    }
    is("b11".U) { // write and read
      when(!fullReg) {
        doWrite := true.B
        emptyReg := false.B
        when(emptyReg) {
          fullReg := false.B
        }.otherwise {
          fullReg := nextWrite === nextRead
        }
        incrWrite := true.B
      }
      when(!emptyReg) {
        fullReg := false.B
        when(fullReg) {
          emptyReg := false.B
        }.otherwise {
          emptyReg := nextRead === nextWrite
        }
        incrRead := true.B
      }
    }
  }

  when(doWrite) {
    memReg(writePtr) := io.enq.bits
  }

  io.deq.bits := memReg(readPtr)
  io.enq.ready := !fullReg
  io.deq.valid := !emptyReg
}
//- end

// -start fifo_mem
class MemFifo[T <: Data](gen: T, depth: Int) extends Fifo(gen: T, depth: Int) {

    def counter(depth: Int, incr: Bool): (UInt, UInt) = {
        val cntReg = RegInit(0.U(log2Ceil(depth).W))
        val nextVal = Mux(cntReg === (depth - 1).U, 0.U, cntReg + 1.U)
        when(incr) {
            cntReg := nextVal
        }
        (cntReg, nextVal)
    }

    val mem = SyncReadMem(depth, gen, SyncReadMem.WriteFirst)

    val incrRead = WireDefault(false.B)
    val incrWrite = WireDefault(false.B)
    val (readPtr, nextRead) = counter(depth, incrRead)
    val (writePtr, nextWrite) = counter(depth, incrWrite)

    val emptyReg = RegInit(true.B)
    val fullReg = RegInit(false.B)

    val outputReg = Reg(gen)                // when readCond is true, to store the data of read
    val outputValidReg = RegInit(false.B)   // indicates if data of read is valid
    val read = WireDefault(false.B)         

    io.deq.valid := outputValidReg          // if data of read is valid, then io.deq.valid is true
    io.enq.ready := !fullReg                // only the fifo is not full, io.enq.ready is true

    val doWrite = WireDefault(false.B)
    val data = Wire(gen)
    data := mem.read(readPtr)
    io.deq.bits := data
    when(doWrite) {
        mem.write(writePtr, io.enq.bits)
    }

    // should add optimization when downstream is ready for pipielining
    val readCond = !outputValidReg && ((readPtr =/= writePtr) || fullReg)
    when(readCond) {
        read := true.B
        incrRead := true.B
        outputReg := data
        outputValidReg := true.B
        emptyReg := nextRead === writePtr
        fullReg := false.B  // no concurrent read when full(at the monent)
    }

    // fire Indicates if IO is both ready and valid, file = ready && valid
    when(io.deq.fire) {
        outputValidReg := false.B
    }
    io.deq.bits := outputReg

    when(io.enq.fire) {
        emptyReg := false.B
        fullReg := (nextWrite === readPtr) & ! read
        incrWrite := true.B
        doWrite := true.B
    }
}
// -end

// -start fifo_comb
class CombFifo[T <: Data](gen: T, depth: Int) extends Fifo(gen: T, depth: Int) {
    val memFifo = Module(new MemFifo(gen, depth))
    val bufferFifo = Module(new DoubleBufferFifo(gen, 2))

    io.enq <> memFifo.io.enq
    io.deq <> memFifo.io.deq
    bufferFifo.io.deq <> io.deq
}