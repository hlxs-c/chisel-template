#include <verilated.h>
#include <verilated_vcd_c.h>
#include <VSerialAdder.h>

VerilatedContext* contextp = NULL;
VerilatedVcdC* tfp = NULL;

static VSerialAdder* top = NULL;

void sim_init() {
    contextp = new VerilatedContext;
    contextp->traceEverOn(true);
    tfp = new VerilatedVcdC;
    top = new VSerialAdder{contextp};
    top->trace(tfp, 0);
    tfp->open("SerialAdder.vcd");
}

void step_and_dump() {
    top->eval();
    contextp->timeInc(1);
    tfp->dump(contextp->time());
    printf("%b + %b = %b, and cout = %b\n", top->io_a, top->io_b, top->io_output, top->io_cout);
}

void sim_exit() {
    step_and_dump();
    top->final();
    tfp->close();

    delete top;
    delete tfp;
    delete contextp;
}

int main() {
    sim_init();

    top->io_a = -3;
    top->io_b = 5;
    step_and_dump();

    top->io_a = 3;
    top->io_b = -5;
    step_and_dump();

    top->io_a = 3;
    top->io_b = 5;
    step_and_dump();

    top->io_a = -3;
    top->io_b = -5;
    step_and_dump();

    top->io_a = -3;
    top->io_b = -6;
    step_and_dump();

    sim_exit();
    return 0;
}