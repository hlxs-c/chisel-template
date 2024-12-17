#include <verilated.h>
#include <verilated_vcd_c.h>
#include <VFA.h>

VerilatedContext* contextp = NULL;
VerilatedVcdC* tfp = NULL;

static VFA* top = NULL;

void step_and_dump_wave() {
    top->eval();
    contextp->timeInc(1);
    tfp->dump(contextp->time());
    printf("%d + %d + %d= %d, and c_out = %d\n", top->io_a, top->io_b, top->io_cin, top->io_s, top->io_cout);
}

void sim_init() {
    contextp = new VerilatedContext;
    tfp = new VerilatedVcdC;
    top = new VFA{contextp};
    contextp->traceEverOn(true);
    top->trace(tfp, 0);
    tfp->open("FA.vcd");
}

void sim_exit() {
    step_and_dump_wave();
    tfp->close();

    delete top;
    delete tfp;
    delete contextp;
}

int main() {
    sim_init();

    top->io_cin = 0;
    top->io_a = 0;
    top->io_b = 0;
    step_and_dump_wave();
    top->io_a = 0;
    top->io_b = 1;
    step_and_dump_wave();
    top->io_a = 1;
    top->io_b = 0;
    step_and_dump_wave();
    top->io_a = 1;
    top->io_b = 1;
    step_and_dump_wave();
    
    top->io_cin = 1;
    top->io_a = 0;
    top->io_b = 0;
    step_and_dump_wave();
    top->io_a = 0;
    top->io_b = 1;
    step_and_dump_wave();
    top->io_a = 1;
    top->io_b = 0;
    step_and_dump_wave();
    top->io_a = 1;
    top->io_b = 1;
    step_and_dump_wave();

    sim_exit();
}