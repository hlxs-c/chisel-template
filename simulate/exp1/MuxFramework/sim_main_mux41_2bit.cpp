#include <verilated.h>
#include <verilated_vcd_c.h>
#include <VMux41_2bit.h>

VerilatedContext* contextp = NULL;
VerilatedVcdC* tfp = NULL;

static VMux41_2bit* top = NULL;

void step_and_dump_wave() {
    top->eval();
    contextp->timeInc(1);
    tfp->dump(contextp->time());
}

void sim_init() {
    contextp = new VerilatedContext;
    tfp = new VerilatedVcdC;
    top = new VMux41_2bit{contextp};
    contextp->traceEverOn(true);
    top->trace(tfp, 0);
    tfp->open("Mux41_2bit.vcd");
}

void sim_exit() {
    step_and_dump_wave();
    tfp->close();
    delete top;
    delete contextp;
    delete tfp;
}

int main() {
    sim_init();

    top->x0 = 0b00;
    top->x1 = 0b01;
    top->x2 = 0b10;
    top->x3 = 0b11;

    top->y = 0b00;
    step_and_dump_wave();
    top->y = 0b01;
    step_and_dump_wave();
    top->y = 0b10;
    step_and_dump_wave();
    top->y = 0b11;
    step_and_dump_wave();
    
    sim_exit();
}

