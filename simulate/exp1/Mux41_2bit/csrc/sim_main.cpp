#include <verilated.h>
#include <nvboard.h>
#include <VMux41_2bit.h>

void nvboard_bind_all_pins(VMux41_2bit* top);

VerilatedContext* contextp = NULL;

static VMux41_2bit* top = NULL;

void step_and_update() {
    top->eval();
    nvboard_update();
}

void sim_init() {
    contextp = new VerilatedContext;
    contextp->debug(0);

    top = new VMux41_2bit{contextp};
    nvboard_bind_all_pins(top);
    nvboard_init();
}

void sim_exit() {
    step_and_update();
    top->final();
    delete top;
    delete contextp;
}

int main() {
    sim_init();
    
    top->x0 = 0b00;
    top->x1 = 0b01;
    top->x2 = 0b10;
    top->x3 = 0b11;

    while(!contextp->gotFinish()) {
        step_and_update();
    }

    sim_exit();
}