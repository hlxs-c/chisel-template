#include <verilated.h>
#include <nvboard.h>
#include <VAlu.h>

void nvboard_bind_all_pins(VAlu* top);

VerilatedContext* contextp = NULL;

static VAlu* top = NULL;

void step_and_update() {
    top->eval();
    nvboard_update();
}

void sim_init() {
    contextp = new VerilatedContext;
    contextp->debug(0);

    top = new VAlu{contextp};

    nvboard_bind_all_pins(top);
    nvboard_init();
}

void sim_exit() {
    step_and_update();6
    top->final();
    delete top;
    delete contextp;
}

int main() {
    sim_init();

    while(!contextp->gotFinish()) {
        step_and_update();
    }

    sim_exit();
}

