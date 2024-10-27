#include <verilated.h>
#include <nvboard.h>
#include <VEncode83p.h>

void nvboard_bind_all_pins(VEncode83p* top);

VerilatedContext* contextp = NULL;

static VEncode83p* top = NULL;

void step_and_update() {
    top->eval();
    nvboard_update();
}

void sim_init() {
    contextp = new VerilatedContext;
    contextp->debug(0);

    top = new VEncode83p{contextp};
    
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

    while(!contextp->gotFinish()) {
        step_and_update();
    }

    sim_exit();
}

