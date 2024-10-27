#include <verilated.h>
#include <nvboard.h>
#include <VBcd7seg.h>

void nvboard_bind_all_pins(VBcd7seg* top);

VerilatedContext* contextp = NULL;

static VBcd7seg* top = NULL;

void step_and_update() {
    top->eval();
    // printf("io_input: %b, io_output: %b\n", top->io_input, top->io_output);
    nvboard_update();
}

void sim_init() {
    contextp = new VerilatedContext;
    contextp->debug(0);

    top = new VBcd7seg{contextp};
    
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

