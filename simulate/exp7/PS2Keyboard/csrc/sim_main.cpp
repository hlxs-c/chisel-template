#include <verilated.h>
#include <nvboard.h>
#include <VPS2Keyboard.h>

void nvboard_bind_all_pins(VPS2Keyboard* top);

VerilatedContext* contextp = NULL;

static VPS2Keyboard* top = NULL;

static int pre_out = 0;

void step_and_update() {
    top->clock = !top->clock;

    top->eval();

    if(top->io_out_valid) {
        top->io_out_ready = 1;
        if(top->io_out_bits != 0 && top->io_out_bits != pre_out){
            pre_out = top->io_out_bits;
            printf("code: [%#X]\n", top->io_out_bits);
        }
    }

    nvboard_update();
}

void sim_init() {
    contextp = new VerilatedContext;
    contextp->debug(0);

    top = new VPS2Keyboard{contextp};
    
    nvboard_bind_all_pins(top);
    nvboard_init();

    top->clock = 0;
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
