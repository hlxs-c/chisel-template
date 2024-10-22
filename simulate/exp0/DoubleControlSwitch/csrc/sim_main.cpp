#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <verilated.h>
#include <nvboard.h>
#include <VDoubleControlSwitch.h>

#define USED_NVBOARD_TEST

void nvboard_bind_all_pins(VDoubleControlSwitch* top);

double sc_time_stamp() { return 0; }

// normal test
void single_cycle(VDoubleControlSwitch* top) {
    int a = rand() & 1;
    int b = rand() & 1;
    top->io_a = a;
    top->io_b = b;

    top->eval();
    printf("a=%d, b=%d, f=%d\n", a, b, top->io_f);
    assert(top->io_f == (a ^ b));
}

// connect to nvboard to test
void single_cycle_nvboard(VDoubleControlSwitch* top){
    top->clock = 0;
    top->eval();
    top->clock = 1;
    top->eval();
    nvboard_update();
}

int main(int argc, char** argv) {
    if(false && argc && argv) {}
    
    VerilatedContext* contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);

    VDoubleControlSwitch* top = new VDoubleControlSwitch{contextp};

    #ifdef USED_NVBOARD_TEST
    nvboard_bind_all_pins(top);
    nvboard_init();
    #endif

    while(!contextp->gotFinish()) {
        // select test mode by USED_NVBOARD_TEST
        #ifdef USED_NVBOARD_TEST
        single_cycle_nvboard(top);
        #else
        single_cycle(top);
        #endif
    }
    
    delete top;
    delete contextp;
    return 0;
}