#include <verilated.h>
#include <verilated_vcd_c.h>
#include <nvboard.h>
#include "VFlowingLight.h"

void nvboard_bind_all_pins(VFlowingLight* top);

double sc_time_stamp() { return 0; }

int main(int argc, char** argv) {

    VerilatedContext* contextp = new VerilatedContext;

    contextp->debug(0);
    contextp->randReset(0);
    contextp->traceEverOn(true);
    contextp->commandArgs(argc, argv);

    VerilatedVcdC* tfp = new VerilatedVcdC;

    VFlowingLight* top = new VFlowingLight{contextp};

    top->trace(tfp, 99);
    tfp->open("FlowingLight.vcd");
    
    top->reset = 0;
    top->clock = 0;

    nvboard_bind_all_pins(top);
    nvboard_init();

    while(!contextp->gotFinish()) {
        contextp->timeInc(100);
        
        top->clock = !top->clock;

        if(!top->clock) {
            if(contextp->time() > 1 && contextp->time() < 1000) {
                top->reset = 1;
            } else {
                top->reset = 0;
            }
        }

        top->eval();
        tfp->dump(contextp->time());
        nvboard_update();
    }

    top->final();
    tfp->close();

    delete top;
    delete contextp;
    delete tfp;

    return 0;
    
}