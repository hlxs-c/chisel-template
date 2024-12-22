#include <verilated.h>
#include <nvboard.h>
#include <VLinearFeedbackShiftReg.h>

void nvboard_bind_all_pins(VLinearFeedbackShiftReg* top);

VerilatedContext* contextp = NULL;

static VLinearFeedbackShiftReg* top = NULL;

static int preClk = 0;

void step_and_update() {
    top->eval();
    nvboard_update();

    if(top->clock != preClk) {
        
        printf("top->clock = %d, ", top->clock);
        if(preClk == 0 && top->clock == 1){
            printf("top->io_dout: [%b]\n", top->io_dout);
        } else {
            printf("\n");
        }
        preClk = top->clock;
    }
    
}

void sim_init() {
    contextp = new VerilatedContext;
    contextp->debug(0);

    top = new VLinearFeedbackShiftReg{contextp};
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

