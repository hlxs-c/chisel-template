#include <verilated.h>
#include <nvboard.h>
#include <VShiftRegister.h>

void nvboard_bind_all_pins(VShiftRegister* top);

VerilatedContext* contextp = NULL;

static VShiftRegister* top = NULL;

static int pre_out = 0;

void step_and_update() {
    // 注意：使用nvboard模拟测试时序电路时，必须模拟时钟，否则无效
    top->clock = !top->clock;
    top->eval();
    nvboard_update();

    if(top->io_out != pre_out){
        pre_out = top->io_out;
        printf("top->io_input: [%b], top->io_out: [%b]\n", top->io_input, top->io_out);
    }
    
}

void sim_init() {
    contextp = new VerilatedContext;
    contextp->debug(0);

    top = new VShiftRegister{contextp};

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