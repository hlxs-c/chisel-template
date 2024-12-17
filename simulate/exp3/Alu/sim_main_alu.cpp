#include <verilated.h>
#include <verilated_vcd_c.h>
#include <VAlu.h>

VerilatedContext* contextp = NULL;
VerilatedVcdC* tfp = NULL;

static VAlu* top = NULL;

void sim_init() {
    contextp = new VerilatedContext;
    contextp->traceEverOn(true);
    tfp = new VerilatedVcdC;
    top = new VAlu{contextp};
    top->trace(tfp, 0);
    tfp->open("Alu.vcd");
}

void step_and_dump_wave() {
    top->eval();
    contextp->timeInc(1);
    tfp->dump(contextp->time());
    printf("a=%b, b=%b, op=%b, result=%b\n", top->io_a, top->io_b, top->io_op, top->io_res);
}

void sim_exit() {
    step_and_dump_wave();
    tfp->close();

    delete top;
    delete tfp;
    delete contextp;
}

int main() {
    sim_init();

    top->io_a = 3;
    top->io_b = -4;
    top->io_op = 0;
    step_and_dump_wave();   // +, -1 -> 1111

    top->io_a = 3;         
    top->io_b = -4;
    top->io_op = 1;
    step_and_dump_wave();   // -, 7 -> 0111

    top->io_a = 0;          // 0000
    top->io_b = 0;
    top->io_op = 2;
    step_and_dump_wave();   // ~, 1111

    top->io_a = 3;          // 0011
    top->io_b = -4;         // 1100 
    top->io_op = 3;
    step_and_dump_wave();   // &, 0000

    top->io_a = 3;          // 0011
    top->io_b = -4;         // 1100 
    top->io_op = 4;
    step_and_dump_wave();   // |, 1111

    top->io_a = 3;          // 0011
    top->io_b = -4;         // 1100 
    top->io_op = 5;
    step_and_dump_wave();   // ^, 1111

    top->io_a = -4;         
    top->io_b = -6;         
    top->io_op = 6;
    step_and_dump_wave();   

    top->io_a = 3;        
    top->io_b = 3;        
    top->io_op = 7;
    step_and_dump_wave();   // 3 = 3, 1

    sim_exit();

    return 0;
}