#include <verilated.h>
#include <verilated_vcd_c.h>
#include <VEncode42.h>

VerilatedContext* contextp = NULL;
VerilatedVcdC* tfp = NULL;

static VEncode42* top = NULL;

void step_and_dump_wave() {
	top->eval();
	contextp->timeInc(1);
	tfp->dump(contextp->time());
}

void sim_init() {
	contextp = new VerilatedContext;
	tfp = new VerilatedVcdC;
	top = new VEncode42{contextp};
	contextp->traceEverOn(true);
	top->trace(tfp, 0);
	tfp->open("Encode42.vcd");
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

    top->io_en=0b0; 
    top->io_x =0b0000; step_and_dump_wave();
    top->io_x =0b0001; step_and_dump_wave();
    top->io_x =0b0010; step_and_dump_wave();
    top->io_x =0b0100; step_and_dump_wave();
    top->io_x =0b1000; step_and_dump_wave();
    
    top->io_en=0b1; 
    top->io_x =0b0000; step_and_dump_wave();
    top->io_x =0b0001; step_and_dump_wave();
    top->io_x =0b0010; step_and_dump_wave();
    top->io_x =0b0100; step_and_dump_wave();
    top->io_x =0b1000; step_and_dump_wave();
	
	sim_exit();

	return 0;
}

