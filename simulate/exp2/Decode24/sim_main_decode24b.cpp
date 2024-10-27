#include <verilated.h>
#include <verilated_vcd_c.h>
#include <VDecode24b.h>

VerilatedContext* contextp = NULL;
VerilatedVcdC* tfp = NULL;

static VDecode24b* top = NULL;

void step_and_dump_wave() {
	top->eval();
	contextp->timeInc(1);
	tfp->dump(contextp->time());
}

void sim_init() {
	contextp = new VerilatedContext;
	tfp = new VerilatedVcdC;
	top = new VDecode24b{contextp};
	contextp->traceEverOn(true);
	top->trace(tfp, 0);
	tfp->open("Decode24b.vcd");
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

    top->io_en = 0b0;  
    top->io_x = 0b00;  
    step_and_dump_wave();
    top->io_x = 0b01; 
    step_and_dump_wave();
    top->io_x = 0b10;  
    step_and_dump_wave();
    top->io_x = 0b11; 
    step_and_dump_wave();
    
    
    top->io_en = 0b1; 
    top->io_x = 0b00; 
    step_and_dump_wave();
    top->io_x = 0b01; 
    step_and_dump_wave();
    top->io_x = 0b10;
    step_and_dump_wave();
    top->io_x = 0b11; 
    step_and_dump_wave();
	
	sim_exit();

	return 0;
}

