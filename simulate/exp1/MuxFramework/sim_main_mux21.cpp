#include <verilated.h>
#include <verilated_vcd_c.h>
#include <VMux21.h>

VerilatedContext* contextp = NULL;
VerilatedVcdC* tfp = NULL;

static VMux21* top = NULL;

void step_and_dump_wave() {
	top->eval();
	contextp->timeInc(1);
	tfp->dump(contextp->time());
}

void sim_init() {
	contextp = new VerilatedContext;
	tfp = new VerilatedVcdC;
	top = new VMux21{contextp};
	contextp->traceEverOn(true);
	top->trace(tfp, 0);
	tfp->open("Mux21.vcd");
}

void sim_exit() {
	step_and_dump_wave();
	tfp->close();
}

int main() {
	sim_init();

	top->s = 0;
	top->a = 0;
	top->b = 0;
	step_and_dump_wave();
	top->b = 1;
	step_and_dump_wave();
	top->a = 1;
	top->b = 0;
	step_and_dump_wave();
	top->b = 1;
	step_and_dump_wave();

	top->s = 1;
	top->a = 0;
	top->b = 0;
	step_and_dump_wave();
	top->b = 1;
	step_and_dump_wave();
	top->a = 1;
	top->b = 0;
	step_and_dump_wave();
	top->b = 1;
	step_and_dump_wave();

	sim_exit();
	delete top;
	delete tfp;
	delete contextp;

	return 0;
}

