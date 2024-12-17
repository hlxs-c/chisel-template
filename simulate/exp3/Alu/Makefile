# Default target
default: all

# Define variables
VERILATOR = verilator
VERILATOR_FLAGS = --cc --exe --build --trace -j 0

# Module name is given by cmd args
TARGET = Mux21	# Default module name

# Match filenames
SV_FILE = $(TARGET).sv
CPP_FILE = sim_main_$(shell echo $(TARGET) | tr '[:upper:]' '[:lower:]').cpp

# Make target
all: $(SV_FILE) $(CPP_FILE)
	@echo "Files to compile:"
	@echo "  Verilog: $(SV_FILE)"
	@echo "  C++: $(CPP_FILE)"
	@echo "Compiling $(TARGET)..."
	$(VERILATOR) $(VERILATOR_FLAGS) -top-module $(TARGET) $(CPP_FILE) $(SV_FILE)
	@echo "Compiling $(TARGET) complete..."
	@echo "./obj_dir/V${TARGET} to run, and use gtkwave to analysis the vcd"

# Help target
help:
	@echo "Available targets:"
	@echo "	make TARGET=<module_name> (e.g., make TARGET=Mux21)"
	@echo "	where <module_name> is the name of the Verilog Module"

# Clean target
clean:
	rm -rf obj_dir *.vcd