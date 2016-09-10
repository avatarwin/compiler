#include <iostream>
#include <string>
#include <vector>

const int STACK_SIZE = 1024;

enum class InstructionCode {
	NOP = 0,
	LOAD,
	STORE,
	ADD,
	SUB,
	MUL,
	DIV,
	JMP,
	PRT,
};

enum {
	UNUSED = 0,
	U = UNUSED,
	R0 = 1,
	R1, R2, R3, R4, R5, R6, R7,
	PC = R0
};

struct instruction {
	InstructionCode code;
	int param1;
	int param2;
	int param3;
};

struct exec_block {
	std::vector<instruction> instructs;
};

struct VM {
	uint64_t registers[8];

	uint64_t stack[STACK_SIZE];
	void execute(exec_block& b);
};

void VM::execute(exec_block& b) {
	uint64_t ip = 0;
	
}

int main( int argv, char **argc ) {
	exec_block test;

	test.instructs.push_back( { InstructionCode::NOP, 0, 0, 0 } );
	test.instructs.push_back( { InstructionCode::PRT, R0, 0, 0 } );
}
