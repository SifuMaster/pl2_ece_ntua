befunge93+: befunge93_vm.o instr_codes.o stack.o torus_help.o
	gcc -o befunge93+ befunge93_vm.o instr_codes.o stack.o torus_help.o

befunge93_vm.o:
	gcc -c befunge93_vm.c
	
instr_codes.o:
	gcc -c instr_codes.c
	
stack.o:
	gcc -c stack.c

torus_help.o:
	gcc -c torus_help.c
	
clean:
	rm -f befunge93_vm.o instr_codes.o stack.o torus_help.o befunge93+
