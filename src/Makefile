CFLAGS+=`llvm-config-9 --cflags`
LLVM_LINK_FLAGS=`llvm-config-9 --libs --cflags --ldflags core analysis executionengine mcjit interpreter native --system-libs`

LEX?=flex
YACC?=bison
YFLAGS+=-d

all: jit_eval

scanner.o: parser.c

ast.o: parser.c

jit_eval: scanner.o parser.o ast.o utils.o
	$(CXX) -o $@ $^ $(LLVM_LINK_FLAGS) -rdynamic

clean:
	rm -f jit_eval ast.o scanner.o parser.o utils.o parser.c y.tab.h
