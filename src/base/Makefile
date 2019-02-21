#CC=clang-7
CFLAGS=-g `/usr/lib/llvm-7/bin/llvm-config --cflags`
CXXFLAGS=-g

LEX_SOURCES=$(wildcard *.l)
LEX_OBJECTS=$(patsubst %.l,%.c,${LEX_SOURCES}) $(patsubst %.l,%.h,${LEX_SOURCES})

YACC_SOURCES=$(wildcard *.y)
YACC_OBJECTS=$(patsubst %.y,%.c,${YACC_SOURCES}) $(patsubst %.y,%.h,${YACC_SOURCES})

SOURCES=$(wildcard *.c)
OBJECTS=$(patsubst %.c,%.o,${SOURCES}) $(patsubst %.l,%.o,${LEX_SOURCES}) $(patsubst %.y,%.o,${YACC_SOURCES})

LEX?=flex
YACC?=bison
YFLAGS?=-dv

LLVM_LINK_FLAGS=`/usr/lib/llvm-7/bin/llvm-config --libs --cflags --ldflags core analysis irreader executionengine mcjit interpreter native --system-libs`

# ensure that the parser (header) is generated before other code is compiled
all: parser.c runtime.bc compiler

compiler: ${OBJECTS}
	$(CXX) -o $@ $^ $(LLVM_LINK_FLAGS) $(CXXFLAGS) -rdynamic

runtime.bc: runtime.c
	clang-7 -c -emit-llvm $^

clean:
	rm -rf compiler runtime.bc ${OBJECTS} ${LEX_OBJECTS} ${YACC_OBJECTS}
