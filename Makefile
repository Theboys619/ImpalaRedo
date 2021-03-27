files := $(wildcard ./src/*.cpp)
objs := $(files:%.cpp=%.o)
CC := g++-8
CFLAGS := -std=c++2a
LIBS = -lpthread -lstdc++fs -lssl -lcrypto -ldl

.PHONY = dist/linux_x86/impala

build: dist/linux_x86/impala

dist/linux_x86/impala: $(objs)
	$(CC) $(CFLAGS) $(objs) -o $@ $(LIBS)

%.o: %.cpp
	$(CC) $(CFLAGS) -c $< -o $@

run:
	clear
	./dist/linux_x86/impala ./examples/test.imp



clean:
	rm ./src/*.o
	rm ./dist/linux_x86/* -R