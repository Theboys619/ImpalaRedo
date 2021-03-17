files := $(wildcard ./src/*.cpp)
objs := $(files:%.cpp=%.o)
CC := g++
CFLAGS := -std=c++2a
LIBS := -lcurl

.PHONY = dist/linux_x86/runtime

build: dist/linux_x86/runtime

dist/linux_x86/runtime: $(objs)
	$(CC) $(CFLAGS) $(objs) -o $@ $(LIBS)

%.o: %.cpp
	$(CC) $(CFLAGS) -c $< -o $@ $(LIBS)

download:
	sudo apt install libcurl4-openssl-dev

run:
	clear
	./dist/linux_x86/runtime ./examples/test.imp

clean:
	rm ./src/*.o
	rm ./dist/linux_x86/* -R