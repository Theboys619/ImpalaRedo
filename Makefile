files := ./src/main.cpp ./src/filesystem.cpp
CXX := g++ $(files) -std=c++17 -lboost_filesystem -I"./boost_1_75_0"

build:
	if [ -d ./dist ]; then \
		if [ -d ./dist/linux_x86 ]; then \
			$(CXX) -o ./dist/linux_x86/runtime; \
		else \
			mkdir ./dist/linux_x86; \
			$(CXX) -o ./dist/linux_x86/runtime; \
		fi \
	else \
		mkdir ./dist; \
		mkdir ./dist/linux_x86; \
		$(CXX) -o ./dist/linux_x86/runtime; \
	fi

run:
	clear
	./dist/linux_x86/runtime ./examples/test.imp

clean:
	rm ./dist/* -R
  