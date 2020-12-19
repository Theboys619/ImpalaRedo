files := ./src/main.cpp ./src/filesystem.cpp

build:
	if [ -d ./dist ]; then \
		if [ -d ./dist/linux_x86 ]; then \
			g++ $(files) -o ./dist/linux_x86/runtime -I./src/include; \
		else \
			mkdir ./dist/linux_x86; \
			g++ $(files) -o ./dist/linux_x86/runtime -I./src/include; \
		fi \
	else \
		mkdir ./dist; \
		mkdir ./dist/linux_x86; \
		g++ $(files) -o ./dist/linux_x86/runtime -I./src/include; \
	fi

run:
	clear
	./dist/linux_x86/runtime

clean:
	rm ./dist/* -R