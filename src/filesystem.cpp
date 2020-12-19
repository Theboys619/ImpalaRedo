#include "filesystem.h"

std::string readFile(std::string filename) {
	std::ifstream file(filename);
	std::string line;

	std::string text = "";

	bool isFirst = true;

	while(std::getline(file, line)) {
		if (isFirst) {
			text += line;
		} else {
			text += '\n' + line;
		}
	}

	file.close();

	return text;
};

void writeFile(std::string filename, std::string data) {
	std::ofstream file(filename);

	file << data;

	file.close();
};

void appendFile(std::string filename, std::string data) {
	std::string fileData = readFile(filename);
	fileData += data;
	
	writeFile(filename, fileData);
}