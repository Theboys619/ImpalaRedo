#pragma once
#include <iostream>
#include <fstream>
#include <string>

std::string readFile(std::string filename);
void writeFile(std::string filename, std::string data);
void appendFile(std::string filename, std::string data);