#include <iostream>
#include <filesystem>
#include "include/http.h"
#include "include/filesystem.h"
#include "include/interpreter.hpp"
#include "include/globals.hpp"
using namespace Impala;

Value* log(Value* self, std::vector<Value*> args, std::string file) {
  for (Value* arg : args) {
    std::cout << arg->ToString() << " ";
  }

  std::cout << "\n";

  return new Value();
}

Value* input(Value* self, std::vector<Value*> args, std::string file) {
  for (Value* arg : args)
    std::cout << arg->ToString();

  std::string x;
  std::getline(std::cin, x);

  return new Value(x);
}

int raw(char* c, char** argv, Scope* globals, Interpreter* interp) {
  try {
    interp->RawInterp(c);
  } catch (Error& e) {
    std::string err = e.what();

    std::cout << "\x1b[31m" << err << "\x1b[0m" << std::endl;
  }

  return 0;
}

int main(int argc, char** argv) {
  if (argc < 2) return 0;

  Scope* globals = new Scope();

	Interpreter* interp = new Interpreter();

  globals->Define("log", new Function(interp, log));
  globals->Define("input", new Function(interp, input));
  interp->SetGlobals(globals);

  DefineGlobals(interp, globals);

  if (std::string(argv[1]) == "-e")
    return raw(argv[2], argv, globals, interp);
  
  fs::path impFile = argv[1];
  fs::path fullFile = fs::current_path() / impFile;

  try {
    interp->Interpret(fullFile.string());
  } catch (Error& e) {
    std::string err = e.what();

    std::cout << "\x1b[31m" << err << "\x1b[0m" << std::endl;
  }

  return 0;
}