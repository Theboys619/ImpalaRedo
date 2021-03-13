#include <iostream>
#include <boost/filesystem.hpp>
#include "include/filesystem.h"
#include "include/interpreter.hpp"
using namespace Impala;
namespace fs = boost::filesystem;

Value* log(std::vector<Value*> args, std::string file) {
  for (Value* arg : args) {
    std::cout << arg->ToString() << " ";
  }

  std::cout << "\n";

  return new Value();
}

Value* input(std::vector<Value*> args, std::string file) {
  for (Value* arg : args)
    std::cout << arg->ToString();

  std::string x;
  std::getline(std::cin, x);

  return new Value(x);
}

Value* fileReadImp(std::vector<Value*> args, std::string file) {
  std::string fileName = args[0]->ToString();
  fs::path fullfile = fs::path(file).remove_filename() / fs::path(fileName);

  return new Value(readFile(fullfile.string()));
}

int raw(char* c, char** argv, Scope* globals, Interpreter* interp) {
  try {
    interp->RawInterp(c);
  } catch (Error& e) {
    std::string err = e.what();

    std::cout << "\x1b[31m" << err << std::endl;
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

  Value* Impala = new Value("[Impala]");
  Impala->Define("readFile", new Function(interp, fileReadImp));
  globals->Define("Impala", Impala);

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