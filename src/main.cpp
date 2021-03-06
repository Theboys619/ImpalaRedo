#include <iostream>
#include "filesystem.h"
#include "interpreter.hpp"

using namespace Impala;

Value* log(std::vector<Value*> args) {
  for (Value* arg : args) {
    std::cout << arg->ToString() << " ";
  }

  std::cout << "\n";

  return new Value();
}

int main(int argc, char** argv) {
  std::string input = readFile("./examples/test.imp");
  Scope* globals = new Scope();

	Interpreter* interp = new Interpreter();

  globals->Define("log", new Function(interp, log));
  interp->SetGlobals(globals);

  interp->Interpret(input);
	Expression* ast = interp->ast;

	// std::cout <<
	// 	"Variable Name: " << ast->block[0]->left->value.getString() << "  "
	// 	<< "Value: " << ast->block[0]->right->value.getString() << std::endl;

  return 0;
}