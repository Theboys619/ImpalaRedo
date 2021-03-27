#include "impala.h"
#include <vector>
#include <string>
using namespace Impala;


Value* printLol(Value* self, std::vector<Value*> args, std::string file) {
  std::cout << "Hithere, LOL" << std::endl;

  return new Value();
};

Value* classes(Value* self, std::vector<Value*> args, std::string file) {
  std::cout << "C++ :flushed:. Imp classes and C++ shared libraries." << std::endl;

  return new Value();
}

std::vector<Definition> definitions = {
  { "printLol", printLol, 0 }
};

ModuleInfo modInfo = {
  "hithere",
  definitions
};

IMPALA_INIT initmodule() {
  Class* lol = new Class("lol", { { "classes", classes, 0 } });
  definitions.push_back({ "lol", nullptr, 0, lol });
  
  modInfo.definitions = definitions;
  return CreateModule(&modInfo);
}