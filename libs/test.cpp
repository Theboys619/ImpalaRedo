#include "impala.h"
#include <vector>
#include <string>
using namespace Impala;

Value* printLol(Value* self, std::vector<Value*> args, std::string file) {
  std::cout << "Hithere, LOL" << std::endl;

  return new Value();
};

std::vector<Definition> definitions = {
  { "printLol", printLol, 0 }
};

ModuleInfo modInfo = {
  "hithere",
  definitions
};

IMPALA_INIT initmodule() {
  return CreateModule(&modInfo);
}