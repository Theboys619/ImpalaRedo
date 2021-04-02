### Creating a C++ Module

Include the "impala.h" header file
The header file includes all necessary functions to create a shared library for the interpreter
```cpp
#include "impala.h"

// more stuff here
```

Each C++ Impala module must have an init function and definitions.
Example:
```cpp
#include "impala.h" // Maind header with interpreter definitions
#include <iostream>
#include <vector>

using namespace impala; // Makes it so you don't have to use the scope resolution operator

// Always a Value* return type
// Each function has a self value, a vetor of Value* for args,
// and a string for the filename that the interpreter is working in.
Value* hi(Value* self, std::vector<Value*> args, std::string filename) {
  std::cout << "Hi" << std::endl;

  return new Value(); // returns Impala "Nothing" or NULL
}

// Where property definitions go
std::vector<Definition> definitions = {
  { "hi", hi, 0 }, // (name, pointer, argCount, value pointer)
  // { "variable", nullptr, 0, val }
  // ^ Using this version allows for adding a Value* variable instead of a function
};

// Module info for definitions
ModuleInfo modInfo = {
  "hithere", // Name of module
  definitions // Definitions
};

// Using IMPALA_INIT defines the necessary return type and export if needed
IMPALA_INIT initmodule() {
  return CreateModule(&modInfo); // return the module
}
```

Compiling using g++:
```bash
g++ -fPIC -c -o libtest.o test.cpp
g++ -shared -o libtest.so libtest.o
```