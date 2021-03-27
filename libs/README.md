### Creating a C++ Module

Include the "impala.h" header file
```cpp
#include "impala.h"

// more stuff here
```

Compiling:
```bash
g++ -fPIC -c -o libtest.o test.cpp
g++ -shared -o libtest.so libtest.o
```