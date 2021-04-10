## Impala

This is a redone version of a Repl.it code jam language I have made. This is completely different from that one though.
Impala is a generic multipurpose-programming language.

### Compiling / Building

Need a C++ compiler with C++2a support. C++2a is used for `std::filesystem`.
Also make sure to create folders for the corresponding executable.
These should be in a `~/dist` folder in the root of the project.

Also some dependencies which are:
- openssl for the `-lssl` and `-lcrypto` static libraries
- standard c++ filesystem helper aka `-lstdc++fs`

### Linux
In the `~/dist` folder create a new folder called `linux_x86`. The folder path should be `~/dist/linux_x86`

**Compiling:**
```bash
# or make linux
make
```

### Windows
Have the Windows.h files and SDK installed.
In the `~/dist` folder create a new folder called `win_x86`. The folder path should be `~/dist/win_x86`

**Compiling:**
```bash
make windows
```

### Running a file
*Example File:* `hello.imp` -> Prints 'Hello, World!'
```bash
impala hello.imp # -> Prints 'Hello, World!'
```

**Arguments:**
- `-e <code>`: Evaluate/Run code string instead of a file.