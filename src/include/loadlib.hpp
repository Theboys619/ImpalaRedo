#include "common.h"
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) && !defined(__CYGWIN__)
// WINDOWS IMPL

#include <Windows.h>

class DynamicLib {
  public:
  DynamicLib(std::string libPath): libPath(libPath), _handle(nullptr) {};
  ~DynamicLib() = default;

  void OpenLib();

  template <typename T>
  std::shared_ptr<T> GetSymbol(const char *);

  void CloseLib();

  private:
  std::string libPath;
  void* _handle;
};

// #elif defined(unix) || defined(__linux__) || defined(__APPLE__)
#else
// UNIX IMPL

#include <dlfcn.h>

class DynamicLib {
  public:
  DynamicLib(std::string libPath): libPath(libPath), _handle(nullptr) {};
  ~DynamicLib() = default;

  void OpenLib() {

    if (!(_handle = dlopen(libPath.c_str(), RTLD_NOW | RTLD_LAZY))) {
      std::cerr << dlerror() << std::endl;
    }
    
  };

  template <typename T>
  std::shared_ptr<T> GetSymbol(const char * sym) {
    T item = reinterpret_cast<T>(dlsym(_handle, sym));

    if (item == nullptr) {
      CloseLib();
      std::cerr << dlerror() << std::endl;
    }

    return std::make_shared<T>(item);
  };

  void CloseLib() {

    if (dlclose(_handle) != 0) {
      std::cerr << dlerror() << std::endl;
    }

  };

  private:
  std::string libPath;
  void* _handle;
};

#endif