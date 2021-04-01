#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "impala.h"
#include "httplib.h"
#include <vector>
#include <string>
#include <regex>
using namespace Impala;

Value* fetchUrl(Value* self, std::vector<Value*> args, std::string file) {
  std::string url = args[0]->ToString();
  std::string host;
  std::string path;

  std::regex rg(R"(^(https?\:\/\/.*?)(/.*)?$)");
  std::smatch sm;

  std::regex_match(url, sm, rg);

  for (unsigned i = 0; i < sm.size(); i++) {
    std::string item = sm[i];

    if (i == 1 && item.size() > 0)
      host = item;
    else if (i == 2 && item.size() > 0)
      path = item;
  }

  if (host.size() == 0) return new Value();
  if (path.size() == 0) path = "/";

  httplib::Client req(host.c_str());

  auto res = req.Get(path.c_str());

  if (res) {
    if (res->status == 200) {
      return new Value(std::string(res->body));
    }
  }

  return new Value();
}

Value* printLol(Value* self, std::vector<Value*> args, std::string file) {
  std::cout << "Hithere, LOL" << std::endl;

  return new Value();
};

Value* classes(Value* self, std::vector<Value*> args, std::string file) {
  std::cout << "C++ :flushed:. Imp classes and C++ shared libraries." << std::endl;

  return new Value();
}

std::vector<Definition> definitions = {
  { "printLol", printLol, 0 },
  { "fetchUrl", fetchUrl, 1 }
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