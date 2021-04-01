#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "../impala.h"
#include "httplib.h"

using namespace Impala;

std::string fetchUrl(std::string url, std::string method = "GET", std::string body = "", std::string headers = "") {
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

  if (host.size() == 0) return "";
  if (path.size() == 0) path = "/";

  httplib::Client req(host.c_str());

  if (method == "GET") {
    auto res = req.Get(path.c_str());

    if (res) {
      if (res->status == 200) {
        return res->body;
      }
    }
  } else if (method == "POST") {
    auto res = req.Post(path.c_str(), body.c_str(), headers.c_str());

    if (res) {
      if (res->status == 200) {
        return res->body;
      }
    }
  }

  return "";
}

// Class* http;
// std::vector<Definition> classMethods = {};

// args[0] = "URL" (STRING)
// args[1] = "METHOD" ("GET" | "POST")
// args[2] = "BODY" ("JSON" | "QUERY")
// args[3] = "HEADERS" ("TYPE")
Value* fetch(Value* self, std::vector<Value*> args, std::string file) {
  int argsSize = args.size();
  if (argsSize < 1) return new Value();

  std::string url = args[0]->ToString();
  std::string method = (argsSize < 2) ? "GET" : args[1]->ToString();
  std::string body = (argsSize < 3) ? "" : args[2]->ToString();
  std::string headers = (argsSize < 4) ? "" : args[3]->ToString();

  std::string resp = fetchUrl(url, method, body, headers);

  return new Value(resp);
}

Class* httpParams() {
  Class* httpParam = new Class("httpParams", {});
  httplib::Params params;
}

std::vector<Definition> definitions = {
  { "fetch", fetch, 4 }
};

ModuleInfo modInfo = {
  "http",
  definitions
};

IMPALA_INIT initmodule() {
  Class* params = httpParams();

  modInfo.definitions = definitions;
  return CreateModule(&modInfo);
}