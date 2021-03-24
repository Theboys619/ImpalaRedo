#include "include/http.h"

std::string fetchUrl(std::string url) {
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

  auto res = req.Get(path.c_str());

  if (res) {
    if (res->status == 200) {
      return res->body;
    }
  }

  return "";
}