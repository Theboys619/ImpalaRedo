#include "include/http.h"

size_t writeFunction(void* ptr, size_t size, size_t nmemb, std::string* data) {
    data->append((char*)ptr, size * nmemb);
    return size * nmemb;
}

std::string fetchUrl(std::string url) {
  CURL *curl;
  int result;

  curl = curl_easy_init();

  curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
  curl_easy_setopt(curl, CURLOPT_USERAGENT, "curl/7.42.0");
  curl_easy_setopt(curl, CURLOPT_MAXREDIRS, 50L);
  curl_easy_setopt(curl, CURLOPT_TCP_KEEPALIVE, 1L);

  std::string response_string;
  std::string header_string; // Unused
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeFunction);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_string);
  curl_easy_setopt(curl, CURLOPT_HEADERDATA, &header_string);

  char* nurl; // Unused
  long response_code; // Unused
  double elapsed; // Unused
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
  curl_easy_getinfo(curl, CURLINFO_TOTAL_TIME, &elapsed);
  curl_easy_getinfo(curl, CURLINFO_EFFECTIVE_URL, &nurl);

  result = curl_easy_perform(curl);

  curl_easy_cleanup(curl);
  curl = NULL;

  if (result == CURLE_OK) {
    return response_string;
  } else {
    return "";
  }
}