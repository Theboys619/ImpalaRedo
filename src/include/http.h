#pragma once
#include "common.h"
#include <curl/curl.h>

size_t writeFunction(void *ptr, size_t size, size_t nmemb, std::string* data);
std::string fetchUrl(std::string url);