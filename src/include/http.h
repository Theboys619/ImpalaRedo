#pragma once
#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "common.h"
#include "httplib.h"
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) && !defined(__CYGWIN__)
#include <Windows.h>
#endif

std::string fetchUrl(std::string url);