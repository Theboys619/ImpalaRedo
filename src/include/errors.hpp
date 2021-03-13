#pragma once
#include <iostream>
#include <exception>
#include <string>

namespace Impala {
  class Error : public std::exception {
    public:
    std::string message;

    Error(std::string message, std::string type = "Error")
      : message(type + ": " + message)
    {}
    Error(char message, std::string type = "Error")
      : message(type + ":" + std::string(1, message) + "'")
    {}

    virtual const char* what() const throw() {
      return message.c_str();
    }
  };

  class SyntaxError : public Error {
    public:

    SyntaxError(std::string message)
      : Error("Invalid or unexpected token '" + message + "'", "SyntaxError")
    {};
    SyntaxError(char message)
      : Error("Invalid or unexpected token '" + std::string(1, message) + "'", "SyntaxError")
    {};

  };

  class UndeclaredError : public Error {
    public:
    std::string message;

    UndeclaredError(std::string message)
      : Error("Use of undeclared identifier '" + message + "'")
    {};
  };

  class TypeError : public Error {
    public:
    TypeError(std::string t1, std::string t2, std::string v, std::string file)
    : Error("Can't assign type " + t1 + " to variable " + v + " of type " + t2, "TypeError") {};

    TypeError(char message): Error(message, "TypeError") {};
  };
};