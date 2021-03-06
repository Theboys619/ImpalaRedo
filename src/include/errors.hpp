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

  class SyntaxError : public std::exception {
    public:
    std::string message;

    SyntaxError(std::string message)
      : message("SyntaxError: Invalid or unexpected token '" + message + "'")
    {}
    SyntaxError(char message)
      : message("SyntaxError: Invalid or unexpected token '" + std::string(1, message) + "'")
    {}

    virtual const char* what() const throw() {
      return message.c_str();
    }
  };

  class UndeclaredError : public std::exception {
    public:
    std::string message;

    UndeclaredError(std::string message)
      : message("Error: Use of undeclared identifier '" + message + "'")
    {}
    
    virtual const char* what() const throw() {
      return message.c_str();
    }
  };

  class TypeError : public Error {
    public:
    TypeError(std::string message): Error(message, "TypeError") {};
    TypeError(char message): Error(message, "TypeError") {};
  };
};