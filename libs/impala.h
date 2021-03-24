#ifndef Impala_H
#define Impala_H

#include <iostream>
#include <vector>
#include <regex>
#include <string>
#include <unordered_map>

namespace Impala {
  class Interpreter;
  class Scope;
  class Expression;

  // Types for the Value Class to know where to cast and how to cast it
  // Have to use this because of void* since we don't know the type
  enum class ValueType {
    Nothing,
    String,
    Bool,
    Int,
    Double,
    Char,
    Function,
    Class,
    Object
  };

  std::vector<std::string> ValueStrings = {
		"Nothing",
    "String",
    "Bool",
    "Int",
    "Double",
    "Char",
    "Function",
    "Class",
    "Object"
	};

	std::string getValueString(ValueType type) {
		return ValueStrings[((int)type)];
	}

  bool hasEnding(std::string const &fullString, std::string const &ending) {
    if (fullString.length() >= ending.length()) {
        return (0 == fullString.compare(fullString.length() - ending.length(), ending.length(), ending));
    } else {
        return false;
    }
  }

  bool isNumber(std::string token)
  {
      return std::regex_match(token, std::regex(("((\\+|-)?[[:digit:]]+)(\\.(([[:digit:]]+)?))?")));
  }

  // Class for values like objects, ints, and other types
  // Used for casting and doing dynamic things
  class Value {
    ValueType type;
    void* value;

    std::unordered_map<std::string, Value*> props = std::unordered_map<std::string, Value*>();

    public:
    Scope* objScope;
    Value* parent;
    std::string explicitType;
    bool isClassObj = false;
    bool returned = false;

    Value() {
      type = ValueType::Nothing;
      value = nullptr;
      explicitType = "nothing";
      objScope = nullptr;
      parent = nullptr;
    };

    Value(ValueType type, void* value, std::string explicitType = "any")
    : type(type),
      value(value),
      explicitType(explicitType) {
        parent = nullptr;
        objScope = nullptr;
      };

    Value(ValueType type, std::string val, std::string explicitType = "any")
    : type(type),
      explicitType(explicitType)
    {
      value = new std::string(val);
      objScope = nullptr;
      parent = nullptr;
    };

    Value(bool val, std::string explicitType = "any")
    : explicitType(explicitType)
    {
      type = ValueType::Bool;
      value = new bool(val);
      objScope = nullptr;
      parent = nullptr;
    }

    Value(int val, std::string explicitType = "any")
    : explicitType(explicitType)
    {
      type = ValueType::Int;
      value = new int(val);
      objScope = nullptr;
      parent = nullptr;
    }
    Value(const char* val, std::string explicitType = "any"): explicitType(explicitType) {
      type = ValueType::String;
      value = new std::string(val);
      objScope = nullptr;
      parent = nullptr;
    }
    Value(std::string val, std::string explicitType = "any"): explicitType(explicitType) {
      type = ValueType::String;
      value = new std::string(val);
      objScope = nullptr;
      parent = nullptr;
    }

    void SetScope(Scope* scpe) {
      if (type == ValueType::Object)
        objScope = scpe;
    }

    std::unordered_map<std::string, Value*> GetProps() {
      return props;
    }

    Scope* GetScope() {
      if (type == ValueType::Object && objScope != nullptr)
        return objScope;

      if (parent != nullptr)
        return parent->GetScope();

      return nullptr;
    }

    virtual std::string ToString() {
      if (type == ValueType::String) {
        return Cast<std::string>();
      } else if (type == ValueType::Int) {
        return std::to_string(Cast<int>());
      } else if (type == ValueType::Bool) {
        bool v = Cast<bool>();
        
        if (v) return "true";
        else return "false";
      } else if (type == ValueType::Object) {
        return "[Object " + Cast<std::string>() + "]";
      } else {
        return "";
      }
    }

    void SetExplicit(std::string t) {
      explicitType = t;
    }

    ValueType GetType() {
      return type;
    }

    int ToInt() {
      // No floating point yet

      if (type == ValueType::String)
        return std::stoi(Cast<std::string>());

      if (type == ValueType::Nothing)
        return 0;
      
      if (type == ValueType::Bool)
        return Cast<bool>();

      return Cast<int>();
    }

    bool ToBool() {
      if (type == ValueType::Int)
        return ToInt() == 1;

      if (type != ValueType::Bool)
        return type != ValueType::Nothing;

      return Cast<bool>();
    }

    template<typename T>
    T Cast() {
      return T(*(T*)value);
    }

    bool HasProp(std::string key) {
      if (props.size() < 1) return false;
      return props.find(key) != props.end();
    }

    bool checkType(std::string t1, std::string t2) {
      return (
        t1 == "any" || t2 == "nothing" || (t1 == t2)
      );
    }

    Value* Define(Expression* exp, Value* prop);

    Value* Define(std::string key, Value* prop) {
      return props[key] = prop;
    }

    void Define(std::string key) {
      props[key] = new Value();
    }

    Value* Get(std::string key) {
      if (HasProp(key))
        return props[key];

      return new Value();
    }

    Value* operator+(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        if (bType == ValueType::Int)
          return new Value(Cast<std::string>() + std::to_string(b.Cast<int>()));

        if (bType == ValueType::String)
          return new Value(Cast<std::string>() + b.Cast<std::string>());
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw std::exception();

        if (bType == ValueType::Int)
          return new Value(Cast<int>() + b.Cast<int>(), "number");
      }

      throw std::exception();
    }

    Value* operator-(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        throw std::exception();
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw std::exception();

        if (bType == ValueType::Int)
          return new Value(Cast<int>() - b.Cast<int>(), "number");
      }

      throw std::exception();
    }

    Value* operator/(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        throw std::exception();
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw std::exception();

        if (bType == ValueType::Int)
          return new Value(Cast<int>() / b.Cast<int>(), "number");
      }

      throw std::exception();
    }

    Value* operator*(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        throw std::exception();
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw std::exception();

        if (bType == ValueType::Int)
          return new Value(Cast<int>() * b.Cast<int>(), "number");
      }

      throw std::exception();
    }

    Value* operator%(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        throw std::exception();
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw std::exception();

        if (bType == ValueType::Int)
          return new Value(Cast<int>() % b.Cast<int>(), "number");
      }

      throw std::exception();
    }

    Value* operator==(Value& b) {
      if (type == ValueType::Int) {
        if (b.GetType() == ValueType::String) {
          if (isNumber(b.ToString())) {
            return new Value(ToInt() == b.ToInt(), "boolean");
          } else {
            return new Value(false, "boolean");
          }
        }
        return new Value(ToInt() == b.ToInt(), "boolean");
      } else if (type == ValueType::String) {
        return new Value(ToString() == b.ToString(), "boolean");
      }

      return new Value(ToBool() == b.ToBool(), "boolean");
    }

    Value* operator>(Value& b) {
      if (type == ValueType::Int) {
        return new Value(ToInt() > b.ToInt(), "boolean");
      } else if (type == ValueType::String) {
        return new Value(ToString().size() > b.ToString().size(), "boolean");
      }

      return new Value(ToBool() > b.ToBool(), "boolean");
    }

    Value* operator<(Value& b) {
      if (type == ValueType::Int) {
        return new Value(ToInt() < b.ToInt(), "boolean");
      } else if (type == ValueType::String) {
        return new Value(ToString().size() < b.ToString().size(), "boolean");
      }

      return new Value(ToBool() < b.ToBool(), "boolean");
    }

    Value* operator<=(Value& b) {
      if (type == ValueType::Int) {
        return new Value(ToInt() <= b.ToInt(), "boolean");
      } else if (type == ValueType::String) {
        return new Value(ToString().size() <= b.ToString().size(), "boolean");
      }

      return new Value(ToBool() <= b.ToBool(), "boolean");
    }

    Value* operator>=(Value& b) {
      if (type == ValueType::Int) {
        return new Value(ToInt() >= b.ToInt(), "boolean");
      } else if (type == ValueType::String) {
        return new Value(ToString().size() >= b.ToString().size(), "boolean");
      }

      return new Value(ToBool() >= b.ToBool(), "boolean");
    }
	};

  // Where variables are stored and different data
  class Scope {
    public:
    Scope* parent;
    std::unordered_map<std::string, Value*> props = std::unordered_map<std::string, Value*>();
    int extensions = 0;

    std::string filepath;
    bool ClassScope = false;

    Scope() {
      parent = nullptr;
      filepath = "unknown";
    };
    Scope(Scope* parent): parent(parent) {
      filepath = parent->filepath;
      ClassScope = parent->ClassScope;
    }

    void SetFile(std::string file) {
      filepath = file;
    }

    bool HasProp(std::string key) {
      if (props.size() < 1) return false;
      return props.find(key) != props.end();
    }

    Scope* Lookup(std::string key) {
      if (HasProp(key)) return this;

      if (parent != nullptr)
        return parent->Lookup(key);

      return nullptr;
    }

    bool checkType(std::string t1, std::string t2) {
      return (
        t1 == "any" || t2 == "nothing" || (t1 == t2)
      );
    }

    Value* Set(Expression* exp, Value* prop);

    Value* Set(std::string key, Value* prop) {
      Scope* scope = Lookup(key);
      if (scope != nullptr)
        return scope->Define(key, prop);
      else
        return Define(key, prop);
    }

    Value* Define(Expression* exp, Value* prop);

    Value* Define(std::string key, Value* prop) {
      return props[key] = prop;
    }

    void Define(std::string key) {
      props[key] = new Value();
    }

    Value* Get(std::string key) {
      if (HasProp(key))
        return props[key];

      Scope* scope = Lookup(key);
      if (scope != nullptr)
        return scope->Get(key);

      if (ClassScope) {
        Value* obj = Get("this");
        Value* propVal = new Value();

        if (obj != nullptr && obj->GetType() != ValueType::Nothing) {
          propVal = obj->Get(key);

          if (propVal == nullptr) throw std::exception();

          return propVal;
        }
      }

      throw std::exception();
    }

    Scope* Extend() {
      extensions++;
      return new Scope(this);
    }
  };

  class Args {
    std::vector<Value*> args;

    public:
    Args() {};

    Value* Get(int i) {
      return args.at(i);
    }

    int Push(Value* value) {
      args.push_back(value);
      return args.size() - 1;
    }
  };

  // For native functions / C++ built in functions
  extern "C" typedef Value* (*CFunction)(Value*, std::vector<Value*>, std::string);

  // Function class to handle Native and Impala functions
  // Needs interpreter* for implementations below
  class Function : public Value {
    public:
    Expression* exp;
    Scope* scope;

    CFunction function;
    Interpreter* interpreter;
    bool isNative = false;

    std::vector<Expression*> argsDefs;

    // Constructor for Native C++ Functions
    Function(Interpreter* interpreter, CFunction function)
    : function(function),
      interpreter(interpreter),
      Value(ValueType::Function, new std::string("[Function built-in]"))
    {
      exp = nullptr; // Since its native we don't use Expression* (ptrs)
      isNative = true;
      explicitType = "function";
    }

    // Constructor for Impala Functions
    Function(Interpreter* interpreter, Expression* exp, Scope* scope);
    
    virtual std::string ToString() {
      return Cast<std::string>();
    }

    // Defined instead of implemented because of the use of the Interpreter* which is defined after Function class
    // Needs to be imeplemented below interpreter
    // Value* Call(std::vector<Value*> vals, Value* thisobj, Scope* scp, bool checkType = true);
    Value* Call(std::vector<Value*> args, Value* thisobj, Scope* scp, bool checkType = true);
    Value* Call(std::vector<Expression*> vals, Value* thisobj, Scope* scp, bool checkType = true);
  };


  extern "C" {
    struct Definition {
      const char* name;
      CFunction function;
      int argCount;
    };
    struct ModuleInfo {
      const char* moduleName;
      std::vector<Definition> definitions;
    };
    class ImpModule {
      public:
      ModuleInfo* info;
      Value* obj;

      ImpModule(ModuleInfo* info, Value* obj): info(info), obj(obj) {};
    };
  };

  typedef ImpModule* (*InitFunction)();
}; // namespace Impala

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) && !defined(__CYGWIN__)
#define IMPALA_EXPORT extern "C" __declspec(dllexport)
#define IMPALA_INIT extern "C" __declspec(dllexport) Impala::ImpModule*
#else
#define IMPALA_EXPORT
#define IMPALA_INIT extern "C" Impala::ImpModule*
#endif

Impala::ImpModule* CreateModule(Impala::ModuleInfo* info) {
  Impala::Value* obj = new Impala::Value(Impala::ValueType::Object, "C ImpCModule");

  return new Impala::ImpModule(info, obj);
}

#endif