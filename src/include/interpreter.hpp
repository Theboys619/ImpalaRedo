#pragma once
#include <regex>
#include "parser.hpp"

namespace Impala {
  class Interpreter; // To be able to use Interpreter*s in classes defined before Interpreter.
  class Scope;

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

    Value* Define(Expression* exp, Value* prop) {
      if (exp->access != nullptr) {

      }

      std::string type = exp->dataType;

      if (!checkType(type, prop->explicitType)) {
        if (
          prop->GetType() != ValueType::Function &&
          type != "function"
        ) throw TypeError(prop->explicitType, type, exp->value.getString(), exp->value.getFile());
        else if (
          prop->GetType() == ValueType::Function &&
          type != "function"
        ) throw TypeError("Function", type, exp->value.getString(), exp->value.getFile());
      }
      
      return Define(exp->value.getString(), prop);
    }

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
          throw Error("Cannot add number to string");

        if (bType == ValueType::Int)
          return new Value(Cast<int>() + b.Cast<int>(), "number");
      }

      throw Error("Cannot perform operation");
    }

    Value* operator-(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        throw Error("Cannot subract from string");
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw Error("Cannot subtract string from number");

        if (bType == ValueType::Int)
          return new Value(Cast<int>() - b.Cast<int>(), "number");
      }

      throw Error("Cannot perform operation");
    }

    Value* operator/(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        throw Error("Cannot divide from string");
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw Error("Cannot divide string from number");

        if (bType == ValueType::Int)
          return new Value(Cast<int>() / b.Cast<int>(), "number");
      }

      throw Error("Cannot perform operation");
    }

    Value* operator*(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        throw Error("Cannot multiply from string");
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw Error("Cannot multiply string and number");

        if (bType == ValueType::Int)
          return new Value(Cast<int>() * b.Cast<int>(), "number");
      }

      throw Error("Cannot perform operation");
    }

    Value* operator%(Value& b) {
      ValueType bType = b.GetType();
      if (type == ValueType::String) {
        throw Error("Cannot mod from string");
      } else if (type == ValueType::Int) {
        if (bType == ValueType::String)
          throw Error("Cannot mod string and number");

        if (bType == ValueType::Int)
          return new Value(Cast<int>() % b.Cast<int>(), "number");
      }

      throw Error("Cannot perform operation");
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
	};

  class Class : public Value {
    public:
    std::vector<Expression*> instructions;
    Interpreter* interpreter;
    Scope* scope;
    std::string name;

    std::vector<Expression*> argsDefs = std::vector<Expression*>();

    Class(): Value(ValueType::Class, nullptr) {
      explicitType = "nothing";
      name = "";
    };

    Class(Interpreter* intrp, std::string nme, Scope* scope)
    : Value(ValueType::Class, nme, nme),
      name(nme),
      scope(scope)
    {
      instructions = {};
      interpreter = intrp;
      scope = nullptr;
      explicitType = nme;
    };

    Class(Interpreter* intrp, Expression* exp, Scope* scope)
    : Value(ValueType::Class, exp->value.getString(), exp->value.getString()),
      scope(scope)
    {
      instructions = exp->scope->block;
      interpreter = intrp;
      explicitType = exp->value.getString();
      name = exp->value.getString();
    };

    Value* Construct(std::vector<Expression*> args, Scope* scope);

    virtual std::string ToString() {
      return "[Class " + Cast<std::string>() + "]";
    }
  };

  // Where variables are stored and different data
  class Scope {
    public:
    Scope* parent;
    std::unordered_map<std::string, Value*> props = std::unordered_map<std::string, Value*>();

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

    Value* Set(Expression* exp, Value* prop) {
      std::string type = Get(exp->value.getString())->explicitType;

      if (!checkType(type, prop->explicitType)) {
        if (
          prop->GetType() != ValueType::Function &&
          type != "function"
        ) throw TypeError(prop->explicitType, type, exp->value.getString(), exp->value.getFile());
        else if (
          prop->GetType() == ValueType::Function &&
          type != "function"
        ) throw TypeError ("Function", type, exp->value.getString(), exp->value.getFile());
      }

      Set(exp->value.getString(), prop);
    }

    Value* Set(std::string key, Value* prop) {
      Scope* scope = Lookup(key);
      if (scope != nullptr)
        return scope->Define(key, prop);
      else
        return Define(key, prop);
    }

    Value* Define(Expression* exp, Value* prop) {
      std::string type = exp->dataType;

      if (!checkType(type, prop->explicitType)) {
        if (
          prop->GetType() != ValueType::Function &&
          type != "function"
        ) throw TypeError(prop->explicitType, type, exp->value.getString(), exp->value.getFile());
        else if (
          prop->GetType() == ValueType::Function &&
          type != "function"
        ) throw TypeError("Function", type, exp->value.getString(), exp->value.getFile());
      }
      
      return Define(exp->value.getString(), prop);
    }

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

          if (propVal == nullptr) throw UndeclaredError(key);

          propVal->parent = obj;

          return propVal;
        }
      }

      throw UndeclaredError(key);
    }

    Scope* Extend() {
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
  typedef Value* (*CFunction)(std::vector<Value*>, std::string);

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
      Value(ValueType::Function, nullptr)
    {
      exp = nullptr; // Since its native we don't use Expression* (ptrs)
      isNative = true;
    }

    // Constructor for Impala Functions
    Function(Interpreter* interpreter, Expression* exp, Scope* scope)
    : interpreter(interpreter),
      exp(exp),
      scope(scope),
      Value(ValueType::Function, nullptr)
    {
      function = nullptr;
      isNative = false; // just incase
      argsDefs = exp->args; // args are Identifiers / definitions
      explicitType = exp->dataType;
    }

    // Defined instead of implemented because of the use of the Interpreter* which is defined after Function class
    // Needs to be imeplemented below interpreter
    Value* Call(std::vector<Expression*> args, Scope* scp, bool checkType = true); // The Call Method
    Value* Call(std::vector<Expression*> args, Scope* propScope, Scope* scp, bool checkType = true);
  };

  // Main Interpreter class
  class Interpreter {
    public:
    Expression* ast;
    Scope* topScope; // For Globals (Top / Main Scope)

    Interpreter() {
      topScope = new Scope();
    }

    Interpreter(Scope* scope) {
      topScope = scope;
    }

    void SetGlobals(Scope* scope) {
      topScope = scope;
    }

    bool checkType(Expression* exp, std::string type) {
      std::string expType = exp->dataType;

      return expType == "any" || expType == type;
    }

    Value* GetProp(Value* prop, Expression* exp, Scope* scope) {
      Scope* propScope = nullptr;
      if (prop->parent != nullptr || prop->GetType() == ValueType::Object) {
        propScope = prop->GetScope();
      }

      if (exp->type == ExprTypes::Assign) {
        std::string propName = exp->left->value.getString();
        Value* right = Evaluate(exp->right, (propScope != nullptr ? propScope : scope));

        if (right->parent == nullptr)
          right->parent = prop;

        Value* newProp = prop->Define(propName, right);

        return newProp;
      }

      if (exp->type == ExprTypes::Binary) {
        Value* left = GetProp(prop, exp->left, scope);
        Value* right = Evaluate(exp->right, scope);
        std::string op = exp->op.getString();

        return iOperation(left, right, op);
      }
      
      if ((exp->type != ExprTypes::Identifier) && (exp->type != ExprTypes::FunctionCall))
        throw Error("Cannot access property.");

      Value* newProp = prop->Get(exp->value.getString());

      if (prop->GetType() == ValueType::Nothing)
        throw Error("Cannot access property " + exp->value.getString() + " from nothing.");

      if (exp->access != nullptr) {
        return AccessIndex(newProp, exp->access, scope);
      }

      if (exp->dotOp != nullptr) {
        return GetProp(prop->Get(exp->value.getString()), exp->dotOp, (propScope != nullptr ? propScope : scope));
      }

      if (exp->type == ExprTypes::FunctionCall) {
        if (newProp->GetType() != ValueType::Function)
          throw Error("Cannot call function as it is not one.");

        return ((Function*)newProp)->Call(exp->args, propScope->Extend(), scope->Extend());
      }

      return newProp;
    }

    Value* AccessIndex(Value* prop, Expression* access, Scope* scope) {
      Value* val = Evaluate(access, scope);
      std::string index = val->ToString();

      Value* item = prop->Get(index);

      if (prop->GetType() == ValueType::Nothing)
        throw Error("Cannot access index " + index + " from nothing.");

      if (access->access != nullptr) {
        return AccessIndex(item, access->access, scope);
      }

      return item;
    }

    Value* ParentAccessedIndex(Value* prop, Expression* access, Scope* scope) {
      std::string index = Evaluate(access, scope)->ToString();

      Value* item = prop->Get(index);

      if (prop->GetType() == ValueType::Nothing)
        throw Error("Cannot access index " + index + " from nothing.");

      if (access->access != nullptr) {
        return ParentAccessedIndex(item, access->access, scope);
      }

      return prop;
    }

    std::string GetLastIndex(Expression* access, Scope* scope) {
      if (access->access != nullptr) {
        return GetLastIndex(access->access, scope);
      }

      std::string index = Evaluate(access, scope)->ToString();

      return index;
    }

    Value* iIdentifier(Expression* exp, Scope* scope) {
      Value* identifier = scope->Get(exp->value.getString());

      if (exp->access != nullptr) {
        return AccessIndex(identifier, exp->access, scope);
      }

      if (exp->dotOp != nullptr) {
        return GetProp(identifier, exp->dotOp, scope);
      }

      return identifier;
    }

    Value* iVariable(Expression* exp, Scope* scope) {
      Value* value = new Value();
      value->SetExplicit(exp->dataType);

      scope->Define(
        exp->value.getString(),
        value
      );

      return value;
    }

    Value* iAssign(Expression* exp, Scope* scope) {
      Value* right = Evaluate(exp->right, scope); // Visit / interpret right branch

      if (exp->left->type == ExprTypes::Identifier) {
        if (scope->parent != nullptr && scope->parent->ClassScope) {
          Scope* scp = scope->parent->Lookup("this");

          if (scp != nullptr) {
            Value* obj = scp->Get("this");

            std::string propName = exp->left->value.getString();

            if (obj->HasProp(propName) && propName != "this") {
              if (exp->left->access != nullptr) {
                Value* val = ParentAccessedIndex(obj->Get(propName), exp->left->access, scope);
                std::string index = GetLastIndex(exp->left->access, scope);
                
                val->Define(index, right);
                scp->Define("this", obj);
                scope->Define(exp->left, val);
                return right;
              }

              obj->Define(exp->left, right);
              scp->Define("this", obj);
              scope->Define(exp->left, right);

              return right;
            }
          }
        }

        if (exp->left->access != nullptr) {
          Value* val = ParentAccessedIndex(scope->Get(exp->left->value.getString()), exp->left->access, scope);
          std::string index = GetLastIndex(exp->left->access, scope);
          
          val->Define(index, right);
          scope->Define(exp->left, val);
          return right;
        }
      } else {
        Evaluate(exp->left, scope); // Visit / interpret left branch
      }

      Value* val = iIdentifier(exp->left, scope); // Just checks to see if it exists

      scope->Set(exp->left, right); // Define or set the variable

      return right;
    }

    Value* iOperation(Value* a, Value* b, std::string op) {
      if (op == "+") {
        return *a + *b;
      } else if (op == "-") {
        return *a - *b;
      } else if (op == "/") {
        return *a / *b;
      } else if (op == "*") {
        return *a * (*b);
      } else if (op == "%") {
        return *a % *b;
      } else if (op == "==") {
        return *a == *b;
      } else if (op == ">") {
        return *a > *b;
      } else if (op == "<") {
        return *a < *b;
      }

      return new Value();
    }

    Value* iBinary(Expression* exp, Scope* scope) {
      Value* a = Evaluate(exp->left, scope);
      Value* b = Evaluate(exp->right, scope);
      std::string op = exp->op.getString();

      return iOperation(a, b, op);
    }

    Value* iFunction(Expression* exp, Scope* scope) {
      Function* func = new Function(this, exp, scope); // Creates a new function with the interpreter

      // Check if it is already defined, if so throw an error.
      if (scope->Lookup(exp->value.getString()) != nullptr) {
        throw Error("Function already defined");
      }

      scope->Define(exp->value.getString(), func); // Define it

      return func; // return it
    }

    Value* iScope(Expression* exp, Scope* scope) {
      Value* returnValue = nullptr;

      std::vector<Expression*> block = exp->block;

      Scope* newScope = scope->Extend(); // Extend scope since we are going into a new scope

      // Loop through each expression / statement
      for (Expression* expr : block) {
        returnValue = Evaluate(expr, newScope); // Evaluate each statement or expression

        if (scope != topScope && returnValue != nullptr && returnValue->returned)
          return returnValue; // Check if it is a returnValue or has been returned and return (only for functions)
      }

      if (returnValue == nullptr) return new Value(); // return nothing;

      return returnValue; // return back the return value from Impala
    }

    Value* iFunctionCall(Expression* exp, Scope* scope) {
      Value* func = scope->Get(exp->value.getString()); // Gets the Value* which is actually a pointer to Function

      if (
        func->GetType() != ValueType::Function &&
        func->GetType() != ValueType::Class
      ) // If its not a pointer to the Function or Class throw an Error.
        throw Error("Cannot call function as it is not one.");

      Scope* funcScope;
      if (func->parent != nullptr)
        funcScope = func->GetScope();
      else
        funcScope = scope->Extend();

      if (func->GetType() == ValueType::Class) {
        Class* clss = (Class*)func;
        
        Value* returnVal = clss->Construct(exp->args, funcScope);

        if (exp->dotOp != nullptr)
          return GetProp(returnVal, exp->dotOp, returnVal->GetScope());

        return returnVal;
      }

      // Cast to a Function* then call the method Call with appropiate arguments
      Value* returnVal = ((Function*)func)->Call(exp->args, funcScope); // Extend the scope since we are now interpreting the functions body

      if (exp->access != nullptr) {
        return AccessIndex(returnVal, exp->access, scope);
      }

      if (exp->dotOp != nullptr) {
        return GetProp(returnVal, exp->dotOp, scope);
      }
      
      return returnVal;
    }

    Value* iReturn(Expression* exp, Scope* scope) {
      Value* returnVal = Evaluate(exp->scope, scope);
      returnVal->returned = true;

      return returnVal;
    }

    Value* iIf(Expression* exp, Scope* scope) {
      Value* condition = Evaluate(exp->condition, scope);

      if (condition->ToBool() || condition->ToInt()) {
        return Evaluate(exp->then, scope);
      }

      if (exp->els != nullptr)
        return Evaluate(exp->els, scope);
      
      return nullptr;
    }

    Value* iClass(Expression* exp, Scope* scope) {
      Class* clss = new Class(this, exp, scope);

      if (scope->Lookup(exp->value.getString()) != nullptr) {
        throw Error("Class already defined");
      }

      if (exp->parent.isNull())
        scope->Define(exp->value.getString(), clss); // Define it

      return clss; // return it
    }

    Value* iArray(Expression* exp, Scope* scope) {
      Value* val = new Value(ValueType::Object, "[Object array]", "Array");
      for (int i = 0; i < exp->block.size(); i++) {
        val->Define(std::to_string(i), Evaluate(exp->block[i], scope));
      }

      return val;
    }

    Value* iFor(Expression* exp, Scope* scope) {
      Value* variable = nullptr;
      Value* condition = new Value(true, "boolean");
      Value* reassign = nullptr;
      Scope* newScope = scope->Extend();

      if (exp->assign != nullptr) {
        variable = Evaluate(exp->assign, newScope);
        if (exp->assign->type == ExprTypes::Variable) {
          variable = newScope->Define(exp->assign->value.getString(), new Value(0, "number"));
        }
      }
      if (exp->condition != nullptr) {
        condition = Evaluate(exp->condition, newScope);
      }

      Expression* forScope = exp->scope;

      Value* returnValue = nullptr;

      std::vector<Expression*> block = forScope->block;

      while (condition->ToBool() || condition->ToInt()) {
        if (exp->condition != nullptr) {
          condition = Evaluate(exp->condition, newScope);
          if (!(condition->ToBool() || condition->ToInt())) break;
        }

        for (Expression* expr : block) {
          returnValue = Evaluate(expr, newScope);

          if (scope != topScope && returnValue != nullptr && returnValue->returned)
            return returnValue;
        }

        if (exp->reassign != nullptr) {
          reassign = Evaluate(exp->reassign, newScope);
        }
      }

      if (returnValue == nullptr) return new Value();

      return returnValue;
    }

    // Main evaluator / visitor
    // I realize this now I probably have lots of memory leaks
    Value* Evaluate(Expression* exp, Scope* scope) {
      switch(exp->type) { // Huge switch statement for garbage (interpreting expressions)
        case ExprTypes::Scope:
          return iScope(exp, scope);

        case ExprTypes::None:  // TBI
          return new Value();

        case ExprTypes::While:
        case ExprTypes::For:
          return iFor(exp, scope);

        case ExprTypes::If:
          return iIf(exp, scope);

        case ExprTypes::Int: {
          Value* val = new Value(exp->value.getInt(), exp->dataType);
          return val;
        }

        case ExprTypes::Float:
          break;

        case ExprTypes::Boolean:
          return new Value(exp->value.getString() == "true", exp->dataType);

        case ExprTypes::String: {
          Value* val = new Value(exp->value.getString(), exp->dataType);
          return val;
        }

        case ExprTypes::Variable:
          return iVariable(exp, scope);

        case ExprTypes::Identifier:
          return iIdentifier(exp, scope);
          
        case ExprTypes::Assign:
          return iAssign(exp, scope);

        case ExprTypes::Binary:
          return iBinary(exp, scope);

        case ExprTypes::FunctionCall:
          return iFunctionCall(exp, scope);
        
        case ExprTypes::Function:
          return iFunction(exp, scope);

        case ExprTypes::Class:
          return iClass(exp, scope);

        case ExprTypes::Array:
          return iArray(exp, scope);
        
        case ExprTypes::FunctionDecl: // TBI
          break;

        case ExprTypes::Return:
          return iReturn(exp, scope);

        case ExprTypes::Datatype:
          break;

        default:
          return new Value();
      }

      return new Value();
    }

    Value* RawInterp(std::string r) {
      Lexer lexer = Lexer(r);
      std::vector<Token> tokens = lexer.tokenize("stdio");

      Parser parser = Parser(tokens);
      ast = parser.parse();

      return Evaluate(ast, topScope);
    }

    Value* Interpret(std::string file, bool debug = false) {
      std::string input = readFile(file);
      Lexer lexer = Lexer(input);
      std::vector<Token> tokens = lexer.tokenize(file);

      if (debug)
        for (auto token : tokens)
          token.debugPrint();

      Parser parser = Parser(tokens);
      ast = parser.parse();

      topScope->SetFile(file);

      return Evaluate(ast, topScope);
    };
  };

  Value* Class::Construct(std::vector<Expression*> args, Scope* scp) {
    Scope* newScope = scp->Extend();
    Value* obj = new Value(ValueType::Object, &explicitType, "object");

    for (Expression* exp : instructions) {
      if (
        exp->type != ExprTypes::Function &&
        exp->type != ExprTypes::Variable &&
        exp->type != ExprTypes::Assign
      ) throw Error("Can only have functions and variables in classes");

      if (exp->type == ExprTypes::Function) {
        Function* func = new Function(interpreter, exp, scope); // Creates a new function with the interpreter

        // Check if it is already defined, if so throw an error.
        if (obj->Get(exp->value.getString())->GetType() != ValueType::Nothing) {
          throw Error("Method already defined");
        }

        if (exp->value.getString() == name)
          func->explicitType = name;

        func->parent = obj;
        obj->Define(exp->value.getString(), func); // Define it
      }

      if (exp->type == ExprTypes::Assign) {
        Value* left = interpreter->Evaluate(exp->left, newScope); // Visit / interpret left branch
        Value* right = interpreter->Evaluate(exp->right, newScope); // Visit / interpret right branch
        Value* val = interpreter->iIdentifier(exp->left, newScope); // Just checks to see if it exists
        right->parent = obj;

        obj->Define(exp->left, right); // Define the property
      }

      if (exp->type == ExprTypes::Variable) {
        Value* value = new Value();
        value->SetExplicit(exp->dataType);

        obj->Define(
          exp->value.getString(),
          value
        );
      }
    }

    newScope->ClassScope = true;
    obj->SetScope(newScope);
    newScope->Define("this", obj);

    Function* constructor = (Function*)obj->Get(name);
    // interpreter->Evaluate(exp->scope, newScope);

    Value* returnValue = constructor->Call(args, newScope, false);

    obj->explicitType = name;

    return obj;
  }

  Value* Function::Call(std::vector<Expression*> args, Scope* propScope, Scope* scp, bool checkType) {
    // exp, scope, argsDefs

    std::vector<Value*> vals = {};

    for (Expression* exp : args) {
      Value* arg = interpreter->Evaluate(exp, scp);
      vals.push_back(arg);
    }

    if (function != nullptr) {
      return this->function(vals, propScope->filepath); // Call native function with arguments (vals)
    }

    Scope* newScope = propScope->Extend();
    newScope->ClassScope = propScope->ClassScope;
    
    for (int i = 0; i < argsDefs.size(); i++) {
      newScope->Define(argsDefs[i], new Value());

      if (argsDefs[i]->type == ExprTypes::Assign) {
        Expression* exp = argsDefs[i];
        Value* right = interpreter->Evaluate(exp->right, newScope);

        newScope->Define(exp->left, right);

        if (vals[i]->explicitType == "nothing") continue;
      }

      if (i >= vals.size()) {
        continue;
      };
      
      if (argsDefs[i]->type == ExprTypes::Assign)
        newScope->Define(argsDefs[i]->left, vals[i]);
      else
        newScope->Define(argsDefs[i], vals[i]);
    }

    Value* returnValue = nullptr;

    std::vector<Expression*> block = exp->scope->block;

    for (Expression* expr : block) {
      returnValue = interpreter->Evaluate(expr, newScope);

      if (newScope != interpreter->topScope && returnValue != nullptr && returnValue->returned)
        break;
    }

    if (checkType) {
      bool typeCheck = interpreter->checkType(exp, returnValue->explicitType);

      if (!typeCheck) throw Error("Incorrect type returned.");
    }

    return returnValue;
  }

  Value* Function::Call(std::vector<Expression*> args, Scope* scp, bool checkType) {
    // exp, scope, argsDefs

    std::vector<Value*> vals = {};

    for (Expression* exp : args) {
      Value* arg = interpreter->Evaluate(exp, scp);
      vals.push_back(arg);
    }

    if (function != nullptr) {
      return this->function(vals, scp->filepath); // Call native function with arguments (vals)
    }

    Scope* newScope = scp->Extend();
    newScope->ClassScope = scp->ClassScope;
    
    for (int i = 0; i < argsDefs.size(); i++) {
      newScope->Define(argsDefs[i], new Value());

      if (argsDefs[i]->type == ExprTypes::Assign) {
        Expression* exp = argsDefs[i];
        Value* right = interpreter->Evaluate(exp->right, newScope);

        newScope->Define(exp->left, right);

        if (vals[i]->explicitType == "nothing") continue;
      }

      if (i >= vals.size()) {
        continue;
      };
      
      if (argsDefs[i]->type == ExprTypes::Assign)
        newScope->Define(argsDefs[i]->left, vals[i]);
      else
        newScope->Define(argsDefs[i], vals[i]);
    }

    Value* returnValue = interpreter->Evaluate(exp->scope, newScope);
    if (checkType) {
      bool typeCheck = interpreter->checkType(exp, returnValue->explicitType);

      if (!typeCheck) throw Error("Incorrect type returned.");
    }

    return returnValue;
  }
}; // namespace Impala