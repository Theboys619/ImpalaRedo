#pragma once
#include "parser.hpp"

namespace Impala {
  class Interpreter; // To be able to use Interpreter*s in classes defined before Interpreter.

  // Types for the Value Class to know where to cast and how to cast it
  // Have to use this because of void* since we don't know the type
  enum class ValueType {
    Nothing,
    String,
    Int,
    Double,
    Char,
    Function
  };

  // Class for values like objects, ints, and other types
  // Used for casting and doing dynamic things
  class Value {
    ValueType type;
    void* value;

    public:
    std::string explicitType;
    bool returned = false;

    Value() {
      type = ValueType::Nothing;
      value = nullptr;
      explicitType = "nothing";
    };

    Value(ValueType type, void* value, std::string explicitType = "any")
    : type(type),
      value(value),
      explicitType(explicitType) {};


    Value(int val, std::string explicitType = "any")
    : explicitType(explicitType)
    {
      type = ValueType::Int;
      value = new int(val);
    }
    Value(std::string val, std::string explicitType = "any"): explicitType(explicitType) {
      type = ValueType::String;
      value = new std::string(val);
    }

    std::string ToString() {
      if (type == ValueType::String) {
        return Cast<std::string>();
      } else if (type == ValueType::Int) {
        return std::to_string(Cast<int>());
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

    template<typename T>
    T Cast() {
      return T(*(T*)value);
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
	};

  // Where variables are stored and different data
  class Scope {
    public:
    Scope* parent;
    std::unordered_map<std::string, Value*> props = std::unordered_map<std::string, Value*>();

    Scope() {
      parent = nullptr;
    };
    Scope(Scope* parent): parent(parent) {}

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
        ) throw Error("Can't assign type " + prop->explicitType + " to variable of type " + type + ". Variable " + exp->value.getString() + ".");
        else if (
          prop->GetType() == ValueType::Function &&
          type != "function"
        ) throw Error ("Can't assign type Function to variable of type " + type + ". Variable " + exp->value.getString());
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
        ) throw Error("Can't assign type " + prop->explicitType + " to variable of type " + type + ". Variable " + exp->value.getString() + ".");
        else if (
          prop->GetType() == ValueType::Function &&
          type != "function"
        ) throw Error ("Can't assign type Function to variable of type " + type + ". Variable " + exp->value.getString());
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
  typedef Value* (*CFunction)(std::vector<Value*>);

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
    Value* Call(std::vector<Expression*> args, Scope* scp); // The Call Method
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

    Value* iIdentifier(Expression* exp, Scope* scope) {
      return scope->Get(exp->value.getString());
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
      Value* left = Evaluate(exp->left, scope); // Visit / interpret left branch
      Value* right = Evaluate(exp->right, scope); // Visit / interpret right branch
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

      if (func->GetType() != ValueType::Function) // If its not a pointer to the Function class throw an Error.
        throw Error("Cannot call function as it is not one.");

      // Cast to a Function* then call the method Call with appropiate arguments
      return ((Function*)func)->Call(exp->args, scope->Extend()); // Extend the scope since we are now interpreting the functions body
    }

    Value* iReturn(Expression* exp, Scope* scope) {
      Value* returnVal = Evaluate(exp->scope, scope);
      returnVal->returned = true;

      return returnVal;
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
        case ExprTypes::If:
          break;

        case ExprTypes::Int: {
          Value* val = new Value(exp->value.getInt(), exp->dataType);
          return val;
        }

        case ExprTypes::Float:
          break;
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

    Value* Interpret(std::string input, bool debug = false) {
      Lexer lexer = Lexer(input);
      std::vector<Token> tokens = lexer.tokenize();

      if (debug)
        for (auto token : tokens)
          token.debugPrint();

      Parser parser = Parser(tokens);
      ast = parser.parse();

      return Evaluate(ast, topScope);
    };
  };

  Value* Function::Call(std::vector<Expression*> args, Scope* scp) {
    // exp, scope, argsDefs

    std::vector<Value*> vals = {};

    for (Expression* exp : args) {
      Value* arg = interpreter->Evaluate(exp, scp);
      vals.push_back(arg);
    }

    if (function != nullptr) {
      return this->function(vals); // Call native function with arguments (vals)
    }

    // TODO - Impala Functions

    Scope* newScope = scp->Extend();
    
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

    bool typeCheck = interpreter->checkType(exp, returnValue->explicitType);

    if (!typeCheck) throw Error("Incorrect type returned.");

    return returnValue;
  }
}; // namespace Impala