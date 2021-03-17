#pragma once
#include <algorithm>
#include <map>
#include "lexer.hpp"

namespace Impala {
	std::map<std::string, int> PRECEDENCE = {
		{ "=", 1 },

		{ "&&", 4 },
		{ "||", 5 },

		{ "<", 7 }, { ">", 7 }, { "<=", 7 }, { ">=", 7 }, { "==", 7 }, { "!=", 7 },

		{ "+", 10 }, { "-", 10 },
		{ "*", 20 }, { "/", 20 }, { "%", 20 }
	};

	template<typename T>
	bool vincludes(std::vector<T> arr, T value) {
		return std::find(arr.begin(), arr.end(), value) != arr.end();
	}

	enum class ExprTypes {
		None,
		While,
		For,
		If,
		Int,
		Float,
		String,
    Boolean,
    Class,
    Access,
    Array,
		Variable,
    Import,
		Identifier,
		Assign,
		Binary,
		Scope,
		FunctionCall,
		Function,
		FunctionDecl,
		Return,
		Datatype
	};

	std::vector<std::string> ExprStrings = {
		"None",
		"While",
		"For",
		"If",
		"Int",
		"Float",
		"String",
    "Boolean",
    "Class",
    "Access",
    "Array",
		"Variable",
    "Import",
		"Identifier",
		"Assign",
		"Binary",
		"Scope",
		"FunctionCall",
		"Function",
		"FunctionDecl",
		"Return",
		"Datatype"
	};

	std::string getExprString(ExprTypes type) {
		return ExprStrings[((int)type)];
	}

	class Expression {
    public:
    ExprTypes type;
    Token value;

    union {
      Expression* left;
      Expression* assign;
    };

    Token op;
    
    union {
      Expression* right;
      Expression* reassign;
    };

    bool isArray;
    Expression* then;
    Expression* els;
    Expression* condition;
    Expression* scope;
    std::vector<Expression*> block;
    std::vector<Expression*> args;

    Token parent;
		Token identifier;
		std::string dataType;
    Expression* dotOp = nullptr;
    Expression* access = nullptr;

    Expression(Token value): type(ExprTypes::None), value(value) {};
    Expression(ExprTypes type, Token value): type(type), value(value) {};
  };

  Expression* CreateString(std::string value) {
    Token tokStr = Token(value);
    Expression* str = new Expression(ExprTypes::String, tokStr);
    str->dotOp = nullptr;
    str->dataType = "string";

    return str;
  }

  Expression* CreateIdentifier(std::string value, std::string dataType = "any") {
    Token tokStr = Token(value, true);
    Expression* identifier = new Expression(ExprTypes::Identifier, tokStr);
    identifier->access = nullptr;
    identifier->dotOp = nullptr;
    identifier->dataType = dataType;

    return identifier;
  }

	class Parser {
		std::vector<Token> tokens;
		int pos = 0;

		Token curTok;

		Expression* ast;

		public:

		Parser(std::vector<Token> tokens): tokens(tokens) {};

		Token advance(int amt = 1) {
			if (pos + amt >= tokens.size()) return curTok = Token();
			pos += amt;

			return curTok = tokens[pos];
		}

		Token peek(int amt = 1) {
			if (pos + amt >= tokens.size()) return Token();

			return tokens[pos + amt];
		}

		bool isType(std::string type, std::string value, Token peeked) {
      return peeked.type == type && peeked.getString() == value;
    }
    bool isType(std::string type, Token peeked) {
      return peeked.type == type;
    }
    bool isType(std::string type, std::string value) {
      return curTok.type == type && curTok.getString() == value;
    }
    bool isType(std::string type) {
      return curTok.type == type;
    }

    bool isIgnore(Token tok) {
      return tok.type == "Delimiter" && tok.getString() == ";";
    }

    bool isEOF() {
      if (curTok.isNull()) return true;

      return curTok.type == "EndOfFile";
    }

    bool isSkipable(Expression* expr) {
      if (expr == nullptr) return false;

      ExprTypes type = expr->type;

      return (
        type == ExprTypes::Function ||
        type == ExprTypes::If ||
        type == ExprTypes::Class ||
        type == ExprTypes::For
      );
    }

		void skipOverVal(std::string val, Token tok, Expression* expr = nullptr) {
      bool skipable = isSkipable(expr);

      if (!skipable && tok.getString() != val) throw SyntaxError(tok.getString());

      if (!skipable)
        advance();
    }

    void skipOver(std::string type, std::string val, Expression* expr = nullptr) {
      bool skipable = isSkipable(expr);

      if (!skipable && curTok.type != type || curTok.getString() != val) throw SyntaxError(curTok.getString());

      if (!skipable)
        advance();
    }
    void skipOver(std::string type, Expression* expr = nullptr) {
      bool skipable = isSkipable(expr);

      if (!skipable && curTok.type != type) throw SyntaxError(curTok.getString());

      if (!skipable)
        advance();
    }

    void skipOver(std::string type, Token tok, Expression* expr = nullptr) {
      bool skipable = isSkipable(expr);
      
      if (!skipable && tok.type != type) throw SyntaxError(tok.getString());

      if (!skipable)
        advance();
    }
    void skipOver(std::string type, std::string val, Token tok, Expression* expr = nullptr) {
      bool skipable = isSkipable(expr);

      if (!skipable && tok.type != type || tok.getString() != val) throw SyntaxError(tok.getString());

      if (!skipable)
        advance();
    }

		bool nonCallabes(Expression* expression) {
      return (
        expression->type != ExprTypes::Function
        && expression->type != ExprTypes::If
        && expression->type != ExprTypes::Return
      );
    }

		// Parse delimtiers from start to end with expressions in a vector
    std::vector<Expression*> pDelimiters(std::string start, std::string end, std::string separator) {
      std::vector<Expression*> values = {};
      bool isFirst = true;
      bool skipable = false;

      skipOverVal(start, curTok);

      while (!isEOF()) {
        if (isType("Delimiter", end)) {
          break;
        } else if (isFirst) {
          isFirst = false;
        } else {
          if (!skipable)
            skipOverVal(separator, curTok);
        }

        Expression* val = pExpression();
        values.push_back(val);
        skipable = isSkipable(val);
      }
      skipOverVal(end, curTok);

      return values;
    }

		// Parse any prop / dot operations
		// probably should change how this works
		bool pDotOp(Expression* exp) {
			if (isType("Delimiter", ".")) {
        advance();

				if (!isType("Identifier"))
					throw SyntaxError(curTok.getString()); // Meaningful errors soon

				exp->identifier = curTok;
        exp->dotOp = pExpression();
        exp->dotOp->parent = exp->value.getString();
        return true;
      }

      return false;
		}

    bool pIndexAccess(Expression* exp) {
      if (isType("Delimiter", "[")) {
        advance();

        if (!isType("Int") && !isType("String") && !isType("Identifier"))
          throw SyntaxError(curTok.getString());

        exp->access = pExpression();

        if (isType("Delimiter", "]", peek()))
          advance();

        if (!isType("Delimiter", "]"))
          throw SyntaxError(curTok.getString());

        advance();

        pIndexAccess(exp->access);

        return true;
      }

      exp->access = nullptr;

      return false;
    }

		Expression* isCall(Expression* expression) {
      // Probably could change
			return isType("Delimiter", "(", peek()) && nonCallabes(expression) ? pCall(expression) : expression;
    }

    Expression* pCall(Expression* expr) {
      Expression* funcCall = new Expression(ExprTypes::FunctionCall, expr->value.getString());
      funcCall->dotOp = nullptr;

      advance();

      funcCall->args = pDelimiters("(", ")", ",");
      
      pIndexAccess(funcCall);
      pDotOp(funcCall);

      return funcCall;
    }

		Expression* pBinary(Expression* left, int prec) {
			// Parse basic operations like 4 + 3. Or chaining function calls and other things
			// TODO
			Token op = curTok;

			if (isType("Operator")) {
				std::string opvalue = op.getString();
				int newPrec = PRECEDENCE[opvalue];

				if (newPrec > prec) {
					advance();

					std::vector<std::string> assignments = {
						"=",
						"+="
					};

					ExprTypes type = vincludes<std::string>(assignments, opvalue)
						? ExprTypes::Assign
						: ExprTypes::Binary;
					Expression* expr = new Expression(type, curTok);
					expr->left = left;
					expr->op = op;
					expr->right = pBinary(isCall(pAll()), newPrec);
          expr->dataType = expr->right->dataType;

					return pBinary(expr, prec);
				}
			}
			
			return left;
		}

		// Send is equivalent to return
		Expression* pSend() {
			advance(); // Skip over keyword

			Expression* expr = new Expression(ExprTypes::Return, curTok);
			expr->scope = pExpression();

			return expr;
		}

		std::string pDatatype() {
			if (curTok.getString() != "oftype") return "any";
			advance(); // Skip over oftype keyword

			// if (!isType("typeKW")) throw SyntaxError(curTok.getString());

			std::string type = curTok.getString();
      advance();

			// Might do other stuff

			return type;
		}

		Expression* pIdentifier(Expression* expr) {
			expr->type = ExprTypes::Identifier;
			expr->dotOp = nullptr; // Should change eventually
			expr->dataType = "any";

			if (!isType("Delimiter", "(", peek()))
				advance();

      pIndexAccess(expr);

			// Could / should change
			if (!pDotOp(expr)) {
				expr->dataType = pDatatype();
			}

			return expr;
		}

		Expression* pFunction() {
			// curTok should be the Identifier
			if (!isType("Identifier")) throw SyntaxError(curTok.getString());

			Expression* func = new Expression(ExprTypes::Function, curTok);
      advance();

			func->args = pDelimiters("(", ")", ",");
			func->scope = new Expression(ExprTypes::Scope, curTok);

      if (curTok.getString() == "oftype")
        func->dataType = pDatatype();
      else
        func->dataType = "any";

			func->scope->block = pDelimiters("{", "}", ";");

			// TODO function types and return types

			return func;
		}

		Expression* pMake() {
			advance(); // advance over make keyword

			if (isType("Delimiter", "(", peek()))
				return pFunction();

			Expression* identifier = pIdentifier(new Expression(curTok));
			identifier->type = ExprTypes::Variable;

			return identifier;
		}

    Expression* pSmallIf(Expression* condition) {
      Expression* ifStmt = new Expression(ExprTypes::If, curTok);
      ifStmt->dataType = "any";

      Expression* then = pExpression();
      Expression* els = nullptr;

      if (curTok.getString() == ";" && isType("Keyword", "else", peek())) advance();

      if (isType("Keyword", "else")) {
        advance();

        if (isType("Delimiter", ":")) {
          advance();
          els = pExpression();
        } else if (isType("Keyword", "if"))
          els = pIf();
        else {
          if (!isType("Delimiter", "{"))
            els = pExpression();
          else {
            els = new Expression(ExprTypes::Scope, curTok);
            els->block = pDelimiters("{", "}", ";");
          }
        }
      }

      ifStmt->then = then;
      ifStmt->condition = condition;
      ifStmt->els = els;

      return ifStmt;
    }

    Expression* pIf() {
      advance();

      Expression* ifStmt = new Expression(ExprTypes::If, curTok);
      ifStmt->dataType = "any";

      Expression* condition = pExpression();
      condition->dataType = "boolean";

      if (isType("Delimiter", ":")) {
        advance();
        return pSmallIf(condition);
      }


      Expression* then = nullptr;
      Expression* els = nullptr;

      if (!isType("Delimiter", "{"))
        then = pExpression();
      else {
        then = new Expression(ExprTypes::Scope, curTok);
        then->block = pDelimiters("{", "}", ";");
      }

      if (isType("Keyword", "else")) {
        advance();

        if (isType("Delimiter", ":")) {
          advance();
          els = pExpression();
        } else if (isType("Keyword", "if"))
          els = pIf();
        else {
          if (!isType("Delimiter", "{"))
            els = pExpression();
          else {
            els = new Expression(ExprTypes::Scope, curTok);
            els->block = pDelimiters("{", "}", ";");
          }
        }
      }

      ifStmt->then = then;
      ifStmt->condition = condition;
      ifStmt->els = els;

      return ifStmt;
    }

    Expression* pClass() {
      advance();

      Expression* clss = new Expression(ExprTypes::Class, curTok);
      advance();

      std::vector<Expression*> instructions = pDelimiters("{", "}", ";");

      clss->scope = new Expression(ExprTypes::Scope, curTok);
      clss->scope->block = instructions;
      clss->access = nullptr;

      return clss;
    }

    Expression* pArray() {
      Expression* arr = new Expression(ExprTypes::Array, curTok);
      std::vector<Expression*> values = pDelimiters("[", "]", ",");
      arr->block = values;
      arr->dataType = "Array";

      pDotOp(arr);

      return arr;
    }

    Expression* pLoop() {
      advance();
      Expression* loop = new Expression(ExprTypes::For, curTok);

      Expression* initial = nullptr;
      Expression* condition = nullptr;
      Expression* reassign = nullptr;
      
      skipOverVal("(", curTok);

      if (isType("Delimiter", ";"))
        advance();
      else {
        initial = pExpression();
        if (isType("Delimiter", ";")) advance();
      }

      if (isType("Delimiter", ";"))
        advance();
      else {
        condition = pExpression();
        if (isType("Delimiter", ";")) advance();
      }

      if (isType("Delimiter", ";"))
        advance();
      else {
        reassign = pExpression();
      }

      skipOverVal(")", curTok);
      
      loop->scope = new Expression(ExprTypes::Scope, curTok);
      loop->scope->block = pDelimiters("{", "}", ";");
      loop->assign = initial;
      loop->condition = condition;
      loop->reassign = reassign;

      return loop;
    }

    Expression* pImport() {
      advance();
      Expression* identifier = nullptr;

      if (isType("Identifier")) {
        identifier = pIdentifier(new Expression(curTok));
        skipOverVal("from", curTok);
      }

      Expression* imprt = new Expression(ExprTypes::Import, curTok);
      imprt->assign = identifier;
      imprt->access = nullptr;

      advance();

      return imprt;
    }

		Expression* pAll() {
			// Parse everything else
			// TODO

			if (isType("Delimiter", "(")) {
				advance();
				Expression* expr = pExpression();
				skipOver("Delimiter", ")");
				return expr;
			}

			Expression* token = new Expression(curTok);

			if (isType("Keyword", "send"))
				return pSend();

			if (isType("Keyword", "make"))
				return pMake();

      if (isType("Keyword", "if"))
        return pIf();

      if (isType("Keyword", "class"))
        return pClass();

      if (isType("Keyword", "loop"))
        return pLoop();

      if (isType("Keyword", "import"))
        return pImport();

      if (isType("true") || isType("false")) {
        advance();
        token->type = ExprTypes::Boolean;
        token->dataType = "boolean";

        return token;
      }

      if (isType("Delimiter", "["))
        return pArray();

			if (isType("Int")) {
				advance();
        token->type = ExprTypes::Int;
        token->dataType = "number";

				return token;
			} else if (isType("Float")) {
				advance();
        token->type = ExprTypes::Float;
        token->dataType = "number";

				return token;
			} else if (isType("String")) {
				advance();
        token->type = ExprTypes::String;
        token->dataType = "string";

				return token;
			} else if (isType("Identifier")) {
				return pIdentifier(token);
			}
		}

		Expression* pExpression() {
			return isCall(pBinary(isCall(pAll()), 0));
		}

		Expression* parse() {
			ast = new Expression(ExprTypes::Scope, Token("_TOP_", true));
			curTok = tokens[0];

			while (!curTok.isNull() && !isEOF()) {
				Expression* expr = pExpression();
				ast->block.push_back(expr);

				if (!curTok.isNull() && !isEOF()) {
          bool skipable = isSkipable(expr);
          if (!skipable)
            skipOver("Delimiter", ";");
        }
			}

			return ast;
		}
	};
};