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
		Variable,
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
		"Variable",
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

    Expression* left;
    Token op;
    Expression* right;

    bool isArray;
    Expression* then;
    Expression* els;
    Expression* condition;
    Expression* scope;
    std::vector<Expression*> block;
    std::vector<Expression*> args;

		Token identifier;
		std::string dataType;
    Expression* dotOp;

    Expression(Token value): type(ExprTypes::None), value(value) {};
    Expression(ExprTypes type, Token value): type(type), value(value) {};
  };

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

		void skipOverVal(std::string val, Token tok) {
      if (tok.getString() != val) throw SyntaxError(tok.getString());

      advance();
    }

    void skipOver(std::string type, std::string val) {
      if (curTok.type != type || curTok.getString() != val) throw SyntaxError(curTok.getString());

      advance();
    }
    void skipOver(std::string type) {
      if (curTok.type != type) throw SyntaxError(curTok.getString());

      advance();
    }

    void skipOver(std::string type, Token tok) {
      if (tok.type != type) throw SyntaxError(tok.getString());

      advance();
    }
    void skipOver(std::string type, std::string val, Token tok) {
      if (tok.type != type || tok.getString() != val) throw SyntaxError(tok.getString());

      advance();
    }

		bool nonCallabes(Expression* expression) {
      return (
        expression->type != ExprTypes::Function
        && expression->type != ExprTypes::If
      );
    }

		// Parse delimtiers from start to end with expressions in a vector
    std::vector<Expression*> pDelimiters(std::string start, std::string end, std::string separator) {
      std::vector<Expression*> values = {};
      bool isFirst = true;

      skipOverVal(start, curTok);

      while (!isEOF()) {
        if (isType("Delimiter", end)) {
          break;
        } else if (isFirst) {
          isFirst = false;
        } else {
          skipOverVal(separator, curTok);
        }

        Expression* val = pExpression();
        values.push_back(val);
      }
      skipOverVal(end, curTok);

      return values;
    }

		// Parse any prop / dot operations
		// probably should change how this works
		bool pDotOp(Expression* exp) {
			// TODO
			if (isType("Delimiter", ".")) {
        advance();

				if (!isType("Identifier"))
					throw SyntaxError(curTok.getString()); // Meaningful errors soon

				exp->identifier = curTok;
        exp->dotOp = pExpression();
        return true;
      }

      return false;
		}

		Expression* isCall(Expression* expression) {
      // Probably could change
			return isType("Delimiter", "(", peek()) && nonCallabes(expression) ? pCall(expression) : expression;
    }

    Expression* pCall(Expression* expr) {
      Expression* funcCall = new Expression(ExprTypes::FunctionCall, expr->value.getString());
      advance();

      funcCall->args = pDelimiters("(", ")", ",");


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

				if (!curTok.isNull() && !isEOF()) skipOver("Delimiter", ";");
			}

			return ast;
		}
	};
};