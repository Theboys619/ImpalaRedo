#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include "errors.hpp"

namespace Impala {
	std::unordered_map<std::string, std::string> keywords = {
		{ "make", "Keyword" },
		{ "send", "Keyword" },
		{ "oftype", "Keyword" },
		{ "function", "typeKW" },
		{ "number", "typeKW" },
		{ "boolean", "typeKW" },
		{ "string", "typeKW" },
		{ "nothing", "typeKW" },
		{ "any", "typeKW" },
    { "class", "Keyword" },
		{ "if", "Keyword" },
		{ "else", "Keyword" },
		{ "import", "Keyword" },
		{ "from", "Keyword" },
		{ "as", "Keyword" },
    { "true", "Keyword" },
    { "false", "Keyword" },
    { "loop", "Keyword" },
	};
	
	class Token {
		void*	value;

		public:
		std::string type;
    std::string file;

		int line = 0;
		int index = 0;
		
		Token() {
			type = "Null";
			value = nullptr;
		};

		Token(std::string type, const char* val): type(type) {
			value = new std::string(val);
		};
		Token(std::string type, char val): type(type) {
			value = new std::string(1, val);
		}
		Token(std::string type, int val): type(type) {
			value = new int(val);
		}
		Token(std::string val, bool isIdentifier = false) {
			type = isIdentifier ? "Identifier" : "String";

			value = new std::string(val);
		}
		Token(std::string type, void* value): type(type), value(value) {};

    void SetFile(std::string f) {
      file = f;
    }

    std::string getFile() {
      return file;
    }

		std::string getString() {
			return *(std::string*)value;
		}

		int getInt(bool cast = true) {
			if (cast)
				return std::stoi(getString());

			return *(int*)value;
		}

		bool isNull() {
			return type == "Null";
		}

		void debugPrint() {
			std::string text = "Token<'" + type + "', '" + getString() + "'>";
			std::cout << text << std::endl;
		}
	};
	
	class Lexer {
		public:
		std::string input;
		int length;

		std::vector<Token> tokens;

		int pos = 0;
		int line = 1;
		int index = 1;

		char curChar;

		Lexer(std::string input): input(input) {
			length = input.length() - 1;
		}

		char advance(int amt = 1) {
			pos += amt;
			index += amt;

			if (pos > length) return curChar = '\0'; // \0 = null byte

			return curChar = input[pos]; // set the current char and return it
		}

		char peek(int amt = 1) {
			if (pos + amt > length) return '\0';

			return input[pos + amt];
		}

		bool isWhitespace(char c) {
			return c == ' ' || c == '\r' || c == '\t';
		}

		bool isAlpha(char c) {
			return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
		}

		bool isDigit(char c) {
			return c >= '0' && c <= '9';
		}

		bool isNumber(char c) {
			return (
				(c == '-' && isDigit(peek()))
				|| isDigit(c)
			);
		}

		bool isQuote(char c) { return c == '\'' || c == '\"'; };
		bool isQuote(char c, char quote) {
			if (quote != '"' && quote != '\'') return false;
			if (quote == '"')
				return c == '"';
			else if (quote == '\'')
				return c == '\'';
		}

		bool isDelimiter(char c) {
			return (
				(c == '(') ||
				(c == ')') ||
				(c == '{') ||
				(c == '}') ||
				(c == '[') ||
				(c == ']') ||
				(c == ';') ||
				(c == ':') ||
				(c == ',') ||
				(c == '.')
			);
		}

		int isOperator(char c) {
			if (
				(c == '=' && peek() == '=') ||
				(c == '!' && peek() == '=') ||
				(c == '&' && peek() == '&') ||
				(c == '|' && peek() == '|') ||
        (c == '<' && peek() == '=') ||
        (c == '>' && peek() == '=')
			) return 2;
			else if (
				(c == '+') ||
				(c == '-') ||
				(c == '*') ||
				(c == '/') ||
				(c == '%') ||
				(c == '=') ||
        (c == '<') ||
        (c == '>')
			) return 1;
			else return 0;
		}

		bool isComment(char c) {
			return c == '$';
		}

		std::vector<Token> tokenize(std::string file = "unknown") {
			curChar = input[0];

			while (curChar != '\0') {
				int lastPos = pos;

				if (isWhitespace(curChar))
					advance();

        if (isComment(curChar)) {
					advance();

					while (curChar != '\0' && curChar != '\n')
						advance();

				}

				if (curChar == '\n') {
					index = 0;
					++line;

					advance();

					//tokens.push_back(Token("Linebreak", "\n"));
				}

				if (isDelimiter(curChar)) {
          Token tok = Token("Delimiter", curChar);
          tok.index = index;
          tok.line = line;
          tok.SetFile(file);

          advance();

          tokens.push_back(tok);
        }

        if (isNumber(curChar)) {
          std::string type = "Int";
          int ind = index;
          int ln = line;

          std::string val = "";

          if (curChar == '-') {
            val += curChar;
            advance();
          }

          while (isNumber(curChar)) {
            val += curChar;
            advance();

            if (curChar == '.') {
              type = "Float";
              val += ".";

              advance();
            }
          }

          Token tok = Token(type, val.c_str());
          tok.index = ind;
          tok.line = ln;
          tok.SetFile(file);

          tokens.push_back(tok);
        }

				if (isOperator(curChar) > 0) {
          int amt = isOperator(curChar);
          std::string op = std::string(1, curChar);
          int ind = index;
          int ln = line;

          for (int i = 0; i < amt - 1; i++) {
            op += advance();
          }

          advance();

          Token tok = Token("Operator", op.c_str());
          tok.index = ind;
          tok.line = ln;
          tok.SetFile(file);

          tokens.push_back(tok);
        }

				if (isQuote(curChar)) {
					char quote = curChar;
					int ind = index;
					int ln = line;

					std::string val = "";
					advance();

					while (curChar != '\0' && curChar != quote) {
						if (curChar == '\n') throw SyntaxError("\\n");
						val += curChar;
						advance();
					}

					advance();

					Token tok = Token(val);
					tok.index = ind;
					tok.line = ln;
          tok.SetFile(file);

					tokens.push_back(tok);
				}

				if (isAlpha(curChar)) {
          std::string val = "";

          int ind = index;
          int ln = line;

          while (curChar != '\0' && isAlpha(curChar)) {
            val += curChar;
            advance();
          }

          std::string type = keywords.find(val) != keywords.end()
            ? keywords[val]
            : "Identifier";

          Token tok = Token(type, val.c_str());
          tok.index = ind;
          tok.line = ln;
          tok.SetFile(file);

          tokens.push_back(tok);
        }

        if (lastPos == pos) {
          throw SyntaxError(curChar);
        }
			}

      Token EOFT = Token("EndOfFile", "EOF");
      EOFT.SetFile(file);
			tokens.push_back(EOFT);

      return tokens;
		}
	};
};

