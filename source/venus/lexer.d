module venus.lexer;
import std.range;
import std.stdio;
import std.conv;
import std.uni;
import std.utf;
import std.ascii;

import venus.context;

unittest {
    Context ctx = new Context();
    string code = q{
        // one-line comment
        /*
         * This is multiline comment and should be ignored by the compiler
         */
        import std.io;

        // extern function definition
        extern sin(d double) double;

        // short function definition
        fun add(a int, b int) int = a + b

        // full function definition
        fun times(c double, d double) double {
            return c * d
        }

        // void function
        fun doSomething() void {
            a = 1 + 1
        }

        // function as parameter
        fun apply(f fun[int, int -> double]) void {
            f(1, 2)
        }

        // extention function
        fun string.reverse() string {
            string(self.data.reverse)
        }

        // script block
        script { println("hello") }

        // dynamic block
        dynamic {
            var x = 1;
            println(sin(x));
        }

        // static block
        static { int MAX_INT = int.MAX; }

        // main block
        main {
            println("hello");

            var a = 1.1;
            val c = 'a';
            1 + 2;
            2 - 3;
            4 * 5;
            6 / 7;

            if (a > 1) {
                println("a is greater");
            } else
                println("a is less");

            val arr = [1, 2, 3, 4, 5];
            for n in arr {
                println(n);
            }
            var i = 0;
            while (i < 5) {
                println(i);
                i = i + 1;
            }
        }

    };
    auto lexer = lex(code, ctx);
    foreach (tok; lexer) {
        writeln(ctx.getTokenString(tok));
    }
    writeln("testing lexer end.");
}

alias isUniAlpha = std.uni.isAlpha;
alias isPunct = std.ascii.isPunctuation;

struct Lexer(R) {

    Token t;
    R r;
    Context context;

    uint line = 1;
    uint index = 0;

    @property auto front() inout {
        return t;
    }

    void popFront() {
        t = getNextToken();
    }

    @property auto save() inout {
        return inout(Lexer)(t, r, context, line, index);
    }

    @property bool empty() const {
        return t.type == TokenType.Fin;
    }

private:
    auto getNextToken() {
        while (1) {
            if (r.empty()) {
                if (t.type == TokenType.End) {
                    Token t;
                    t.type = TokenType.Fin;
                    return t;
                } else {
                    return lexEnd();
                }
            }
            dchar f = r.front;
            switch (f) {
                case '\0':
                    return lexEnd();
                case ' ', '\t', '\v', '\f':
                    popChar();
                    continue;
                case '\n', '\r', ';':
                    return lexLineSep();
                case '/':
                    popChar();
                    switch (r.front) {
                        case '/', '*':
                            lexComment(f);
                            continue;
                        default: // '/' operator
                            return lexOperator(f);
                    }
                case '0': .. case '9':
                    return lexNumeric();
                case '"' :
                    return lexString();
                case '\'':
                    return lexChar();
                case '+', '-', '*', '=', '.', '{', '}', '(', ')', ',', '<', '>', '!', '[', ']', ':': 
                    // TODO: use operator map here. What if D has pattern matching...
                    popChar();
                    return lexOperator(f);
                default:
                    return lexIdentifier();
            }
        }
    }
    
    void popChar() {
        r.popFront();
        ++index;
    }
    
    auto nextChar() {
        r.popFront();
        ++index;
        if (r.empty()) 
            return EOF;
        return r.front;
    }
    
private: // lex parts
    Token lexLineSep() {
        auto c = r.front;
        Token tok;
        setLoc(tok);
        tok.type = TokenType.LineSep;
        string name;
        if (c == ';') {
            name = ";";
            c = nextChar();
        }
        if (c == '\n') {
            name ~= "\\n";
            ++line;
            popChar();
        }
        else if (c == '\r') {
            c = nextChar();
            if (c == '\n') {
                popChar();
                name ~= "\\r\\n";
            } else {
                name ~= "\\r";
            }
            ++line;
        } else if (c == ';') { // ';'
            name = ";";
            c = nextChar();
        }
        tok.name = context.getName(name);
        return tok;
    }   
    

    void setLoc(ref Token tok) {
        tok.loc.line = line;
        tok.loc.index = index;
    }
    
    Token lexNumeric() {
        Token tok;
        tok.type = TokenType.IntLiteral;
        setLoc(tok);
        string name;
        auto c = r.front;
        while (c.isDigit || c == '.') {
            name ~= c;
            c = nextChar();
        }
        tok.name = context.getName(name);

        return tok;
    }
    
    Token lexString() {
        Token tok;
        setLoc(tok);
        string name;
        popChar(); // '"'
        auto c = r.front;
        while (c != '"') {
            name ~= c;
            c = nextChar();
        }
        popChar(); // '"'
        
        tok.type = TokenType.StringLiteral;
        tok.name = context.getName(name);
        return tok;
    }

    Token lexChar() {
        Token tok;
        setLoc(tok);
        popChar(); // '\''
        auto c = r.front;
        string name;
        while (c != '\'') {
            name ~= c;
            c = nextChar();
        }
        popChar(); // '\'
        tok.type = TokenType.CharLiteral;
        tok.name = context.getName(name);
        return tok;
    }

    Token lexIdentifier() {
        
        auto c = r.front;
        if (!c.isIdChar) {
            writeln("lex error at:", r);
            assert(0, "Wrong identifier!");
        }
        Token tok;
        setLoc(tok);
        string name;
        while (c.isIdChar) {
            name ~= c;
            c = nextChar();
        }

        tok.name = context.getName(name);
        if (auto tokType = name in KeywordsMap) {
            tok.type = *tokType;
        } else {
            tok.type = TokenType.Identifier;
        }

        return tok;
    }
    
    Token lexEnd() {
        Token tok;
        setLoc(tok);
        tok.type = TokenType.End;
        return tok;
    }
    
    
    // TODO: argument 'dchar f' here is unintuitive....
    Token lexOperator(dchar f) {
        Token tok;

        setLoc(tok);
        string name;
        auto c = f;
        if (r.empty()) {
            name ~= c;
            if (auto tokType = name in OperatorsMap) {
                tok.type = *tokType;
            } else { // if two chars not match, fall back to one char operator
                writeln("unknown token:", name);
                assert(0, "Lex Error");
            }
        } else { // if there are more chars, we need to check the possibility for two char operators like '<=' or '>='
            if (c.isOp) { 
                name ~= c;
                c = r.front;
            }
            // try with two chars for '<=', '>=', etc
            if (!r.empty() && r.front.isOp) {
                name ~= r.front;
            }
            if (auto tokType = name in OperatorsMap) {
                tok.type = *tokType;
                if (!r.empty() && r.front.isOp) c = nextChar();
            } else { // if two chars not match, fall back to one char operator
                name = name[0..$-1];
                if (auto tokType = name in OperatorsMap) {
                    tok.type = *tokType;
                } else {
                    writeln("unknown token:", name);
                    assert(0, "Lex Error");
                }
            }
        }

        tok.name = context.getName(name);
        return tok;
    }

    void lexComment(dchar head) {
        //popChar(); // pop the already checked '/'
        auto c = r.front;
        if (c == '/') { // one line comment
            while (c != '\n' && c != '\r') {
                c = nextChar();
            }
            
            popChar();
            if (c == '\r') {
                if (r.front == '\n') popChar();
            }
            ++line;
        } else if (c == '*') {
        Pump: while (1) {
                while (c != '*' && c != '\r' && c != '\n') {
                    c = nextChar();
                }
                
                auto match = c;
                c = nextChar();
                switch (match) {
                    case '*':
                        if (c == '/') {
                            popChar();
                            break Pump;
                        }
                        break;
                    case '\r':
                        if (c == '\n') {
                            c = nextChar();
                        }
                        
                        line++;
                        break;
                    case '\n':
                        line++;
                        break;
                    default:
                        assert(0, "Unreachable in lexComment.");
                }
            }
        }
    }

}


auto lex(R)(R r, Context ctx) if (isForwardRange!R) {

    auto lexer = Lexer!R();
    lexer.r = r.save;
    lexer.context = ctx;
    lexer.t.type = TokenType.Begin;

    return lexer;
}

auto isIdChar(C)(C c) {
    static if (is(C == char)) {
        return c == '_' || isAlphaNum(c);
    } else static if (is(C == wchar) || is(C == dchar)) {
        return isUniAlpha(c) || c == '_';
    } else {
        static assert(0, "This function is only for character types");
    }
}

/**
 * Check a char is an operator (and not string/char quotes)
 */
bool isOp(dchar c) {
    return c == '+' || c == '-' || c == '*' || c == '/' 
        || c == '.' || c == '=' || c == ',' || c == '!'
            || c == '<' || c == '>'
            || c == '{' || c == '}' || c == '(' || c == ')' || c == '[' || c == ']'
            || ':'
            ;
}


