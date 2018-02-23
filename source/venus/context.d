module venus.context;
import std.range;
import std.stdio;
import std.conv;
import std.uni;
import std.utf;
import std.ascii;
alias isUniAlpha = std.uni.isAlpha;
alias isPunct = std.ascii.isPunctuation;

struct Token {
    TokenType type;
    Name name;
    Location loc; // TODO: track location when lexing
}

enum TokenType {
    Invalid = 0, Begin, End, Fin,
    
    // Literals
    Identifier, StringLiteral, CharLiteral, IntLiteral, FloatLiteral,
    
    // Keywords

    // Object/Type
    Val, Var, Ptr, Ref, Void,
    
    // Builtin Type
    Bit, Byte, Char, Short, Int, Long, Float, Double,
    
    
    // Control flow
    Do, Else, If, While, In, Return, For,
    
    // Seperators
    BraceBegin, BraceEnd,
    ParenBegin, ParenEnd,
    SquareBegin, SquareEnd,
    
    // Operators
    Plus, // '+'
    Minus, // '-'
    Star, // '*'
    Slash, // '/'
    Dot, // '.'
    Assign, // "="
    Comma, // ','
    Greater, // '>'
    Less, // '<'
    Equal, // '=='
    NotEqual, // '!='
    Not, // '!'
    Colon, // ':'
    
    // Language
    Fun,
    Import, Extern, LineSep, 
    Main, Script, Dynamic, Static,
}


struct Name {
private:
    uint id;
    this(uint id) { this.id = id; }
public:
    @property bool isEmpty() const { return this == BuiltinName!""; }
    @property bool isReserved() const { return id < (Names.length - Prefill.length); }
    @property bool isDefined() const { return id != 0; }
    string toString() const { return id.to!string; }
    string toString(const ref NameManager nm) const { return nm.names[id]; }
}

struct Location {
    uint line = 1;
    uint index = 0;
    uint length = 0;

    string toString() {
        import std.conv: to;
        return "@" ~ line.to!string ~ ":" ~ index.to!string ~ "";
    }
}

struct NameManager {
private:
    string[] names;
    uint[string] lookups;
    
    // make it not copyable
    @disable this(this);
    
package:
    static get() {
        return NameManager(Names, Lookups);
    }
    
public:
    auto getName(string s) {
        if (auto id = s in lookups) {
            return Name(*id);
        }
        
        // Do not keep around slice of potentially large input.
        s = s.idup; // similar problem to old java's string
        
        auto id = lookups[s] = cast(uint) names.length;
        names ~= s;
        
        return Name(id);
    }
    
    string getTokenString(Token tok) {
        import std.string: rightJustify;
        return "Token:\t" 
            ~ tok.name.to!string.rightJustify(6) ~ ","
                ~ tok.type.to!string.rightJustify(14) ~ ",\t"
                ~ tok.name.toString(this) ~ ", \t"
                ~ tok.loc.to!string
                ;
    }

    string getTokenName(Token tok) {
        return tok.name.toString(this);
    }
    
    void printLookups() {
        writeln("Lookups:", lookups);
        writeln("Names:", names);
    }
}

/** Context **/
final class Context {
    NameManager nameManager;
    alias nameManager this;
    
    this() { nameManager = NameManager.get(); }
}

public {
    auto getOperatorsMap() {
        with(TokenType) return [
            "+" : Plus,
            "-" : Minus,
            "*" : Star,
            "/" : Slash,
            "." : Dot,
            "=" : Assign,
            "{": BraceBegin,
            "}": BraceEnd,
            "(": ParenBegin,
            ")": ParenEnd,
            ",": Comma,
            ">": Greater,
            "<": Less,
            "==" : Equal,
            "!=" : NotEqual,
            "!" : Not,
            "[" : SquareBegin,
            "]" : SquareEnd,
            ":" : Colon,
        ];
        
    }
    
    auto getKeywordsMap() {
        with(TokenType) return [
            // built-in types
            "val": Val,
            "var": Var,
            "ptr": Ptr,
            "ref": Ref, 
            "bit": Bit,
            "byte": Byte,
            "char": Char,
            "short": Short,
            "int": Int,
            "long": Long,
            "float": Float,
            "double": Double,
            "void" : Void,
            // Control flow
            "do": Do,
            "if": If,
            "else": Else,
            "while": While,
            // language block
            "import": Import,
            "extern": Extern,
            "in": In,
            "return" : Return,
            "main" : Main,
            "for" : For,
            "script" : Script,
            "dynamic" : Dynamic,
            "static" : Static,
            "fun": Fun,
        ];
    }
    
}

public {
    enum Reserved = ["__ctor", "__dtor", "__vtbl"];
    
    enum Prefill = [
        // Linkages
        "C", "D", "C++", "Windows", "System", "Pascal", "Java", "Venus",
        // Version
        "Linux", "OSX",
        // Generated
        "init", "length", "max", "min", "sizeof", "create",
        // Scope
        "exit", "success", "failure",
        // Main
        "main", "_vmain",
        // Attribute
        "property", "unsafe", "dynamic", "alloc",
        
    ];
    
    auto getNames() {
        auto identifiers = [""];
        foreach (k, _; getOperatorsMap()) {
            identifiers ~= k;
        }
        foreach (k, _; getKeywordsMap()) {
            identifiers ~= k;
        }
        
        return identifiers ~ Reserved ~ Prefill;
    }
    
    enum Names = getNames();
    
    static assert(Names[0] == "");
    
    auto getLookups() {
        uint[string] lookups;
        foreach (uint i, id; Names) {
            lookups[id] = i;
        }
        return lookups;
    }
    
    enum Lookups = getLookups();
    
    enum OperatorsMap = getOperatorsMap();
    enum KeywordsMap = getKeywordsMap();
}


template BuiltinName(string name) {
    private enum id = Lookups.get(name, uint.max);
    static assert(id < uint.max, name ~ " is not a builtin name.");
    enum BuiltinName = Name(id);
}
