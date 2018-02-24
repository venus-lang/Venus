module venus.parser;
import std.range;
import std.stdio;
import std.experimental.logger;

import venus.context;
import venus.lexer;
import venus.ast;
import venus.exception;

unittest {

    AstNode[] testParse(string code) {
        writeln("\n> Testing parse for: \n{", code, "\n}");
        Context ctx = new Context();
        auto rootNode = code.lex(ctx).parseTree(ctx);
        info("parsed AstNodes: ", rootNode.children);
        return rootNode.children;
    }

    // one line import
    testParse("import std.io");

    // multi import
    testParse(q{
        import std.io;
        import std.net;
    });

    // empty main block
    testParse("main{ }\n");

    // main block with function call
    testParse(q{ main{println("hello")} });

    // simple expressions
    testParse(q{main { 1 + 1 }});

    testParse(q{main { 1 + (2 * 3) }});

    // function definition
    testParse(q{
        import std.io;
        fun add(a int, b int) int { a + b }

        main { println(add(a, b)) }
    });

    // all
    testParse(q{
        import std.io
        import std.math
        fun add(a int, b int) int {
            info("a+c")
            a + b
        }
        main {
            add(1, 3)
        }
    });
}

class TreeParser {
    Context ctx;
    RootNode root;
    Lexer tokens;

    this(Lexer tokens, Context ctx) {
        this.tokens = tokens;
        this.ctx = ctx;
    }

    bool parse() {

        /*
        foreach (t; tokens) {
            writeln(t);
        }
        */
        Token t = tokens.front;
        Location loc = t.loc;
        this.root = new RootNode(loc);
        if (t.type != TokenType.Begin) {
            error("Tokens not starting with TokenType.Begin!");
            return false;
        }
        while (t.type != TokenType.End) {
            with (TokenType) switch (t.type)  {
                case Begin:
                    break;
                case LineSep:
                    break;
                case Fun:
                    root.addChild(parseFunDef());
                    break;
                case Import:
                    auto imp = parseImport();
                    root.addChild(imp);
                    break;
                case Main:
                    writeln("case main");
                    root.addChild(parseMain());
                    break;
                default:
                    writeln("Unkown Token:", ctx.getTokenString(t));
                    //return new AstNode(t.loc);
                    break;
            }
            tokens.popFront();
            t = tokens.front;
        }
        return true;
    }

    Token nextTok() {
        tokens.popFront();
        return tokens.front;
    }

    void match(TokenType type) {
        //    writeln("Matching:", type, " with ", ctx.getTokenString(tokens.front));
        auto token = tokens.front;

        if(token.type != type) {
            import venus.exception;
            import std.conv, std.string;

            auto err = format("expected '%s', got '%s'.", to!string(type), to!string(token.type));

            error(err);
            // throw new CompileException(token.location, error);
            Location loc;
            throw new CompileException(loc, err);
        }

        tokens.popFront();

    }

    MainBlock parseMain() {
        Location loc = tokens.front.loc;
        match(TokenType.Main);
        // parse body
        Block block = parseBlock();
        return new MainBlock(loc, block);
    }

    auto parseModuleName() {
        auto mod = [tokens.front.name];
        match(TokenType.Identifier);
        while (tokens.front.type == TokenType.Dot) {
            match(TokenType.Dot);
            mod ~= tokens.front.name;
            match(TokenType.Identifier);
        }
        import std.algorithm: map, joiner;
        info("module: ", mod.map!(x => this.lookup(x)).join("."));
        return mod;
    }

    string lookup(ref in Name name) {
        return ctx.lookup(name);
    }

    ImportDeclaration parseImport() {
        Token front = nextTok(); // pop 'import'
        Name[][] modules = [parseModuleName()];
        info("import parsed");
        return new ImportDeclaration(front.loc, modules);
    }

    Expr[] parseArguments() {
        Expr[] es;
        Expr e = parseExpression();
        es ~= e;
        while (tokens.front.type == TokenType.Comma) {
            match(TokenType.Comma);
            es ~= parseExpression();
        }
        Location loc;
        return es;
    }

    CallExpr parseFunctionCall(IdentifierExpr ident) {
        IdentifierExpr callee = ident;
        match(TokenType.ParenBegin);
        Expr[] args = parseArguments();
        match(TokenType.ParenEnd);
        Location loc;
        return new CallExpr(loc, callee, args);
    }


    StringLiteralExpr parseStringLiteral() {
        Location loc;
        match(TokenType.StringLiteral);
        return new StringLiteralExpr(loc, ctx.getTokenName(tokens.front));
    }

    IntExpr parseIntExpr() {
        Location loc;
        match(TokenType.IntLiteral);
        return new IntExpr(loc, tokens.front.name);
    }

    Expr parseParenExpr() {
        match(TokenType.ParenBegin); // eat '('
        auto expr = parseExpression();
        Location loc;
        if (!isNext(TokenType.ParenEnd)) {
            import venus.exception;
            throw new CompileException(loc, "expected ')' for paren expr");
        }
        match(TokenType.ParenEnd);
        return expr;
    }

    Expr parsePrimaryExpr() {

        //        writeln("parsing primary expr");
        switch (tokens.front.type) {
            case TokenType.Identifier:
                IdentifierExpr id = parseIdentifier();
                if (isNext(TokenType.ParenBegin)) {
                    CallExpr expr = parseFunctionCall(id);
                    return expr;
                } else return id;
            case TokenType.StringLiteral:
                return parseStringLiteral();
            case TokenType.IntLiteral:
                return parseIntExpr();
            case TokenType.ParenBegin:
                return parseParenExpr();
            default:
                writeln("Error: Unknown expression:", tokens.front);
                return null;
                
        }

    }

    // TODO: move this to the definition of binop
    bool isNextBinOp() {
        return isNext(TokenType.Plus) 
            || isNext(TokenType.Minus)
                || isNext(TokenType.Star)
                || isNext(TokenType.Slash);
    }

    
    Expr parseExpression() {
        auto lhs = parsePrimaryExpr();
        if (isNextBinOp()) {
            return parseBinOp(lhs);
        }
        else return lhs;
    }

    
    Expr parseBinOp(Expr lhs) {
        Token tok = tokens.front;
        //        writeln("parsing binop:", tok.type);
        BinaryOp op;
        switch (tok.type) {
            case TokenType.Plus:
                op = BinaryOp.Add;
                break;
            case TokenType.Minus:
                op = BinaryOp.Sub;
                break;
            case TokenType.Star:
                op = BinaryOp.Mul;
                break;
            case TokenType.Slash:
                op = BinaryOp.Div;
                break;
            default:
                op = BinaryOp.Unknown;
                break;
        }
        nextTok();
        Expr rhs = parseExpression();
        Location loc;
        return new BinaryExpr(loc, op, lhs, rhs);
    }


    TypeExpr parseReturnType() {
        return parseType();
    }

    TypeExpr parseType() {
        Token t = tokens.front;
        nextTok();
        Location loc;
        switch (t.type) {
            case TokenType.Int:
                return new TypeExpr(loc, Type.Int, t);
            case TokenType.Double:
                return new TypeExpr(loc, Type.Double, t);
            case TokenType.Identifier:
                return new TypeExpr(loc, Type.Custom, t);
            default:
                throw new CompileException(loc, "Unkown type: " ~ ctx.getTokenName(t));
        }
    }

    Expr parseFunDef() {
        Location loc = tokens.front.loc;
        match(TokenType.Fun);
        IdentifierExpr funName = parseIdentifier();
        match(TokenType.ParenBegin);
        Expr[] args = parseDefArgs();
        match(TokenType.ParenEnd);
        TypeExpr retType = parseReturnType();
        Block bodyBlock = parseBlock();
        return new FunDef(loc, funName, args, retType, bodyBlock);
    }


    IdentifierExpr parseIdentifier() {
        Location loc;
        Identifier ident = new Identifier(loc, tokens.front.name);
        match(TokenType.Identifier);
        return new IdentifierExpr(loc, ident);
    }

    ArgExpr parseArg() {
        IdentifierExpr id = parseIdentifier();
        Token t = tokens.front;
        writeln("type:", ctx.getTokenName(t));
        nextTok();
        Location loc;
        return new ArgExpr(loc, id, t.name);
    }

    Expr[] parseDefArgs() {
        Expr[] es;
        Expr e = parseArg();
        es ~= e;
        while (tokens.front.type == TokenType.Comma) {
            match(TokenType.Comma);
            es ~= parseArg();
        }
        Location loc;
        return es;
    }


    auto parseStatements() {
        Expr[] exprs;
        while (!isNext(TokenType.BraceEnd)) {
            Expr e = parseExpression();
            exprs ~= e;
            if (isNext(TokenType.LineSep)) {
                nextTok();
            }
        }
        return exprs;
    }

    Block parseBlock() {
        match(TokenType.BraceBegin);
        // parse block body
        Expr[] exprs;
        if (!isNext(TokenType.BraceEnd)) {
            exprs = parseStatements();
        }
        match(TokenType.BraceEnd);
        Location loc;
        return new Block(loc, exprs);
    }

    bool isNext(TokenType type) {
        return !tokens.empty() && tokens.front.type == type;
    }


}

struct Parser(TokenRange) {
    AstNode n;
    TokenRange tokens;
    Context ctx;

    AstNode next() {
        Location loc = tokens.front.loc;
        if (tokens.empty()) {
            return new EmptyNode(loc);
        }

        Token front = tokens.front;
        front = nextTok();
        loc = front.loc;

        while (front.type != TokenType.End) {
            // writeln("check token:", ctx.getTokenString(front));
            with (TokenType) switch (front.type)  {
                case Begin:
                    break;
                case LineSep:
                    break;
                case Fun:
                    return parseFunDef();
                case Import:
                    return parseImport();
                case Main:
                    writeln("case main");
                    return parseMain();
                default:
                    writeln("Unkown Token:", ctx.getTokenString(front));
                    return new AstNode(front.loc);
            }
            front = nextTok();
        }
        return new EmptyNode(loc);
    }

    @property auto front() {
        if (!n) {
            popFront();
        }
        return n;
    }

    void popFront() {
        n = next();
    }

    @property auto save() inout {
        return inout(Parser)(n, tokens, ctx);
    }

    @property bool empty() const {
        return tokens.empty();
    }

    Token nextTok() {
        tokens.popFront();
        if (!tokens.empty()) {
            return tokens.front;
        } else {
            Token t;
            t.type = TokenType.End;
            t.name = ctx.getName("End");
            return t;
        }
    }

    void match(TokenType type) {
        //    writeln("Matching:", type, " with ", ctx.getTokenString(tokens.front));
        auto token = tokens.front;

        if(token.type != type) {
            import venus.exception;
            import std.conv, std.string;

            auto error = format("expected '%s', got '%s'.", to!string(type), to!string(token.type));

            // throw new CompileException(token.location, error);
            Location loc;
            throw new CompileException(loc, error);
        }

        tokens.popFront();
    }

    bool isNext(TokenType type) {
        return !tokens.empty() && tokens.front.type == type;
    }

    TypeExpr parseReturnType() {
        return parseType();
    }

    TypeExpr parseType() {
        Token t = tokens.front;
        nextTok();
        Location loc;
        switch (t.type) {
            case TokenType.Int:
                return new TypeExpr(loc, Type.Int, t);
            case TokenType.Double:
                return new TypeExpr(loc, Type.Double, t);
            case TokenType.Identifier:
                return new TypeExpr(loc, Type.Custom, t);
            default:
                throw new CompileException(loc, "Unkown type: " ~ ctx.getTokenName(t));
        }
    }

    Expr parseFunDef() {
        Location loc = tokens.front.loc;
        match(TokenType.Fun);
        IdentifierExpr funName = parseIdentifier();
        match(TokenType.ParenBegin);
        Expr[] args = parseDefArgs();
        match(TokenType.ParenEnd);
        TypeExpr retType = parseReturnType();
        Block bodyBlock = parseBlock();
        return new FunDef(loc, funName, args, retType, bodyBlock);
    }

    auto parseModuleName() {
        auto mod = [tokens.front.name];
        match(TokenType.Identifier);
        while (tokens.front.type == TokenType.Dot) {
            match(TokenType.Dot);
            mod ~= tokens.front.name;
            match(TokenType.Identifier);
        }
        return mod;
    }

    ImportDeclaration parseImport() {
        Token front = nextTok(); // pop 'import'
        Name[][] modules = [parseModuleName()];
        return new ImportDeclaration(front.loc, modules);
    }

    IdentifierExpr parseIdentifier() {
        Location loc;
        Identifier ident = new Identifier(loc, tokens.front.name);
        match(TokenType.Identifier);
        return new IdentifierExpr(loc, ident);
    }

    StringLiteralExpr parseStringLiteral() {
        Location loc;
        match(TokenType.StringLiteral);
        return new StringLiteralExpr(loc, ctx.getTokenName(tokens.front));
    }

    IntExpr parseIntExpr() {
        Location loc;
        match(TokenType.IntLiteral);
        return new IntExpr(loc, tokens.front.name);
    }

    Expr parseParenExpr() {
        match(TokenType.ParenBegin); // eat '('
        auto expr = parseExpression();
        Location loc;
        if (!isNext(TokenType.ParenEnd)) {
            import venus.exception;
            throw new CompileException(loc, "expected ')' for paren expr");
        }
        match(TokenType.ParenEnd);
        return expr;
    }

    Expr parsePrimaryExpr() {

        //        writeln("parsing primary expr");
        switch (tokens.front.type) {
            case TokenType.Identifier:
                IdentifierExpr id = parseIdentifier();
                if (isNext(TokenType.ParenBegin)) {
                    CallExpr expr = parseFunctionCall(id);
                    return expr;
                } else return id;
            case TokenType.StringLiteral:
                return parseStringLiteral();
            case TokenType.IntLiteral:
                return parseIntExpr();
            case TokenType.ParenBegin:
                return parseParenExpr();
            default:
                writeln("Error: Unknown expression:", tokens.front);
                return null;
                
        }

    }

    // TODO: move this to the definition of binop
    bool isNextBinOp() {
        return isNext(TokenType.Plus) 
            || isNext(TokenType.Minus)
                || isNext(TokenType.Star)
                || isNext(TokenType.Slash);
    }

    
    Expr parseExpression() {
        auto lhs = parsePrimaryExpr();
        if (isNextBinOp()) {
            return parseBinOp(lhs);
        }
        else return lhs;
    }

    
    Expr parseBinOp(Expr lhs) {
        Token tok = tokens.front;
        //        writeln("parsing binop:", tok.type);
        BinaryOp op;
        switch (tok.type) {
            case TokenType.Plus:
                op = BinaryOp.Add;
                break;
            case TokenType.Minus:
                op = BinaryOp.Sub;
                break;
            case TokenType.Star:
                op = BinaryOp.Mul;
                break;
            case TokenType.Slash:
                op = BinaryOp.Div;
                break;
            default:
                op = BinaryOp.Unknown;
                break;
        }
        nextTok();
        Expr rhs = parseExpression();
        Location loc;
        return new BinaryExpr(loc, op, lhs, rhs);
    }

    ArgExpr parseArg() {
        IdentifierExpr id = parseIdentifier();
        Token t = tokens.front;
        writeln("type:", ctx.getTokenName(t));
        nextTok();
        Location loc;
        return new ArgExpr(loc, id, t.name);
    }

    Expr[] parseDefArgs() {
        Expr[] es;
        Expr e = parseArg();
        es ~= e;
        while (tokens.front.type == TokenType.Comma) {
            match(TokenType.Comma);
            es ~= parseArg();
        }
        Location loc;
        return es;
    }

    Expr[] parseArguments() {
        Expr[] es;
        Expr e = parseExpression();
        es ~= e;
        while (tokens.front.type == TokenType.Comma) {
            match(TokenType.Comma);
            es ~= parseExpression();
        }
        Location loc;
        return es;
    }

    CallExpr parseFunctionCall(IdentifierExpr ident) {
        IdentifierExpr callee = ident;
        match(TokenType.ParenBegin);
        Expr[] args = parseArguments();
        match(TokenType.ParenEnd);
        Location loc;
        return new CallExpr(loc, callee, args);
    }


    auto parseStatements() {
        Expr[] exprs;
        while (!isNext(TokenType.BraceEnd)) {
            Expr e = parseExpression();
            exprs ~= e;
            if (isNext(TokenType.LineSep)) {
                nextTok();
            }
        }
        return exprs;
    }

    Block parseBlock() {
        match(TokenType.BraceBegin);
        // parse block body
        Expr[] exprs;
        if (!isNext(TokenType.BraceEnd)) {
            exprs = parseStatements();
        }
        match(TokenType.BraceEnd);
        Location loc;
        return new Block(loc, exprs);
    }

    MainBlock parseMain() {
        Location loc = tokens.front.loc;
        match(TokenType.Main);
        // parse body
        Block block = parseBlock();
        return new MainBlock(loc, block);
    }
}

auto parse(TokenRange)(TokenRange tokens, Context ctx) if (isForwardRange!TokenRange) {
    AstNode n;
    auto p = Parser!TokenRange(n, tokens, ctx);
    return p;
}

RootNode parseTree(Lexer tokens, Context ctx) {
    auto parser = new TreeParser(tokens, ctx);
    parser.parse();
    return parser.root;
}
