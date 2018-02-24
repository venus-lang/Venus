module venus.ast;

import std.experimental.logger;
import venus.context;
import std.conv: to;

class AstNode {
    Location loc;
    this(Location loc) {
        this.loc = loc;
    }

    override string toString() {
        import std.conv: to;
        return "Node:" ~ loc.to!string;
    }
}

class RootNode :AstNode {
    AstNode[] children;

public:
    this(Location loc) {
        super(loc);
    }

    void addChild(AstNode n) {
        info("adding chld:", n);
        children ~= n;
    }
}

class EmptyNode : AstNode {
    this(Location loc) {
        super(loc);
    }

    override string toString() {
        return "EmptyNode:" ~ loc.to!string;
    }
}

class Declaration : AstNode {
    this(Location loc) {
        super(loc);
    }
}

class Identifier : AstNode {
    Name name;

    this(Location loc, Name name) {
        super(loc);
        this.name = name;
    }
}

class ImportDeclaration : Declaration {
    Name[][] modules;

    this(Location loc, Name[][] modules) {
        super(loc);
        this.modules = modules;
    }

    override string toString() {
        return "Import" ~ loc.to!string;
    }
}

class Block : AstNode {
    Expr[] exprs;
    this(Location loc, Expr[] exprs) {
        super(loc);
        this.exprs = exprs;
    }
}

class MainBlock : AstNode {
    Block block;

    string name = "main";

    this(Location loc, Block block) {
        super(loc);
        this.block = block;
    }

    override string toString() {
        return "MainBlock" ~ loc.to!string;
    }
}

class CallExpr : Expr {
    Expr callee;
    Expr[] args;
    this(Location loc, Expr callee, Expr[] args) {
        super(loc);

        this.callee = callee;
        this.args = args;
    }
}

class Expr: AstNode {
    this(Location loc) {
        super(loc);
    }
}

enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Unknown
}

class BinaryExpr : Expr {
    Expr lhs;
    Expr rhs;

    BinaryOp op;

    this(Location loc, BinaryOp op, Expr lhs, Expr rhs) {
        super(loc);
        this.lhs = lhs;
        this.rhs = rhs;
        this.op = op;
    }
}

class IdentifierExpr: Expr {
    Identifier identifier;

    this(Location loc, Identifier ident) {
        super(loc);
        this.identifier = ident;
    }

}

class StringLiteralExpr: Expr {
    private string value;
    this(Location loc, string value) {
        super(loc);
        this.value = value;
    }

    override string toString() {
        return "'" ~ value ~ "'";
    }
}

class IntExpr : Expr {
    private Name name;
    this(Location loc, Name name) {
        super(loc);
        this.name = name;
    }
}

class ArgExpr: Expr {
    private IdentifierExpr name;
    private Name type;

    this(Location loc, IdentifierExpr name, Name type) {
        super(loc);
        this.name = name;
        this.type = type;
    }
}

class FunDef: Expr {
    IdentifierExpr name;
    Expr[] args;
    TypeExpr type;
    Block bodyBlock;

    this(Location loc, IdentifierExpr name, Expr[] args, TypeExpr type, Block bodyBlock) {
        super(loc);
        this.name = name;
        this.args = args;
        this.type = type;
        this.bodyBlock = bodyBlock;
    }

    override string toString() {
        return "FunDef" ~ loc.to!string;
    }
}

enum Type {
    Int, 
    Double,
    Custom
}

// TODO: is Type an Expr?
class TypeExpr : Expr {
    Type type;
    Token token;

    this(Location loc, Type type, Token token) {
        super(loc);
        this.type = type;
        this.token = token;
    }
}

