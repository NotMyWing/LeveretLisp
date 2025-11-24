export type Token =
	| { type: "LPAREN" }
	| { type: "RPAREN" }
	| { type: "QUOTE" }
	| { type: "QUASIQUOTE" }
	| { type: "UNQUOTE" }
	| { type: "STRING"; value: string }
	| { type: "IDENT"; value: string }
	| { type: "EOF" };

export type ASTNode =
	| { type: "StringLiteral"; value: string }
	| { type: "Symbol"; name: string }
	| { type: "List"; head: ASTNode; args: ASTNode[] }
	| { type: "Nil" };

export type ProgramNode = { type: "Program"; body: ASTNode[] };

export type AnyNode = ASTNode | ProgramNode;
