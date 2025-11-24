import { AnyNode, ASTNode, Token } from "./ast";

function sym(name: string): ASTNode {
	return { type: "Symbol", name };
}

function makeList(items: ASTNode[]): ASTNode {
	if (items.length === 0) return { type: "Nil" };
	const [head, ...args] = items;
	return { type: "List", head, args };
}

export function parse(tokens: Token[], allowMultiple = false): AnyNode {
	let pos = 0;
	const peek = () => tokens[pos];
	const consume = (type?: string) => {
		const t = tokens[pos];
		if (type && t.type !== type) {
			throw new Error(`expected ${type} but found ${t.type}`);
		}
		pos++;
		return t;
	};

	function parseExpr(): ASTNode {
		const t = peek();

		if (t.type === "QUOTE") {
			consume("QUOTE");
			return makeList([sym("quote"), parseExpr()]);
		}

		if (t.type === "QUASIQUOTE") {
			consume("QUASIQUOTE");
			return makeList([sym("quasiquote"), parseExpr()]);
		}

		if (t.type === "UNQUOTE") {
			consume("UNQUOTE");
			return makeList([sym("unquote"), parseExpr()]);
		}

		if (t.type === "STRING") {
			consume("STRING");
			return { type: "StringLiteral", value: t.value };
		}

		if (t.type === "IDENT") {
			consume("IDENT");
			return { type: "Symbol", name: t.value };
		}

		if (t.type === "LPAREN") {
			consume("LPAREN");
			const items: ASTNode[] = [];

			while (true) {
				const p = peek();
				if (p.type === "RPAREN") break;
				if (p.type === "EOF") throw new Error("unterminated list");
				items.push(parseExpr());
			}

			consume("RPAREN");

			if (items.length === 0) return { type: "Nil" };
			const [head, ...args] = items;
			return { type: "List", head, args };
		}

		throw new Error(`unexpected token: ${t.type}`);
	}

	const forms: ASTNode[] = [];
	const multipleOk = allowMultiple || !!(tokens as any)._allowMultiple;

	forms.push(parseExpr());
	while (peek().type !== "EOF") {
		if (!multipleOk) {
			throw new Error("unexpected tokens at end");
		}
		forms.push(parseExpr());
	}

	if (forms.length === 1) return forms[0];
	return { type: "Program", body: forms };
}
