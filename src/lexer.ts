import { Token } from "./ast";

export function lex(input: string): Token[] {
	const tokens: Token[] = [];
	let i = 0;

	const isSpace = (c: string) => /\s/.test(c);
	const isIdentStart = (c: string) => /[A-Za-z0-9_+\-/*?<>=!]/.test(c);
	const isIdentPart = (c: string) => /[A-Za-z0-9_+\-/*?<>=!]/.test(c);

	while (i < input.length) {
		const ch = input[i];

		if (isSpace(ch)) {
			i++;
			continue;
		}

		if (ch === ";") {
			while (i < input.length && input[i] !== "\n") i++;
			continue;
		}

		if (ch === "(") {
			tokens.push({ type: "LPAREN" });
			i++;
			continue;
		}

		if (ch === ")") {
			tokens.push({ type: "RPAREN" });
			i++;
			continue;
		}

		if (ch === '"' || ch === "'") {
			const quote = ch;
			const start = i;
			let j = i + 1;
			let buf = "";
			let closed = false;
			while (j < input.length) {
				const c = input[j];
				if (c === "\\") {
					if (j + 1 >= input.length) break;
					const n = input[j + 1];
					if (n === quote || n === "\\") {
						buf += n;
						j += 2;
					} else if (n === "n") { buf += "\n"; j += 2; }
					else if (n === "t") { buf += "\t"; j += 2; }
					else if (n === "r") { buf += "\r"; j += 2; }
					else { buf += n; j += 2; }
					continue;
				}
				if (c === quote) { closed = true; j++; break; }
				buf += c;
				j++;
			}
			if (!closed) {
				if (quote === "'") {
					tokens.push({ type: "QUOTE" });
					i++;
					continue;
				}
				throw new Error("unterminated string literal");
			}
			tokens.push({ type: "STRING", value: buf });
			i = j;
			continue;
		}

		if (ch === "`") {
			tokens.push({ type: "QUASIQUOTE" });
			i++;
			continue;
		}

		if (ch === ",") {
			tokens.push({ type: "UNQUOTE" });
			i++;
			continue;
		}

		if (isIdentStart(ch)) {
			let start = i;
			i++;
			while (i < input.length && isIdentPart(input[i])) i++;
			tokens.push({ type: "IDENT", value: input.slice(start, i) });
			continue;
		}

		throw new Error(`unexpected character: ${ch}`);
	}

	tokens.push({ type: "EOF" });
	// Allow downstream parser to detect multi-form inputs in helpers.
	(tokens as any)._allowMultiple = input.includes("\n");
	return tokens;
}
