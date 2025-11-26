import { lex } from "../src/lexer.ts";
import { parse } from "../src/parser.ts";
import { evaluate, jitStats } from "../src/evaluator.ts";
// If you have the wrapper/entry, keep this. If not, you can comment it out.
import { runLeveretLisp } from "../src/leveretlisp";

// A mock executor for tags
const execTag = jest.fn((name: string, args: string) => {
	return { name, args };
});

const evalSrc = (src: string) => {
	const tokens = lex(src);
	const ast = parse(tokens);
	return evaluate(ast, execTag);
};

describe("1. Lexical Structure", () => {

	test("recognizes parentheses", () => {
		const out = lex("( )");
		expect(out.map(t => t.type)).toEqual(["LPAREN", "RPAREN", "EOF"]);
	});

	test("recognizes identifiers", () => {
		const out = lex("(foo bar baz)");
		expect(out.filter(t => t.type === "IDENT").map(t => (t as any).value))
			.toEqual(["foo", "bar", "baz"]);
	});

	test("recognizes strings with double quotes", () => {
		const out = lex("\"abc\"");
		expect(out[0]).toEqual({ type: "STRING", value: "abc" });
	});

	test("recognizes strings with single quotes", () => {
		const out = lex("'xyz'");
		expect(out[0]).toEqual({ type: "STRING", value: "xyz" });
	});

	test("string escape sequences", () => {
		const out = lex("\"a\\nb\"");
		expect(out[0]).toEqual({ type: "STRING", value: "a\nb" });
	});

	test("errors on invalid characters", () => {
		expect(() => lex("@")).toThrow();
	});

	test("line comments starting with ;", () => {
		const out = lex("(foo ;comment\n bar)");
		const idents = out.filter(t => t.type === "IDENT").map(t => (t as any).value);
		expect(idents).toEqual(["foo", "bar"]);
	});
});

describe("1b. Lexical edge cases", () => {
	test("whitespace robustness", () => {
		const src = "\t(   foo   \n   bar\t)\r\n";
		const out = lex(src);
		const idents = out.filter(t => t.type === "IDENT").map(t => (t as any).value);
		expect(idents).toEqual(["foo", "bar"]);
	});

	test("full-line comment", () => {
		const src = "; just a comment\n(foo)";
		const out = lex(src);
		const idents = out.filter(t => t.type === "IDENT").map(t => (t as any).value);
		expect(idents).toEqual(["foo"]);
	});

	test("trailing comment at EOF", () => {
		const src = "(foo) ; trailing\n";
		const out = lex(src);
		const idents = out.filter(t => t.type === "IDENT").map(t => (t as any).value);
		expect(idents).toEqual(["foo"]);
	});

	test("identifier with operator-ish chars", () => {
		const out = lex("(+foo? bar! baz=)");
		const idents = out.filter(t => t.type === "IDENT").map(t => (t as any).value);
		expect(idents).toEqual(["+foo?", "bar!", "baz="]);
	});

	test("escaped quote inside string", () => {
		const out = lex("\"he said \\\"hi\\\"\"");
		expect(out[0]).toEqual({ type: "STRING", value: "he said \"hi\"" });
	});
});


describe("2. Parser", () => {

	test("parses simple list", () => {
		const ast = parse(lex("(foo bar)"));
		expect(ast.type).toBe("List");
		expect(ast.head.type).toBe("Symbol");
		expect(ast.args.length).toBe(1);
	});

	test("parses nested lists", () => {
		const ast = parse(lex("(a (b (c)))"));
		expect(ast.type).toBe("List");
		expect(ast.args[0].type).toBe("List");
		expect(ast.args[0].args[0].type).toBe("List");
	});

	test("empty list becomes Nil", () => {
		const ast = parse(lex("()"));
		expect(ast).toEqual({ type: "Nil" });
	});

	test("string literal node", () => {
		const ast = parse(lex("\"hello\""));
		expect(ast).toEqual({ type: "StringLiteral", value: "hello" });
	});

	test("symbol node", () => {
		const ast = parse(lex("abc"));
		expect(ast).toEqual({ type: "Symbol", name: "abc" });
	});

	test("errors on unexpected tokens", () => {
		expect(() => parse(lex(")"))).toThrow();
	});

	test("reader macro for quote `'`", () => {
		const ast = parse(lex("'foo"));
		const expected = parse(lex("(quote foo)"));
		expect(ast).toEqual(expected);
	});

	test("reader macro for quasiquote ``` ` ```", () => {
		const ast = parse(lex("`foo"));
		const expected = parse(lex("(quasiquote foo)"));
		expect(ast).toEqual(expected);
	});

	test("reader macro for unquote `,`", () => {
		const ast = parse(lex(",bar"));
		const expected = parse(lex("(unquote bar)"));
		expect(ast).toEqual(expected);
	});
});

describe("2b. Parser edge cases", () => {
	test("multiple top-level forms is an error (single-form language)", () => {
		expect(() => parse(lex("(foo)(bar)"))).toThrow(/unexpected tokens at end/i);
	});

	test("nested empty list", () => {
		const ast = parse(lex("(foo ())"));
		expect(ast.type).toBe("List");
		expect(ast.args[0]).toEqual({ type: "Nil" });
	});

	test("list with only head", () => {
		const ast = parse(lex("(foo)"));
		expect(ast.type).toBe("List");
		expect(ast.head.type).toBe("Symbol");
		expect(ast.args.length).toBe(0);
	});
});


describe("3. Evaluation: Strings, Symbols, Lists", () => {

	beforeEach(() => execTag.mockClear());

	test("string literal evaluates to the string", () => {
		expect(evalSrc("\"hi\"")).toBe("hi");
	});

	test("symbol evaluates as zero-arg tag call", () => {
		evalSrc("foo");
		expect(execTag).toHaveBeenCalledWith("foo", "");
	});

	test("list head calls tag with arguments", () => {
		evalSrc("(foo \"a\" \"b\")");
		expect(execTag).toHaveBeenCalledWith("foo", "a b");
	});

	test("nested tag evaluation", () => {
		evalSrc("(foo (bar))");
		expect(execTag).toHaveBeenCalledWith("bar", "");
		expect(execTag).toHaveBeenCalledWith("foo", expect.any(String));
	});

	test("Nil evaluates to null", () => {
		const ast = parse(lex("()"));
		const v = evaluate(ast, execTag);
		expect(v).toBeNull();
	});

	test("error when head is not a symbol", () => {
		expect(() => evalSrc("(\"x\")")).toThrow(/head must be a symbol/i);
	});
});

describe("3b. Evaluation edge cases", () => {

	beforeEach(() => execTag.mockClear());

	test("calling unknown symbol as tag passes empty args", () => {
		evalSrc("unknown");
		expect(execTag).toHaveBeenCalledWith("unknown", "");
	});

	test("list with no args still calls tag", () => {
		evalSrc("(foo)");
		expect(execTag).toHaveBeenCalledWith("foo", "");
	});

	test("deeply nested lists evaluate all inner calls", () => {
		evalSrc("(a (b (c (d))))");
		expect(execTag).toHaveBeenCalledTimes(4);
		expect(execTag.mock.calls[0][0]).toBe("d");
		expect(execTag.mock.calls[3][0]).toBe("a");
	});
});


describe("4. Builtins - concat, +, unembed_desc", () => {

	test("concat joins arguments", () => {
		expect(evalSrc("(concat \"a\" \"b\" \"c\")")).toBe("abc");
	});

	test("+ joins arguments", () => {
		expect(evalSrc("(+ \"1\" \"2\" \"3\")")).toBe("123");
	});

	test("unembed_desc extracts description", () => {
		const obj = { embed: { description: "abc" } };
		execTag.mockReturnValueOnce(obj);

		const res = evalSrc("(unembed_desc (foo))");
		expect(res).toBe("abc");
	});

	test("concat coercion rules for numbers", () => {
		execTag.mockReturnValueOnce(1);
		execTag.mockReturnValueOnce(2);
		execTag.mockReturnValueOnce(3);
		const res = evalSrc("(concat (one) (two) (three))");
		expect(res).toBe("123");
	});

	test("+ error on non-string coercion failure", () => {
		const bad = {
			toJSON() { throw new Error("coercion failed"); },
			toString() { throw new Error("coercion failed"); }
		};
		execTag.mockReturnValueOnce(bad);
		expect(() => evalSrc("(+ (bad))")).toThrow(/coercion failed/);
	});
});

describe("4b. Builtins - edge behavior", () => {
	test("concat with zero args returns empty string", () => {
		const res = evalSrc("(concat)");
		expect(res).toBe("");
	});

	test("+ with single arg returns same string", () => {
		const res = evalSrc("(+ \"x\")");
		expect(res).toBe("x");
	});

	test("unembed_desc returns empty string on missing embed", () => {
		execTag.mockReturnValueOnce({ other: "x" });
		const res = evalSrc("(unembed_desc (foo))");
		expect(res).toBe("");
	});
});

describe("4c. Expanded builtins", () => {
	test("string transforms", () => {
		expect(evalSrc("(upper \"abC\")")).toBe("ABC");
		expect(evalSrc("(lower \"AbC\")")).toBe("abc");
		expect(evalSrc("(trim \"  spaced  \")")).toBe("spaced");
	});

	test("length, substr, replace", () => {
		expect(evalSrc("(len \"hello\")")).toBe("5");
		expect(evalSrc("(substr \"hello\" \"1\" \"2\")")).toBe("el");
		expect(evalSrc("(replace \"a-b-a\" \"a\" \"x\")")).toBe("x-b-x");
	});

	test("join and split", () => {
		expect(evalSrc("(join \",\" \"a\" \"b\" \"c\")")).toBe("a,b,c");
		const split = evalSrc("(split \"a,b,c\" \",\")");
		expect(split).toEqual(["a", "b", "c"]);
	});

	test("string predicates", () => {
		expect(evalSrc("(starts_with \"hello\" \"he\")")).toBe(true);
		expect(evalSrc("(ends_with \"hello\" \"lo\")")).toBe(true);
		expect(evalSrc("(includes \"hello\" \"zz\")")).toBe(false);
	});

	test("boolean combinators", () => {
		expect(evalSrc("(and \"1\" \"2\")")).toBe(true);
		expect(evalSrc("(and \"1\" \"\")")).toBe(false);
		expect(evalSrc("(or \"\" \"x\")")).toBe(true);
		expect(evalSrc("(not \"\")")).toBe(true);
	});

	test("coalesce and number", () => {
		expect(evalSrc("(coalesce \"\" \"\" \"x\" \"y\")")).toBe("x");
		expect(evalSrc("(number \"3.14\")")).toBe("3.14");
	});

	test("multiply", () => {
		expect(evalSrc("(* \"2\" \"3\" \"4\")")).toBe("24");
		expect(evalSrc("(* )")).toBe("1");
	});

	test("json helpers", () => {
		const obj = evalSrc("(json_parse \"{\\\"a\\\":1}\")");
		expect(obj).toEqual({ a: 1 });
		expect(evalSrc("(json_stringify (quote (a b)))")).toBe(JSON.stringify(evalSrc("(quote (a b))")));
	});
});


describe("5. Tag Invocation Model", () => {

	beforeEach(() => execTag.mockClear());

	test("single argument string passes directly", () => {
		evalSrc("(foo \"x\")");
		expect(execTag).toHaveBeenCalledWith("foo", "x");
	});

	test("object argument stringifies", () => {
		execTag.mockReturnValueOnce({ test: 1 });
		evalSrc("(foo (bar))");
		const last = execTag.mock.calls[1];
		expect(last[1]).toMatch(/{"test":1}/);
	});

	test("multiple arguments join with space", () => {
		evalSrc("(foo \"a\" \"b\" \"c\")");
		expect(execTag).toHaveBeenCalledWith("foo", "a b c");
	});

	test("multi-value returns for tags", () => {
		const payload = { content: "hi", embed: { description: "ok" } };
		execTag.mockReturnValueOnce(payload);
		const res = evalSrc("(foo)");
		expect(res).toBe(payload);
	});
});

describe("5b. Tag Invocation - special cases", () => {

	beforeEach(() => execTag.mockClear());

	test("nested object results still stringify correctly", () => {
		execTag.mockReturnValueOnce({ data: { value: 1 } });
		evalSrc("(foo (bar))");
		const last = execTag.mock.calls[1];
		expect(last[1]).toMatch(/"value":1/);
	});

	test("tag result reused as arg for another", () => {
		execTag
			.mockReturnValueOnce({ embed: { description: "X" } })
			.mockReturnValueOnce({ embed: { description: "Y" } });

		const res = evalSrc("(concat (unembed_desc (a)) (unembed_desc (b)))");
		expect(res).toBe("XY");
	});
});


describe("6. Integrated Examples", () => {

	test("basic nested concat example", () => {
		execTag.mockReturnValueOnce({ embed: { description: "A" } });
		execTag.mockReturnValueOnce({ embed: { description: "B" } });

		const res = evalSrc("(concat (unembed_desc (x)) \" \" (unembed_desc (y)))");
		expect(res).toBe("A B");
	});

	test("embed reconstruction example", () => {
		const out = evalSrc("(foo (+ \"a\" \"b\"))");
		expect(execTag).toHaveBeenCalledWith("foo", "ab");
	});
});

describe("6b. Larger integrated scripts", () => {
	test("complex nest with multiple tags and builtins", () => {
		execTag
			.mockReturnValueOnce({ embed: { description: "Latest" } })
			.mockReturnValueOnce({ embed: { description: "Dev" } })
			.mockReturnValueOnce({ embed: { description: "Other" } });

		const src = `
      (concat
        (unembed_desc (latest))
        " / "
        (unembed_desc (devguide))
        " / "
        (unembed_desc (other)))
    `;
		const res = evalSrc(src);
		expect(res).toBe("Latest / Dev / Other");
	});
});


describe("7. Quoting", () => {

	test("quote returns literal AST", () => {
		const res = evalSrc("(quote (foo bar))");
		const expected = parse(lex("(foo bar)"));
		expect(res).toEqual(expected);
	});

	test("'foo expands to (quote foo)", () => {
		const quoted = parse(lex("'foo"));
		const expanded = parse(lex("(quote foo)"));
		expect(quoted).toEqual(expanded);
	});

	test("'(1 2 3) expands into list literal", () => {
		const quoted = parse(lex("'(1 2 3)"));
		const expanded = parse(lex("(quote (1 2 3))"));
		expect(quoted).toEqual(expanded);
	});

	test("symbols in quoted lists do not get evaluated", () => {
		execTag.mockClear();
		const res = evalSrc("'(foo (bar))");
		expect(execTag).not.toHaveBeenCalled();
		const expected = parse(lex("(foo (bar))"));
		expect(res).toEqual(expected);
	});
});

describe("7b. Quoting edge cases", () => {
	test("quote of symbol returns symbol-node AST", () => {
		const res = evalSrc("(quote foo)");
		const expected = parse(lex("foo"));
		expect(res).toEqual(expected);
	});

	test("nested quote (quote (quote x))", () => {
		const res = evalSrc("(quote (quote foo))");
		const expected = parse(lex("(quote foo)"));
		expect(res).toEqual(expected);
	});
});


describe("8. Quasiquote / Unquote", () => {

	test("`x expands to (quasiquote x)", () => {
		const ast = parse(lex("`x"));
		const expanded = parse(lex("(quasiquote x)"));
		expect(ast).toEqual(expanded);
	});

	test("`,x expands inside quasiquoted list", () => {
		const ast = parse(lex("`(a ,x b)"));
		const expanded = parse(lex("(quasiquote (a (unquote x) b))"));
		expect(ast).toEqual(expanded);
	});

	test("deep unquote inside nested structures", () => {
		const ast = parse(lex("`(a (b ,c))"));
		const expanded = parse(lex("(quasiquote (a (b (unquote c))))"));
		expect(ast).toEqual(expanded);
	});

	test("quasiquote lists evaluate only unquoted expressions", () => {
		execTag.mockClear();
		execTag.mockReturnValueOnce("inner");
		evalSrc("`(foo ,(bar) baz)");
		expect(execTag).toHaveBeenCalledTimes(1);
		expect(execTag).toHaveBeenCalledWith("bar", "");
	});
});

describe("8b. Quasiquote / Unquote edge cases", () => {

	test("nested quasiquote with inner unquote", () => {
		const ast = parse(lex("``(a ,b)"));
		// spec-dependent but we assert correct expansion layering
		// quasiquote of quasiquote etc.
		expect(ast.type).toBe("List");
	});

	test("unquote outside quasiquote is an error at eval", () => {
		expect(() => evalSrc("(unquote x)")).toThrow();
	});
});


describe("9. Let Bindings", () => {

	test("let binds local variables", () => {
		const res = evalSrc("(let ((x \"a\")) x)");
		expect(res).toBe("a");
	});

	test("let does not leak variables outside", () => {
		evalSrc("(let ((x \"inner\")) x)");
		execTag.mockClear();
		evalSrc("x");
		expect(execTag).toHaveBeenCalledWith("x", "");
	});

	test("let supports multiple bindings", () => {
		const res = evalSrc("(let ((a \"1\") (b \"2\")) (+ a b))");
		expect(res).toBe("12");
	});

	test("let body returns last expression result", () => {
		const res = evalSrc("(let ((a \"x\")) (concat a \"y\") (concat a \"z\"))");
		expect(res).toBe("xz");
	});

	test("let shadowing builtins", () => {
		const res = evalSrc("(let ((concat \"shadowed\")) concat)");
		expect(res).toBe("shadowed");
	});
});

describe("9b. Let Bindings - deeper scoping", () => {

	test("nested let uses inner shadow", () => {
		const res = evalSrc(`
      (let ((x "outer"))
        (let ((x "inner"))
          x))
    `);
		expect(res).toBe("inner");
	});

	test("outer binding visible when not shadowed", () => {
		const res = evalSrc(`
      (let ((x "outer"))
        (let ((y "inner"))
          x))
    `);
		expect(res).toBe("outer");
	});

	test("sequential bindings in same let (if spec chooses that)", () => {
		// if 'let' is sequential: b sees a
		const res = evalSrc("(let ((a \"1\") (b (+ a \"2\"))) b)");
		expect(res).toBe("12");
	});
});


describe("10. Macro System", () => {

	test("defmacro defines a macro", () => {
		const res = evalSrc(`
			(defmacro twice (x)
				(+ x x))
			(twice "ha")
		`);
		expect(res).toBe("haha");
	});

	test("macro expands at parse-time / eval pre-pass", () => {
		execTag.mockClear();
		const res = evalSrc(`
			(defmacro const-first (x y) x)
			(const-first "ok" (boom))
		`);
		expect(res).toBe("ok");
		expect(execTag).not.toHaveBeenCalledWith("boom", expect.anything());
	});

	test("macro receives raw AST", () => {
		const res = evalSrc(`
			(defmacro raw (form) \`(quote ,form))
			(raw (foo bar))
		`);
		expect(res).toEqual(parse(lex("(foo bar)")));
	});

	test("macro expansion returns AST", () => {
		const res = evalSrc(`
			(defmacro lit-list () '(foo bar))
			(lit-list)
		`);
		expect(res).toEqual(parse(lex("(foo bar)")));
	});

	test("macro calling macro", () => {
		const res = evalSrc(`
			(defmacro twice (x) (+ x x))
			(defmacro twice-more (x) (twice (+ x "!")))
			(twice-more "ha")
		`);
		expect(res).toBe("ha!ha!");
	});

	test("hygienic gensym", () => {
		const res = evalSrc(`
			(defmacro with-temp (body)
				(let ((g (gensym)))
					\`(let ((,g "temp")) ,body)))
			(let ((temp "outer"))
				(with-temp temp))
		`);
		expect(res).toBe("outer");
	});

	test("macro argument matching", () => {
		expect(() => evalSrc(`
			(defmacro one-arg (x) x)
			(one-arg "a" "b")
		`)).toThrow();
	});

	test("macro failing on invalid expansion", () => {
		expect(() => evalSrc(`
			(defmacro bad () "not-an-ast")
			(bad)
		`)).toThrow();
	});
});

describe("10b. Macro System - advanced patterns", () => {

	test("macro that wraps builtins", () => {
		const res = evalSrc(`
      (defmacro join3 (a b c) (+ a "-" b "-" c))
      (join3 "x" "y" "z")
    `);
		expect(res).toBe("x-y-z");
	});

	test("macro that generates let", () => {
		const res = evalSrc(`
      (defmacro with-x (val body)
        \`(let ((x ,val)) ,body))
      (with-x "v" (+ x "!"))
    `);
		expect(res).toBe("v!");
	});
});


describe("11. Error Conditions", () => {

	test("unknown head type", () => {
		// simulate invalid AST
		const ast: any = { type: "List", head: { type: "StringLiteral", value: "x" }, args: [] };
		expect(() => evaluate(ast, execTag)).toThrow();
	});

	test("unexpected EOF in list", () => {
		expect(() => parse(lex("(foo"))).toThrow();
	});

	test("unterminated string", () => {
		expect(() => lex("\"abc")).toThrow();
	});

	test("macro expansion error messages", () => {
		execTag.mockImplementationOnce(() => { throw new Error("boom"); });
		expect(() => evalSrc(`
			(defmacro explode () (boom))
			(explode)
		`)).toThrow();
	});

	test("let binding errors", () => {
		expect(() => evalSrc("(let (x) x)")).toThrow();
	});

	test("quote/unquote nesting errors", () => {
		expect(() => evalSrc(",foo")).toThrow();
	});
});

describe("11b. Additional Error Conditions", () => {

	test("extra tokens after valid form", () => {
		expect(() => parse(lex("(foo) (bar)"))).toThrow(/unexpected tokens at end/i);
	});

	test("invalid macro use (calling non-macro as macro)", () => {
		expect(() => evalSrc("(foo 1 2 3)")).not.toThrow(); // normal tag
		// but something like (macrocall ...) where macrocall isn't defined as macro
		// is just a normal tag call; semantics okay.
	});

	test("quasiquote/unquote mismatched", () => {
		expect(() => evalSrc("(quasiquote (unquote x y))")).toThrow();
	});
});


/**
 * Entry wrapper tests (runLeveretLisp) – alias + $ expansion + reply payload.
 * Comment out if you don't keep runLeveretLisp as exported.
 */
describe("12. Entry wrapper / runLeveretLisp", () => {
	test("basic runLeveretLisp evaluates expression and replies", () => {
		const msg = {
			content: "%t leveretlisp (+ \"a\" \"b\")",
			reply: jest.fn()
		};
		const util = {
			executeTagSafe: execTag
		};
		(runLeveretLisp as any)("(+ \"a\" \"b\")", msg, util);
		expect(msg.reply).toHaveBeenCalledWith({ content: "ab" });
	});

	test("alias and $ replacement logic", () => {
		const msg = {
			content: "%t leveretlisp ALIAS rest",
			reply: jest.fn()
		};
		const util = {
			executeTagSafe: execTag
		};

		// Simulate tag + args as wrapper would have them:
		(global as any).tag = { name: "leveretlisp", args: "$ world" };
		(global as any).msg = msg;
		(global as any).util = util;

		// If you also have the actual IIFE tag wrapper, this is where it runs.
		// Here we only test runLeveretLisp itself:
		runLeveretLisp("(+ \"hello\" \" world\")", msg, util);
		expect(msg.reply).toHaveBeenCalledWith({ content: "hello world" });
	});

	test("code fence input is unwrapped", () => {
		const msg = {
			content: "%t leveretlisp ```lisp (fact \"5\")```",
			reply: jest.fn()
		};
		const util = { executeTagSafe: execTag };

		const fenced = `
			\`\`\`lisp
			(defmacro fact (n)
				(if (= ,n "0")
					"1"
					(* ,n (fact (- ,n "1")))))

			(fact "5")
			\`\`\`
		`;

		(runLeveretLisp as any)(fenced, msg, util);
		expect(msg.reply).toHaveBeenCalledWith({ content: "120" });
	});

	test("alias doesn't strip arguments", () => {
		const msg = { content: '%t ls "1"', reply: jest.fn() };
		const util = { executeTagSafe: execTag };

		(global as any).tag = { name: "leveretlisp", args: '"1"' };
		(global as any).msg = msg;
		(global as any).util = util;

		jest.isolateModules(() => {
			require("../src/entry");
		});

		expect(msg.reply).toHaveBeenCalledWith({ content: "1" });
	});

	test("object payloads are passed through", () => {
		const payload = { text: "", file: { name: "caption.png", data: [""] } };
		const msg = { content: "%t leveretlisp (upload)", reply: jest.fn() };
		const util = { executeTagSafe: jest.fn().mockReturnValue(payload) };

		(runLeveretLisp as any)("(upload)", msg, util);
		expect(msg.reply).toHaveBeenCalledWith(payload);
	});

	test("alias without args passes literal string", () => {
		const msg = { content: "%t ls hello", reply: jest.fn() };
		const util = { executeTagSafe: execTag };

		(global as any).tag = { name: "leveretlisp", args: "" };
		(global as any).msg = msg;
		(global as any).util = util;

		jest.isolateModules(() => {
			require("../src/entry");
		});

		expect(msg.reply).toHaveBeenCalledWith({ content: "hello" });
	});

	test("plain args without alias are wrapped as string literal", () => {
		const msg = { content: "%t bvc2 genesis 1:1", reply: jest.fn() };
		const util = { executeTagSafe: execTag };

		// Simulate alias execution where host already routed to this tag
		(global as any).tag = { name: "bvc2", args: "genesis 1:1" };
		(global as any).msg = msg;
		(global as any).util = util;

		jest.isolateModules(() => {
			require("../src/entry");
		});

		expect(msg.reply).toHaveBeenCalledWith({ content: "genesis 1:1" });
	});

	test("alias with expression form and $ characters stays raw", () => {
		const expr = `(echo (awk "/$/ Math.random() < 0.5 ? 'fart smella': 'smart fella'"))`;
		const msg = { content: `%t ls ${expr}`, reply: jest.fn() };
		const util = { executeTagSafe: execTag };

		(global as any).tag = { name: "leveretlisp", args: "" };
		(global as any).msg = msg;
		(global as any).util = util;

		jest.isolateModules(() => {
			require("../src/entry");
		});

		// awk tag gets full string with quotes intact; echo returns that result
		expect(execTag).toHaveBeenCalledWith("awk", expect.stringContaining("fart smella"));
		expect(msg.reply).toHaveBeenCalledTimes(1);
	});
});

describe("13. Fibonacci", () => {

	beforeEach(() => execTag.mockClear());

	test("fib(10) produces 55", () => {
		const src = `
            (defmacro fib (n)
              (if (= ,n "0")
                  "0"
                (if (= ,n "1")
                    "1"
                  (+ (fib (- ,n "1"))
                     (fib (- ,n "2"))))))

            (fib "10")
        `;

		const result = evalSrc(src);

		expect(result).toBe("55");
	});

});

describe("14. Factorial", () => {

	test("fact(5) produces 120", () => {
		const src = `
            (defmacro fact (n)
              (if (= ,n "0")
                  "1"
                (* ,n (fact (- ,n "1")))))

            (fact "5")
        `;

		const result = evalSrc(src);

		expect(result).toBe("120");
	});

});

describe("15. Common Lisp Compatibility (subset)", () => {

	test("when macro behaves like CL when", () => {
		const src = `
            (defmacro when (cond body)
              \`(if ,cond ,body))

            (when (= "1" "1") "ok")
        `;
		expect(evalSrc(src)).toBe("ok");

		const srcFalse = `
            (defmacro when (cond body)
              \`(if ,cond ,body))

            (when (= "1" "2") "ok")
        `;
		expect(evalSrc(srcFalse)).toBeNull();
	});

	test("and/or/not mirror CL truthiness rules", () => {
		expect(evalSrc("(and \"a\" \"b\")")).toBe(true);
		expect(evalSrc("(and \"a\" \"\")")).toBe(false);
		expect(evalSrc("(or \"\" \"b\")")).toBe(true);
		expect(evalSrc("(or \"\" \"\")")).toBe(false);
		expect(evalSrc("(not \"\")")).toBe(true);
		expect(evalSrc("(not \"x\")")).toBe(false);
	});

	test("= compares stringified values", () => {
		expect(evalSrc("(= \"1\" \"1\")")).toBe(true);
		expect(evalSrc("(= \"foo\" \"bar\")")).toBe(false);
	});

	test("cond behaves like CL cond", () => {
		const src = `
            (cond
              ((= "1" "2") "no")
              ((= "2" "3") "nope")
              (t "ok"))
        `;
		expect(evalSrc(src)).toBe("ok");

		const srcNoDefault = `
            (cond
              ((= "1" "2") "no")
              ((= "2" "3") "nope"))
        `;
		expect(evalSrc(srcNoDefault)).toBeNull();
	});

	test("cond => behaves like CL cond arrow", () => {
		const src = `
            (defmacro echo (x) x)
            (cond
              ("a" => echo)
              (t "no"))
        `;
		expect(evalSrc(src)).toBe("a");
	});

	test("case dispatches by equality", () => {
		const src = `
            (case "b"
              ("a" "no")
              ("b" "yes")
              (otherwise "nope"))
        `;
		expect(evalSrc(src)).toBe("yes");

		const srcDefault = `
            (case "z"
              ("a" "no")
              (t "default"))
        `;
		expect(evalSrc(srcDefault)).toBe("default");
	});

	test("cons/car/cdr/list work for list operations", () => {
		const resList = evalSrc("(list \"1\" \"2\" \"3\")");
		expect(resList).toEqual(["1", "2", "3"]);

		const resCons = evalSrc("(cons \"0\" (list \"1\" \"2\"))");
		expect(resCons).toEqual(["0", "1", "2"]);

		const resCar = evalSrc("(car (list \"a\" \"b\"))");
		expect(resCar).toBe("a");

		const resCdr = evalSrc("(cdr (list \"a\" \"b\" \"c\"))");
		expect(resCdr).toEqual(["b", "c"]);
	});

	test("mapcar applies builtin over list", () => {
		const res = evalSrc("(mapcar 'upper (list \"a\" \"b\"))");
		expect(res).toEqual(["A", "B"]);
	});

	test("assoc finds pair in alist", () => {
		const res = evalSrc("(assoc \"b\" (list (list \"a\" \"1\") (list \"b\" \"2\")))");
		expect(res).toEqual(["b", "2"]);
		const resNil = evalSrc("(assoc \"z\" (list (list \"a\" \"1\")))");
		expect(resNil).toBeNull();
	});

	test("numeric comparisons for random splits", () => {
		expect(evalSrc("(< \"0.4\" \"0.5\")")).toBe(true);
		expect(evalSrc("(< \"0.6\" \"0.5\")")).toBe(false);
		expect(evalSrc("(>= \"1\" \"1\")")).toBe(true);
	});

	test("let* alias and sequential binding semantics", () => {
		const src = `
            (let* ((a "1")
                   (b (+ a "2")))
              b)
        `;
		expect(evalSrc(src)).toBe("12");
	});

	test("eq compares symbols by identity/name", () => {
	const resSame = evalSrc("(eq 'foo 'foo)");
	const resDiff = evalSrc("(eq 'foo 'bar)");
	expect(resSame).toBe(true);
	expect(resDiff).toBe(false);
});


describe("16. Tail Call Optimization", () => {
	test("tail-recursive macro unwinds without stack overflow", () => {
		const res = evalSrc(`
      (defmacro countdown (n)
        (if (= ,n "0")
            "done"
            (countdown (- ,n "1"))))

      (countdown "50")
    `);

		expect(res).toBe("done");
	});

	test("tail recursion preserves accumulator across many steps", () => {
		const res = evalSrc(`
      (defmacro sum-down (n acc)
        (if (= ,n "0")
            acc
            (sum-down (- ,n "1") (+ acc n))))

      (sum-down "100" "0")
    `);

		expect(res).toBe("5050");
	});

	test("mutual tail recursion between macros", () => {
		const res = evalSrc(`
      (defmacro even? (n)
        (if (= ,n "0")
            "true"
            (odd? (- ,n "1"))))

      (defmacro odd? (n)
        (if (= ,n "0")
            "false"
            (even? (- ,n "1"))))

      (even? "200")
    `);

		expect(res).toBe("true");
	});

	test("tail recursion runs at very deep depth without stack overflow", () => {
		const res = evalSrc(`
      (defmacro countdown (n)
        (if (= ,n "0")
            "ok"
            (countdown (- ,n "1"))))

      (countdown "1500")
    `);

		expect(res).toBe("ok");
	});
});


describe("17. JIT cache", () => {
	test("simple application is compiled and reused", () => {
		jitStats.compiles = 0;
		jitStats.hits = 0;

		const ast = parse(lex("(+ \"a\" \"b\" \"c\")"));
		const first = evaluate(ast, execTag);
		const beforeHits = jitStats.hits;
		const second = evaluate(ast, execTag);

		expect(first).toBe("abc");
		expect(second).toBe("abc");
		expect(jitStats.compiles).toBe(1);
		expect(jitStats.hits).toBeGreaterThan(beforeHits);
	});

	test("JIT can be disabled via config", () => {
		jitStats.compiles = 0;
		jitStats.hits = 0;

		const ast = parse(lex("(+ \"1\" \"2\")"));
		const first = evaluate(ast, execTag, undefined, undefined, false, { enableJit: false });
		const second = evaluate(ast, execTag, undefined, undefined, false, { enableJit: false });

		expect(first).toBe("12");
		expect(second).toBe("12");
		expect(jitStats.compiles).toBe(0);
		expect(jitStats.hits).toBe(0);
	});
});

describe("18. Loop (Common Lisp style subset)", () => {
	test("inclusive to loop collects values", () => {
		const res = evalSrc("(loop for i from \"1\" to \"3\" collect i)");
		expect(res).toEqual(["1", "2", "3"]);
	});

	test("below loop with positive step", () => {
		const res = evalSrc("(loop for i from \"5\" below \"9\" by \"2\" collect i)");
		expect(res).toEqual(["5", "7"]);
	});

	test("descending loop with to and negative step", () => {
		const res = evalSrc("(loop for i from \"3\" to \"1\" by \"-1\" collect i)");
		expect(res).toEqual(["3", "2", "1"]);
	});

	test("while loop collects until condition false", () => {
		const res = evalSrc(`
      (let ((x "0"))
        (loop while (< x "3") collect (set! x (- x "-1"))))
    `);
		expect(res).toEqual(["1", "2", "3"]);
	});
});

describe("19. Integrated stress programs", () => {
	test("triangle numbers up to 200", () => {
		const res = evalSrc(`
      (let ((sum "0"))
        (loop for i from "1" to "200" collect
          (let ((n (- sum (- "0" i))))
            (set! sum n))))
    `);
		expect(res.slice(0, 5)).toEqual(["1", "3", "6", "10", "15"]);
		expect(res[res.length - 1]).toBe("20100");
	});

	test("map/filter style transformation", () => {
		const res = evalSrc(`
      (let ((out (loop for i from "1" to "6" collect (* i i))))
        (join "," out))
    `);
		expect(res).toBe(JSON.stringify(["1","4","9","16","25","36"]));
	});

	test("alist reshape and aggregation", () => {
		const res = evalSrc(`
      (let ((pairs (list
        (list "name" "bob")
        (list "score" "90")
        (list "city" "x"))))
        (let ((name "") (score "0"))
        (loop for idx from "0" below (get pairs "length") collect
            (let ((pair (get pairs idx)))
              (case (car pair)
                ("name" (set! name (car (cdr pair))))
                ("score" (set! score (car (cdr pair))))))
            )
          (concat name ":" score)))
    `);
		expect(res).toEqual("bob:90");
	});

	test("nested loops with conditional aggregation", () => {
		const res = evalSrc(`
      (let ((rows (list
        (list "alice" "10" "20")
        (list "bob" "5" "15")
        (list "carol" "7" "30")))
        (tot "0")
        (hi ""))
        (loop for r from "0" below (get rows "length") collect
            (let ((row (get rows r))
                (name (get row "0"))
                (a (get row "1"))
                (b (get row "2")))
            (let ((sum (- a (- "0" b))))
              (set! tot (- tot (- "0" sum)))
              (if (> a "6")
                  (set! hi (concat hi name ";"))
                  hi)
              sum)))
        (list tot hi))
    `);
		expect(res).toEqual(["87", "alice;carol;"]);
	});

	test("JSON decode/encode with numeric processing", () => {
		const res = evalSrc(`
      (let ((payload (json_parse "{\\\"items\\\":[1,2,3,4,5]}"))
            (acc "0"))
        (loop for i from "0" below (get (get payload "items") "length") collect
          (let ((v (get (get payload "items") i)))
            (set! acc (- acc (- "0" v)))
            (number v)))
        (json_stringify (list "sum" acc)))
    `);
		expect(res).toBe(JSON.stringify(["sum", "15"]));
	});
});
});
