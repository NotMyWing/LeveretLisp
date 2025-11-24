import { AnyNode, ASTNode, ProgramNode } from "./ast";
import { builtins } from "./builtins";

export function toStringValue(v: any): string {
	if (typeof v === "string") return v;
	if (v == null) return "";
	if (typeof v === "object") {
		try { return JSON.stringify(v); }
		catch { return String(v); }
	}
	return String(v);
}

export interface TagExecutor {
	(tagName: string, argsString: string): any;
}

const hasBuiltin = (name: string): boolean =>
	Object.prototype.hasOwnProperty.call(builtins, name);

const isTruthy = (v: any): boolean =>
	!(v === null || v === undefined || v === "" || v === false || v === 0 || v === "false");

class Env {
	parent?: Env;
	bindings: Map<string, any>;

	constructor(parent?: Env) {
		this.parent = parent;
		this.bindings = new Map();
	}

	has(name: string): boolean {
		if (this.bindings.has(name)) return true;
		return this.parent ? this.parent.has(name) : false;
	}

	get(name: string): any {
		if (this.bindings.has(name)) return this.bindings.get(name);
		if (this.parent) return this.parent.get(name);
		throw new Error(`unbound symbol: ${name}`);
	}

	set(name: string, value: any) {
		this.bindings.set(name, value);
	}
}

class Macro {
	params: string[];
	body: ASTNode[];
	env: Env;

	constructor(params: string[], body: ASTNode[], env: Env) {
		this.params = params;
		this.body = body;
		this.env = env;
	}

	expand(args: ASTNode[], macroEnv: Map<string, Macro>, callerBindings?: Map<string, ASTNode>): ASTNode {
		if (args.length !== this.params.length) {
			throw new Error(`macro arity mismatch: expected ${this.params.length} got ${args.length}`);
		}

		const bindings = new Map<string, ASTNode>();
		this.params.forEach((p, idx) => {
			const arg = callerBindings ? substitute(args[idx], callerBindings, false) : args[idx];
			bindings.set(p, cloneAst(arg));
		});

		const toAst = (v: any): ASTNode => astFromValue(v);

		function substitute(node: ASTNode, env: Map<string, ASTNode>, inQuasi: boolean): ASTNode {
			switch (node.type) {
				case "StringLiteral": return cloneAst(node);
				case "Nil": return { type: "Nil" };
				case "Symbol":
					if (!inQuasi && env.has(node.name)) return cloneAst(env.get(node.name)!);
					return cloneAst(node);
				case "List": {
					if (node.head.type === "Symbol") {
						const name = node.head.name;
						if (name === "quote") {
							return { type: "List", head: cloneAst(node.head), args: node.args.map(cloneAst) };
						}
						if (name === "quasiquote") {
							if (node.args.length !== 1) throw new Error("quasiquote takes one argument");
							return buildList([cloneAst(node.head), quasiquote(node.args[0], env)]);
						}
						if (name === "unquote") {
							if (node.args.length !== 1) throw new Error("unquote expects one argument");
							return substitute(node.args[0], env, false);
						}
						if (!inQuasi && macroEnv.has(name)) {
							const m = macroEnv.get(name)!;
							const expanded = m.expand(node.args.map(a => substitute(a, env, false)), macroEnv, env);
							return substitute(expanded, env, false);
						}
					}
					return {
						type: "List",
						head: substitute(node.head, env, inQuasi),
						args: node.args.map(a => substitute(a, env, inQuasi))
					};
				}
			}
		}

		function quasiquote(node: ASTNode, env: Map<string, ASTNode>): ASTNode {
			if (node.type === "List") {
				if (node.head.type === "Symbol" && node.head.name === "unquote") {
					if (node.args.length !== 1) throw new Error("unquote expects one argument");
					return substitute(node.args[0], env, false);
				}
				return buildList([node.head, ...node.args].map(n => quasiquote(n, env)));
			}
			return substitute(node, env, true);
		}

		function evalValue(node: ASTNode): any {
			switch (node.type) {
				case "StringLiteral": return node.value;
				case "Nil": return null;
				case "Symbol": {
					if (bindings.has(node.name)) return evalValue(bindings.get(node.name)!);
					return cloneAst(node);
				}
				case "List": {
					if (node.head.type === "Symbol") {
						const name = node.head.name;
						if (name === "quote") return { type: "List", head: cloneAst(node.head), args: node.args.map(cloneAst) };
						if (name === "quasiquote") return quasiquote(node.args[0], bindings);
						if (name === "unquote") return evalValue(node.args[0]);
						if (name === "if") {
							const cond = evalValue(node.args[0]);
							const truthy = isTruthy(cond);
							const branch = truthy ? node.args[1] : node.args[2];
							return branch ? evalValue(branch) : null;
						}
						if (name === "=") {
							const a = toStringValue(evalValue(node.args[0]));
							const b = toStringValue(evalValue(node.args[1]));
							return a === b ? "true" : "false";
						}
						if (name === "-") {
							const nums = node.args.map(a => Number(evalValue(a)));
							if (nums.some(n => Number.isNaN(n))) return { type: "List", head: cloneAst(node.head), args: node.args.map(sub => substitute(sub, bindings, false)) };
							const res = nums.slice(1).reduce((acc, n) => acc - n, nums[0]);
							return String(res);
						}
						if (name === "+") {
							const vals = node.args.map(a => evalValue(a));
							const nums = vals.map(v => Number(v));
							if (nums.every(n => !Number.isNaN(n))) return String(nums.reduce((a, b) => a + b, 0));
							return vals.map(toStringValue).join("");
						}
						if (name === "gensym") return builtins.gensym();
						if (macroEnv.has(name)) {
							const m = macroEnv.get(name)!;
							const expanded = m.expand(node.args, macroEnv, bindings);
							return evalValue(expanded);
						}
					}
					return {
						type: "List",
						head: toAst(evalValue(node.head)),
						args: node.args.map(a => toAst(evalValue(a)))
					};
				}
			}
		}

		let result: any = null;
		for (const expr of this.body) {
			result = evalValue(expr);
		}

		const astResult = toAst(result);
		if (this.params.length === 0 && astResult.type === "StringLiteral") {
			throw new Error("macro must return AST");
		}
		return astResult;
	}
}

function isProgram(node: AnyNode): node is ProgramNode {
	return (node as any).type === "Program";
}

function cloneAst(node: ASTNode): ASTNode {
	switch (node.type) {
		case "StringLiteral": return { type: "StringLiteral", value: node.value };
		case "Symbol": return { type: "Symbol", name: node.name };
		case "Nil": return { type: "Nil" };
		case "List":
			return {
				type: "List",
				head: cloneAst(node.head),
				args: node.args.map(cloneAst)
			};
	}

	throw new Error(`unknown AST node: ${(node as any).type}`);
}

function astFromValue(v: any): ASTNode {
	if (v && typeof v === "object" && "type" in v) return cloneAst(v as ASTNode);
	if (v === null || typeof v === "undefined") return { type: "Nil" };
	if (typeof v === "number" || typeof v === "boolean") {
		return { type: "StringLiteral", value: String(v) };
	}

	return { type: "StringLiteral", value: toStringValue(v) };
}

function listToArray(node: ASTNode): ASTNode[] {
	if (node.type === "Nil") return [];
	if (node.type !== "List") throw new Error("expected list");
	return [node.head, ...node.args];
}

function buildList(items: ASTNode[]): ASTNode {
	if (items.length === 0) return { type: "Nil" };
	const [head, ...args] = items;
	return { type: "List", head, args };
}

export function evaluate(
	ast: AnyNode,
	execTag: TagExecutor,
	env: Env = new Env(),
	macroEnv: Map<string, Macro> = new Map(),
	_inMacro = false
): any {

	function callTag(name: string, argVals: any[]): any {
		if (argVals.length === 0) return execTag(name, "");

		const parts = argVals.map(toStringValue);
		const argsStr = parts.length === 1 ? parts[0] : parts.join(" ");
		return execTag(name, argsStr);
	}

	function evalSymbol(name: string, scope: Env): any {
		if (scope.has(name)) return scope.get(name);
		if (hasBuiltin(name)) return builtins[name]();
		return callTag(name, []);
	}

	function evalQuote(arg: ASTNode): ASTNode {
		return cloneAst(arg);
	}

	function valueToAst(v: any): ASTNode {
		return astFromValue(v);
	}

	function quasiquote(node: ASTNode): ASTNode {
		if (node.type === "List") {
			if (node.head.type === "Symbol" && node.head.name === "unquote") {
				if (node.args.length !== 1) throw new Error("unquote expects one argument");
				const v = evalWithMacros(node.args[0]);
				return valueToAst(v);
			}
			const items = [node.head, ...node.args].map(quasiquote);
			return buildList(items);
		}
		if (node.type === "Nil") return { type: "Nil" };
		return cloneAst(node);
	}

	function evalLet(args: ASTNode[], scope: Env): any {
		if (args.length < 2) throw new Error("let requires bindings and body");
		const bindingsNode = args[0];
		const body = args.slice(1);
		const bindings = listToArray(bindingsNode);

		const inner = new Env(scope);

		for (const b of bindings) {
			const pair = listToArray(b);
			if (pair.length !== 2 || pair[0].type !== "Symbol")
				throw new Error("invalid let binding");
			const value = evalWithMacros(pair[1], inner);
			inner.set(pair[0].name, value);
		}

		let result: any = null;
		for (const expr of body) {
			result = evalWithMacros(expr, inner);
		}
		return result;
	}

	function defineMacro(args: ASTNode[], scope: Env) {
		if (args.length < 2) throw new Error("defmacro requires name and params");
		const nameNode = args[0];
		if (nameNode.type !== "Symbol") throw new Error("macro name must be symbol");

		const paramsArr = listToArray(args[1]);
		const params: string[] = [];
		for (const p of paramsArr) {
			if (p.type !== "Symbol") throw new Error("macro params must be symbols");
			params.push(p.name);
		}
		const body = args.slice(2);
		const macro = new Macro(params, body, scope);
		macroEnv.set(nameNode.name, macro);
		return null;
	}

	function evalList(head: ASTNode, args: ASTNode[], scope: Env): any {
		if (head.type !== "Symbol")
			throw new Error("list head must be a symbol");

		const name = head.name;

		if (name === "if") {
			if (args.length < 2) throw new Error("if requires a condition and a consequent");
			const cond = evalWithMacros(args[0], scope);
			if (isTruthy(cond)) return evalWithMacros(args[1], scope);
			if (args.length >= 3) return evalWithMacros(args[2], scope);
			return null;
		}

		if (name === "quote") {
			if (args.length !== 1) throw new Error("quote takes one argument");
			return evalQuote(args[0]);
		}

		if (name === "quasiquote") {
			if (args.length !== 1) throw new Error("quasiquote takes one argument");
			return quasiquote(args[0]);
		}

		if (name === "unquote") {
			if (_inMacro && args.length === 1) return evalWithMacros(args[0], scope);
			throw new Error("unquote outside quasiquote");
		}

		if (name === "let") {
			return evalLet(args, scope);
		}

		if (name === "let*") {
			return evalLet(args, scope);
		}

		if (name === "cond") {
			for (const clause of args) {
				const parts = listToArray(clause);
				if (parts.length === 0) throw new Error("cond clause cannot be empty");
				const [test, ...body] = parts;
				const isOtherwise =
					test.type === "Symbol" && (test.name === "t" || test.name === "otherwise");
				const testVal = isOtherwise ? true : evalWithMacros(test, scope);
				if (isTruthy(testVal)) {
					if (body.length === 0) return testVal;
					// (test => expr) support
					if (
						body.length === 2 &&
						body[0].type === "Symbol" &&
						body[0].name === "=>"
					) {
						const target = body[1];
						const callAst: ASTNode = {
							type: "List",
							head: target,
							args: [valueToAst(testVal)]
						};
						return evalWithMacros(callAst, scope);
					}
					let res: any = null;
					for (const expr of body) res = evalWithMacros(expr, scope);
					return res;
				}
			}
			return null;
		}

		if (name === "case") {
			if (args.length < 2) throw new Error("case requires key and clauses");
			const keyVal = evalWithMacros(args[0], scope);
			const clauses = args.slice(1);
			for (const clause of clauses) {
				const items = listToArray(clause);
				if (items.length === 0) throw new Error("case clause cannot be empty");
				const headItem = items[0];
				const body = items.slice(1);
				const isDefault = headItem.type === "Symbol" && (headItem.name === "t" || headItem.name === "otherwise");
				const matchList = headItem.type === "List" ? listToArray(headItem) : [headItem];
				const matched = isDefault || matchList.some(k => toStringValue(evalWithMacros(k, scope)) === toStringValue(keyVal));
				if (matched) {
					if (body.length === 0) return keyVal;
					let res: any = null;
					for (const expr of body) res = evalWithMacros(expr, scope);
					return res;
				}
			}
			return null;
		}

		if (name === "defmacro") {
			return defineMacro(args, scope);
		}

		if (hasBuiltin(name)) {
			const values = args.map(arg => evalWithMacros(arg, scope));
			return builtins[name](...values);
		}

		const values = args.map(arg => evalWithMacros(arg, scope));
		return callTag(name, values);
	}

	function macroExpand(node: ASTNode): ASTNode {
		if (node.type === "List") {
			if (
				node.head.type === "Symbol" &&
				(node.head.name === "quote" || node.head.name === "quasiquote" || node.head.name === "defmacro")
			) {
				return node;
			}
			if (node.head.type === "Symbol" && macroEnv.has(node.head.name)) {
				const m = macroEnv.get(node.head.name)!;
				const expanded = m.expand(node.args, macroEnv);
				return macroExpand(expanded);
			}
			const newHead = macroExpand(node.head);
			const newArgs = node.args.map(macroExpand);
			return { type: "List", head: newHead, args: newArgs };
		}
		return node;
	}

	function evalWithMacros(node: ASTNode, scope: Env = env): any {
		const expanded = macroExpand(node);
		if (
			expanded.type === "List" &&
			expanded.head.type === "Symbol" &&
			expanded.head.name === "quote"
		) {
			return evalQuote(expanded.args[0]);
		}
		return evalNode(expanded, scope);
	}

	function evalNode(node: ASTNode, scope: Env = env): any {
		switch (node.type) {
			case "StringLiteral": return node.value;
			case "Symbol": return evalSymbol(node.name, scope);
			case "List": return evalList(node.head, node.args, scope);
			case "Nil": return null;
		}
	}

	function evalProgram(body: ASTNode[]): any {
		let result: any = null;
		for (const expr of body) {
			result = evalWithMacros(expr, env);
		}
		return result;
	}

	if (isProgram(ast)) {
		return evalProgram(ast.body);
	}

	return evalWithMacros(ast, env);
}
