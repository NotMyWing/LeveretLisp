import type { AnyNode, ASTNode, ProgramNode } from "./ast.ts";
import { builtins } from "./builtins.ts";

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

type JitFn = (scope: Env, helpers: {
	callTag: (name: string, vals: any[]) => any;
	evalSymbol: (name: string, scope: Env) => any;
	execTag: TagExecutor;
}) => any;
const jitCache = new WeakMap<object, JitFn>();
export const jitStats = { compiles: 0, hits: 0 };
const disallowedJitHeads = new Set(["if", "quote", "quasiquote", "unquote", "let", "let*", "cond", "case", "defmacro", "loop", "set!"]);
const constFoldBuiltins = new Set(["concat", "+", "-", "*", "=", "<", ">", "<=", ">=", "len", "upper", "lower", "trim"]);

export interface EvaluateOptions {
	enableJit?: boolean;
}

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
		return value;
	}

	update(name: string, value: any) {
		if (this.bindings.has(name)) {
			this.bindings.set(name, value);
			return value;
		}
		if (this.parent) return this.parent.update(name, value);
		throw new Error(`unbound symbol: ${name}`);
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
		const TAIL_CALL = Symbol("tail-call");

		type TailCall = { marker: typeof TAIL_CALL; target: Macro; args: ASTNode[]; caller: Map<string, ASTNode> };
		const isTailCall = (v: any): v is TailCall => v && v.marker === TAIL_CALL;

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

		function evalValue(node: ASTNode, allowTail: boolean, bindings: Map<string, ASTNode>): any {
			switch (node.type) {
				case "StringLiteral": return node.value;
				case "Nil": return null;
				case "Symbol": {
					if (bindings.has(node.name)) return evalValue(bindings.get(node.name)!, allowTail, bindings);
					return cloneAst(node);
				}
				case "List": {
					if (node.head.type === "Symbol") {
						const name = node.head.name;
						if (name === "quote") return { type: "List", head: cloneAst(node.head), args: node.args.map(cloneAst) };
						if (name === "quasiquote") return quasiquote(node.args[0], bindings);
						if (name === "unquote") return evalValue(node.args[0], allowTail, bindings);
						if (name === "if") {
							const cond = evalValue(node.args[0], false, bindings);
							const truthy = isTruthy(cond);
							const branch = truthy ? node.args[1] : node.args[2];
							return branch ? evalValue(branch, allowTail, bindings) : null;
						}
						if (name === "=") {
							const a = toStringValue(evalValue(node.args[0], false, bindings));
							const b = toStringValue(evalValue(node.args[1], false, bindings));
							return a === b ? "true" : "false";
						}
						if (name === "-") {
							const nums = node.args.map(a => Number(evalValue(a, false, bindings)));
							if (nums.some(n => Number.isNaN(n))) return { type: "List", head: cloneAst(node.head), args: node.args.map(sub => substitute(sub, bindings, false)) };
							const res = nums.slice(1).reduce((acc, n) => acc - n, nums[0]);
							return String(res);
						}
						if (name === "+") {
							const vals = node.args.map(a => evalValue(a, false, bindings));
							const nums = vals.map(v => Number(v));
							if (nums.every(n => !Number.isNaN(n))) return String(nums.reduce((a, b) => a + b, 0));
							return vals.map(toStringValue).join("");
						}
						if (name === "gensym") return builtins.gensym();
						if (macroEnv.has(name)) {
							const m = macroEnv.get(name)!;
							if (allowTail) {
								return { marker: TAIL_CALL, target: m, args: node.args, caller: bindings };
							}
							const expanded = m.expand(node.args, macroEnv, bindings);
							return evalValue(expanded, allowTail, bindings);
						}
					}
					return {
						type: "List",
						head: toAst(evalValue(node.head, false, bindings)),
						args: node.args.map(a => toAst(evalValue(a, false, bindings)))
					};
				}
			}
		}

		let nextArgs = args;
		let nextCallerBindings = callerBindings;
		let activeMacro: Macro = this;

		expansion: while (true) {
			if (nextArgs.length !== activeMacro.params.length) {
				throw new Error(`macro arity mismatch: expected ${activeMacro.params.length} got ${nextArgs.length}`);
			}

			const bindings = new Map<string, ASTNode>();
			activeMacro.params.forEach((p, idx) => {
				const arg = nextCallerBindings ? substitute(nextArgs[idx], nextCallerBindings, false) : nextArgs[idx];
				bindings.set(p, cloneAst(arg));
			});

			let result: any = null;
			for (let i = 0; i < activeMacro.body.length; i++) {
				const expr = activeMacro.body[i];
				const val = evalValue(expr, i === activeMacro.body.length - 1, bindings);
				if (isTailCall(val)) {
					activeMacro = val.target;
					nextArgs = val.args;
					nextCallerBindings = val.caller;
					continue expansion;
				}
				result = val;
			}

			const astResult = toAst(result);
			if (activeMacro.params.length === 0 && astResult.type === "StringLiteral") {
				throw new Error("macro must return AST");
			}
			return astResult;
		}
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

function constEval(node: ASTNode): { ok: true; value: any } | { ok: false } {
	switch (node.type) {
		case "StringLiteral": return { ok: true, value: node.value };
		case "Nil": return { ok: true, value: null };
		case "Symbol": return { ok: false };
		case "List": {
			if (node.head.type !== "Symbol") return { ok: false };
			const name = node.head.name;
			if (!constFoldBuiltins.has(name) || !hasBuiltin(name)) return { ok: false };
			const foldedArgs: any[] = [];
			for (const arg of node.args) {
				const c = constEval(arg);
				if (!c.ok) return { ok: false };
				foldedArgs.push(c.value);
			}
			try {
				return { ok: true, value: builtins[name](...foldedArgs) };
			} catch {
				return { ok: false };
			}
		}
	}
}

function compileForJit(node: AnyNode): JitFn | null {
	if ((node as ProgramNode).type === "Program") {
		const prog = node as ProgramNode;
		const compiledBody: JitFn[] = [];
		for (const expr of prog.body) {
			const c = compileForJit(expr);
			if (!c) return null;
			compiledBody.push(c);
		}
		return (scope, helpers) => {
			let res: any = null;
			for (const fn of compiledBody) res = fn(scope, helpers);
			return res;
		};
	}

	const astNode = node as ASTNode;
	const constFolded = constEval(astNode as any);
	if (constFolded.ok) {
		const cached = constFolded.value;
		return () => cached;
	}
	switch (astNode.type) {
		case "StringLiteral": return () => astNode.value;
		case "Nil": return () => null;
		case "Symbol": return (_scope, helpers) => helpers.evalSymbol(astNode.name, _scope);
		case "List": {
			if (astNode.head.type !== "Symbol") return null;
			const name = astNode.head.name;
			if (disallowedJitHeads.has(name)) return null;
			const constArgs: any[] = [];
			let allConstArgs = true;
			for (const arg of astNode.args) {
				const c = constEval(arg);
				if (!c.ok) {
					allConstArgs = false;
					break;
				}
				constArgs.push(c.value);
			}
			const compiledArgs: JitFn[] = [];
			for (const arg of astNode.args) {
				const c = compileForJit(arg);
				if (!c) return null;
				compiledArgs.push(c);
			}
			return (scope, helpers) => {
				const argFns = compiledArgs;
				const arity = argFns.length;
				if (hasBuiltin(name)) {
					const builtinFn = builtins[name];
					switch (arity) {
						case 0: return builtinFn();
						case 1: return builtinFn(argFns[0](scope, helpers));
						case 2: return builtinFn(argFns[0](scope, helpers), argFns[1](scope, helpers));
						case 3: return builtinFn(argFns[0](scope, helpers), argFns[1](scope, helpers), argFns[2](scope, helpers));
						default: {
							const vals = new Array(arity);
							for (let i = 0; i < arity; i++) vals[i] = argFns[i](scope, helpers);
							return builtinFn(...vals);
						}
					}
				}
				if (allConstArgs) {
					if (arity === 0) return helpers.execTag(name, "");
					if (arity === 1) return helpers.execTag(name, toStringValue(constArgs[0]));
					return helpers.execTag(name, constArgs.map(toStringValue).join(" "));
				}
				switch (arity) {
					case 0: return helpers.execTag(name, "");
					case 1: {
						const v0 = argFns[0](scope, helpers);
						return helpers.execTag(name, toStringValue(v0));
					}
					case 2: {
						const v0 = argFns[0](scope, helpers);
						const v1 = argFns[1](scope, helpers);
						return helpers.execTag(name, `${toStringValue(v0)} ${toStringValue(v1)}`);
					}
					case 3: {
						const v0 = argFns[0](scope, helpers);
						const v1 = argFns[1](scope, helpers);
						const v2 = argFns[2](scope, helpers);
						return helpers.execTag(
							name,
							`${toStringValue(v0)} ${toStringValue(v1)} ${toStringValue(v2)}`
						);
					}
					default: {
						const vals = new Array(arity);
						for (let i = 0; i < arity; i++) vals[i] = argFns[i](scope, helpers);
						return helpers.callTag(name, vals);
					}
				}
			};
		}
	}
	return null;
}

function getJit(node: AnyNode): JitFn | null {
	const cached = jitCache.get(node as any);
	if (cached) return cached;
	const compiled = compileForJit(node);
	if (compiled) {
		jitCache.set(node as any, compiled);
		jitStats.compiles++;
		return compiled;
	}
	return null;
}

export function evaluate(
	ast: AnyNode,
	execTag: TagExecutor,
	env: Env = new Env(),
	macroEnv: Map<string, Macro> = new Map(),
	_inMacro = false,
	options: EvaluateOptions = {}
): any {
	const trace: string[] = [];
	const runWithTrace = <T>(label: string, fn: () => T): T => {
		trace.push(label);
		try {
			return fn();
		} catch (e: any) {
			if (!e || (e as any)._leveretlispTrace) throw e;
			(e as any)._leveretlispTrace = true;
			const traceLines = trace.join(" > ");
			e.message = `${e.message}\nTrace: ${traceLines}`;
			throw e;
		} finally {
			trace.pop();
		}
	};
	const describeNode = (node: AnyNode): string => {
		if ((node as ProgramNode).type === "Program") return "Program";
		const n = node as ASTNode;
		if (n.type === "List" && n.head.type === "Symbol") return `(${n.head.name})`;
		if (n.type === "Symbol") return n.name;
		if (n.type === "StringLiteral") return `"${n.value}"`;
		return n.type;
	};

	const useJit = options.enableJit !== false;
	const jitHelpers = {
		callTag: (name: string, vals: any[]) => callTag(name, vals),
		evalSymbol: (name: string, scope: Env) => evalSymbol(name, scope),
		execTag
	};
	if (useJit && macroEnv.size === 0) {
		const direct = getJit(ast);
		if (direct) {
			jitStats.hits++;
			return direct(env, jitHelpers);
		}
	}

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

	function evalLoop(args: ASTNode[], scope: Env): any {
		let idx = 0;
		const expectSymbol = (node: ASTNode, text: string) => {
			if (node.type !== "Symbol" || node.name !== text) throw new Error(`loop: expected ${text}`);
		};

		if (args.length < 2) throw new Error("loop: invalid form");

		const results: any[] = [];
		const loopEnv = new Env(scope);

		if (args[0].type === "Symbol" && args[0].name === "while") {
			// (loop while <test> collect <expr>)
			if (args.length < 3) throw new Error("loop while: expected test and collect");
			const testExpr = args[1];
			let collectExpr: ASTNode | undefined;
			for (let j = 2; j < args.length; j++) {
				if (args[j].type === "Symbol" && args[j].name === "collect") {
					collectExpr = args[j + 1];
					break;
				}
			}
			if (!collectExpr) throw new Error("loop while: missing collect expression");
			while (isTruthy(evalWithMacros(testExpr, loopEnv))) {
				const val = evalWithMacros(collectExpr, loopEnv);
				results.push(val);
			}
			return results;
		}

		// for/below/to form
		if (args.length < 7) throw new Error("loop: invalid form");
		expectSymbol(args[idx++], "for");
		const varNode = args[idx++];
		if (varNode.type !== "Symbol") throw new Error("loop: expected loop variable");
		expectSymbol(args[idx++], "from");
		const startNode = args[idx++];
		const relNode = args[idx++];
		if (relNode.type !== "Symbol" || (relNode.name !== "to" && relNode.name !== "below")) {
			throw new Error("loop: expected to/below");
		}
		const endNode = args[idx++];
		let stepNode: ASTNode | undefined;
		if (args[idx] && args[idx].type === "Symbol" && args[idx].name === "by") {
			idx++;
			stepNode = args[idx++];
		}
		expectSymbol(args[idx++], "collect");
		const collectNode = args[idx];
		if (!collectNode) throw new Error("loop: missing collect expression");

		const startNum = Number(evalWithMacros(startNode, scope));
		const endNum = Number(evalWithMacros(endNode, scope));
		if (Number.isNaN(startNum) || Number.isNaN(endNum)) throw new Error("loop: invalid numbers");
		const stepVal = stepNode ? Number(evalWithMacros(stepNode, scope)) : (startNum <= endNum ? 1 : -1);
		if (Number.isNaN(stepVal) || stepVal === 0) throw new Error("loop: invalid step");
		const inclusive = relNode.name === "to";

		for (let i = startNum; ; i += stepVal) {
			const done = stepVal > 0
				? inclusive ? i > endNum : i >= endNum
				: inclusive ? i < endNum : i <= endNum;
			if (done) break;

			loopEnv.set(varNode.name, String(i));
			const val = evalWithMacros(collectNode, loopEnv);
			results.push(val);
		}

		return results;
	}

	function evalSet(args: ASTNode[], scope: Env): any {
		if (args.length !== 2) throw new Error("set!: expected name and value");
		const nameNode = args[0];
		if (nameNode.type !== "Symbol") throw new Error("set!: name must be symbol");
		const value = evalWithMacros(args[1], scope);
		return scope.update(nameNode.name, value);
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

		if (name === "loop") {
			return evalLoop(args, scope);
		}

		if (name === "set!") {
			return evalSet(args, scope);
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
			const headChanged = newHead !== node.head;
			const argsChanged = newArgs.length !== node.args.length || newArgs.some((a, idx) => a !== node.args[idx]);
			if (!headChanged && !argsChanged) return node;
			return { type: "List", head: newHead, args: newArgs };
		}
		return node;
	}

	function evalWithMacros(node: ASTNode, scope: Env = env): any {
		return runWithTrace(describeNode(node), () => {
			const expanded = macroExpand(node);
			if (
				expanded.type === "List" &&
				expanded.head.type === "Symbol" &&
				expanded.head.name === "quote"
			) {
				return evalQuote(expanded.args[0]);
			}
			if (useJit) {
				const maybeJit = expanded === node ? getJit(node) : getJit(expanded);
				if (maybeJit) {
					jitStats.hits++;
					return maybeJit(scope, jitHelpers);
				}
			}
			return evalNode(expanded, scope);
		});
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
		if (useJit) {
			const maybeJit = getJit(ast as any);
			if (maybeJit) {
				jitStats.hits++;
				return maybeJit(env, jitHelpers);
			}
		}
		return evalProgram(ast.body);
	}

	return evalWithMacros(ast, env);
}
