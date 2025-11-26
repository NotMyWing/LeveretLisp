import { toStringValue } from "./evaluator.ts";

export interface Builtins {
	[name: string]: (...args: any[]) => any;
}

let gensymCounter = 0;

const isTruthy = (v: any): boolean =>
	!(v === null || v === undefined || v === "" || v === false || v === 0 || v === "false");

const toNumber = (value: any, label: string) => {
	const n = Number(value);
	if (Number.isNaN(n)) {
		throw new Error(`${label}: invalid number`);
	}
	return n;
};

const getHttp = () => {
	const http = (globalThis as any).http;
	if (!http || typeof http.request !== "function") {
		throw new Error("http.request not available");
	}
	return http;
};

const toFunction = (v: any) => {
	if (typeof v === "function") return v;
	if (v && typeof v === "object" && (v as any).type === "Symbol" && builtins[(v as any).name]) {
		return builtins[(v as any).name];
	}
	if (typeof v === "string" && builtins[v]) return builtins[v];
	return undefined;
};

export const builtins: Builtins = {
	concat(...args: any[]) {
		return args.map(toStringValue).join("");
	},

	"+": function (...args: any[]) {
		return args.map(toStringValue).join("");
	},

	"-": function (...args: any[]) {
		if (args.length === 0) return "0";
		const nums = args.map(n => {
			const v = Number(n);
			if (Number.isNaN(v)) throw new Error("invalid number");
			return v;
		});
		const res = nums.slice(1).reduce((acc, n) => acc - n, nums[0]);
		return String(res);
	},

	"*": function (...args: any[]) {
		if (args.length === 0) return "1";
		const nums = args.map((n, idx) => toNumber(n, `*:${idx}`));
		const res = nums.reduce((acc, n) => acc * n, 1);
		return String(res);
	},

	"=": function (a: any, b: any) {
		return toStringValue(a) === toStringValue(b);
	},

	unembed_desc(value: any) {
		if (
			value &&
			typeof value === "object" &&
			value.embed &&
			typeof value.embed.description !== "undefined"
		) {
			return String(value.embed.description);
		}
		return "";
	},

	gensym() {
		const id = `g__${++gensymCounter}`;
		return { type: "Symbol", name: id };
	},

	len(value: any = "") {
		return String(toStringValue(value).length);
	},

	upper(value: any = "") {
		return toStringValue(value).toUpperCase();
	},

	lower(value: any = "") {
		return toStringValue(value).toLowerCase();
	},

	trim(value: any = "") {
		return toStringValue(value).trim();
	},

	substr(value: any, start: any, length?: any) {
		const s = toStringValue(value);
		const from = toNumber(start, "substr:start");
		if (typeof length === "undefined") return s.substr(from);
		const len = toNumber(length, "substr:length");
		return s.substr(from, len);
	},

	replace(value: any, find: any, repl: any = "") {
		const source = toStringValue(value);
		const needle = toStringValue(find);
		const replacement = toStringValue(repl);
		return source.split(needle).join(replacement);
	},

	join(delimiter: any, ...parts: any[]) {
		const sep = toStringValue(delimiter ?? "");
		return parts.map(toStringValue).join(sep);
	},

	split(value: any, delimiter: any) {
		const source = toStringValue(value);
		const sep = toStringValue(delimiter ?? "");
		return sep === "" ? source.split("") : source.split(sep);
	},

	starts_with(value: any, search: any) {
		return toStringValue(value).startsWith(toStringValue(search));
	},

	ends_with(value: any, search: any) {
		return toStringValue(value).endsWith(toStringValue(search));
	},

	includes(value: any, search: any) {
		return toStringValue(value).includes(toStringValue(search));
	},

	and(...vals: any[]) {
		return vals.every(isTruthy);
	},

	or(...vals: any[]) {
		return vals.some(isTruthy);
	},

	not(val: any) {
		return !isTruthy(val);
	},

	coalesce(...vals: any[]) {
		const found = vals.find(isTruthy);
		if (typeof found === "undefined") return "";
		return typeof found === "string" ? found : toStringValue(found);
	},

	json_parse(input: any) {
		const text = toStringValue(input);
		return JSON.parse(text);
	},

	json_stringify(input: any) {
		return JSON.stringify(input);
	},

	number(value: any) {
		return String(toNumber(value, "number"));
	},

	"<": function (a: any, b: any) {
		return toNumber(a, "<:a") < toNumber(b, "<:b");
	},

	">": function (a: any, b: any) {
		return toNumber(a, ">:a") > toNumber(b, ">:b");
	},

	"<=": function (a: any, b: any) {
		return toNumber(a, "<=:a") <= toNumber(b, "<=:b");
	},

	">=": function (a: any, b: any) {
		return toNumber(a, ">=:a") >= toNumber(b, ">=:b");
	},

	msg() {
		return (globalThis as any).msg;
	},

	util() {
		return (globalThis as any).util;
	},

	tag() {
		return (globalThis as any).tag;
	},

	overclocking() {
		return (globalThis as any).overclocking;
	},

	http() {
		return (globalThis as any).http;
	},

	http_request(config: any) {
		const http = getHttp();
		let cfg: any = config;
		if (typeof cfg === "string") {
			const trimmed = cfg.trim();
			if (trimmed.startsWith("{")) {
				cfg = JSON.parse(trimmed);
			} else if (trimmed.startsWith("http://{") || trimmed.startsWith("https://{")) {
				const brace = trimmed.indexOf("{");
				const jsonChunk = trimmed.slice(brace);
				try { cfg = JSON.parse(jsonChunk); }
				catch { cfg = trimmed; }
			} else if (/^https?:\/\//i.test(trimmed)) {
				cfg = trimmed;
			} else if (trimmed.includes("{")) {
				const brace = trimmed.indexOf("{");
				const jsonChunk = trimmed.slice(brace);
				try { cfg = JSON.parse(jsonChunk); }
				catch { cfg = trimmed; }
			} else {
				cfg = trimmed;
			}
		}
		return http.request(cfg);
	},

	http_get(url: any) {
		const http = getHttp();
		return http.request(toStringValue(url));
	},

	list(...vals: any[]) {
		return vals;
	},

	cons(head: any, tail: any) {
		if (Array.isArray(tail)) return [head, ...tail];
		return [head, tail];
	},

	car(list: any) {
		if (Array.isArray(list)) return list[0] ?? null;
		return null;
	},

	cdr(list: any) {
		if (Array.isArray(list)) return list.slice(1);
		return [];
	},

	mapcar(fn: any, list: any) {
		const f = toFunction(fn);
		if (!f) throw new Error("mapcar requires a function or builtin name");
		if (!Array.isArray(list)) throw new Error("mapcar requires a list");
		return list.map(item => f(item));
	},

	assoc(key: any, alist: any) {
		if (!Array.isArray(alist)) return null;
		const k = toStringValue(key);
		for (const pair of alist) {
			if (Array.isArray(pair) && pair.length >= 2) {
				if (toStringValue(pair[0]) === k) return pair;
			}
		}
		return null;
	},

	eq(a: any, b: any) {
		if (a === b) return true;
		const symbolName = (v: any) => {
			if (v && typeof v === "object" && (v as any).type === "Symbol" && typeof (v as any).name === "string") {
				return (v as any).name;
			}
			if (v && typeof v === "object" && typeof (v as any).name === "string") {
				return (v as any).name;
			}
			return null;
		};
		const sa = symbolName(a);
		const sb = symbolName(b);
		if (sa !== null && sb !== null) return sa === sb;
		if (typeof a === "string" && typeof b === "string") return a.trim() === b.trim();
		return toStringValue(a) === toStringValue(b);
	},

	get(obj: any, key: any) {
		if (obj === null || typeof obj === "undefined") return null;
		const prop = toStringValue(key);
		return (obj as any)[prop];
	}
};
