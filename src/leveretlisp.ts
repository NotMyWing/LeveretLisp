import { lex } from "./lexer";
import { parse } from "./parser";
import { evaluate } from "./evaluator";

export function runLeveretLisp(
	src: string,
	msg: any,
	util: any
) {
	const code = unwrapCodeBlock(src);
	let tokens, ast, value;

	try {
		tokens = lex(code);
		ast = parse(tokens);
		value = evaluate(ast, util.executeTagSafe);
	} catch (e: any) {
		msg.reply({ content: "leveretlisp error: " + e.message });
		return;
	}

	msg.reply(wrapPayload(value));
}

function unwrapCodeBlock(src: string): string {
	const trimmed = src.trim();
	if (!trimmed.startsWith("```")) return trimmed;

	const afterFence = trimmed.slice(3);
	const newline = afterFence.indexOf("\n");
	if (newline === -1) return trimmed;

	// language is optional; ignore whatever appears on the fence line
	const bodyAndClosing = afterFence.slice(newline + 1);
	const closing = bodyAndClosing.lastIndexOf("```");
	if (closing === -1) return trimmed;

	return bodyAndClosing.slice(0, closing).trim();
}

function wrapPayload(v: any) {
	if (typeof v === "string") return { content: v };
	if (v && typeof v === "object") return v;
	return { content: JSON.stringify(v) };
}
