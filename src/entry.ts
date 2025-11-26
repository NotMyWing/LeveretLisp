import { runLeveretLisp } from "./leveretlisp.ts";

(function () {
	// The host environment provides: tag, msg, util.
	if (!tag) {
		msg.reply({ content: "usage: `%t leveretlisp (expr ...)`" });
		return;
	}

	let alias: string | undefined;
	if (msg.content) {
		const match = msg.content.match(/^\s*%\s*(?:t|tag)\s+(\S+)(?:\s+(.*))?$/);
		if (match && match[1] !== tag.name) alias = match[2] ?? "";
	}

	let args = tag.args ?? "";
	if (typeof alias !== "undefined") {
		const trimmedAlias = alias.trim();
		const aliasIsForm = !!trimmedAlias && ["(", "`", "'", "\""].includes(trimmedAlias[0]);
		const aliasValue = aliasIsForm ? trimmedAlias : JSON.stringify(trimmedAlias);
		args = args
			? args.replace(/\$/g, () => aliasValue)
			: aliasValue;
	}

	let src = args.trim();
	if (src && !["(", "`", "'", "\""].includes(src[0])) {
		src = JSON.stringify(src);
	}
	if (!src) {
		msg.reply({ content: "leveretlisp: empty expression" });
		return;
	}

	tag.args = src;
	runLeveretLisp(src, msg, util);
})();
