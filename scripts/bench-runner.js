import { register } from "ts-node";

register({
	transpileOnly: true,
	compilerOptions: {
		module: "ESNext",
		moduleResolution: "NodeNext",
		verbatimModuleSyntax: false
	},
	experimentalResolver: true,
	experimentalSpecifierResolution: "node"
});

await import("./bench.ts");
