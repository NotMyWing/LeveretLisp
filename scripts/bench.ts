import { performance } from "node:perf_hooks";
import { lex } from "../src/lexer.ts";
import { parse } from "../src/parser.ts";
import { evaluate, jitStats } from "../src/evaluator.ts";

type Scenario = { label: string; src: string };
type BenchRow = {
	label: string;
	mode: "jit:on" | "jit:off";
	ms: number;
	compiles: number;
	hits: number;
	result: any;
};

const scenarios: Scenario[] = [
	{ label: "concat-flat", src: "(+ \"a\" \"b\" \"c\" \"d\" \"e\")" },
	{ label: "arith-nested", src: "(+ (* \"2\" \"3\") (* \"4\" \"5\") \"7\")" },
	{ label: "tag-strings", src: "(foo (json_parse \"{\\\"x\\\":1}\") \"tail\")" },
	{ label: "loop-100", src: "(loop for i from \"0\" below \"100\" collect i)" },
	{ label: "loop-while", src: "(let ((x \"0\")) (loop while (< x \"50\") collect (set! x (- x \"-1\"))))" }
];

const execTag = (_name: string, _args: string) => "";

function benchScenario(scenario: Scenario, iterations: number, enableJit: boolean): BenchRow {
	const ast = parse(lex(scenario.src));

	// Warm once to build JIT if enabled; then clear stats to measure steady-state
	evaluate(ast, execTag, undefined, undefined, false, { enableJit });
	jitStats.compiles = 0;
	jitStats.hits = 0;

	let last: any = null;
	const start = performance.now();
	for (let i = 0; i < iterations; i++) {
		last = evaluate(ast, execTag, undefined, undefined, false, { enableJit });
	}
	const ms = performance.now() - start;

	return {
		label: scenario.label,
		mode: enableJit ? "jit:on" : "jit:off",
		ms,
		compiles: jitStats.compiles,
		hits: jitStats.hits,
		result: last
	};
}

async function main() {
	const iterations = Number(process.env.BENCH_ITERS ?? 20000);
	const rows: BenchRow[] = [];

	for (const scenario of scenarios) {
		rows.push(benchScenario(scenario, iterations, true));
		rows.push(benchScenario(scenario, iterations, false));
	}

	console.log(`LeveretLisp benchmark (${iterations} iterations per scenario)`);
	for (const row of rows) {
		console.log(
			`${row.label.padEnd(12)} ${row.mode.padEnd(8)} ${row.ms.toFixed(2).padStart(8)} ms  compiles=${row.compiles} hits=${row.hits}`
		);
	}
}

main().catch(err => {
	console.error(err);
	process.exit(1);
});
