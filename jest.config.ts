import type { Config } from "jest";

const config: Config = {
	verbose: true,
	testEnvironment: "node",

	// Glob for your test files
	testMatch: ["**/test/**/*.test.ts"],

	// Use ts-jest to handle TypeScript
	transform: {
		"^.+\\.tsx?$": ["ts-jest", {}],
	},

	moduleFileExtensions: ["ts", "js", "json"],

	// If needed for path aliases:
	// moduleNameMapper: { "^src/(.*)$": "<rootDir>/src/$1" },
};

export default config;
