import { terser } from "rollup-plugin-terser";

export default {
  input: "build/exec/kb",
  output: [
    { file: "build/dist/kb.js", format: "cjs" },
    { file: "build/dist/kb.min.js", format: "cjs", plugins: [terser()] }
  ],
};
