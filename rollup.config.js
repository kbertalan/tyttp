import { terser } from "rollup-plugin-terser";

export default {
  input: "build/exec/tyttp",
  output: [
    { file: "build/dist/tyttp.js", format: "cjs" },
    { file: "build/dist/tyttp.min.js", format: "cjs", plugins: [terser()] }
  ],
};
