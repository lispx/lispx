import fs from "fs";
import readline from "readline";
import { exit } from "process";
import { VM } from "../../../dist/lispx-vm.min.mjs";
import { init_repl_stream } from "../repl-stream.mjs";

/*
 * Init the VM and load the REPL code.
 */

var vm = new VM();
init_repl_stream(vm);
const repl_code = fs.readFileSync("../repl.lispx", "utf-8");
const repl_stream_code = fs.readFileSync("../repl-stream.lispx", "utf-8");
vm.eval_js_string(repl_code);
vm.eval_js_string(repl_stream_code);

/*
 * Set up Lisp standard output to print to the Node stdout.
 */

vm.STANDARD_OUTPUT.set_value(new vm.JS_console_output_stream(function (output) {
    process.stdout.write(output.to_js_string());
}));

/*
 * Set up Lisp standard input to come from the input buffer.
 */

var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: "* "
});

const input_buffer = new vm.REPL_input_buffer(() => rl.prompt());
vm.STANDARD_INPUT.set_value(input_buffer);

rl.on("line", function(line) {
    input_buffer.add_line(vm.str(line + "\n"));
    /*
     * Force all output after each evaluation.
     */
    vm.STANDARD_OUTPUT.get_value().force_output();
});

/*
 * Run the REPL.
 */

vm.define_alien_function("repl:%set-debug-level", (level) => {
});

vm.eval_js_string("(repl:run)");