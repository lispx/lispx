import fs from "fs";
import readline from "readline";
import { VM } from "../../../src/vm.mjs";
import { init_repl_input_buffer } from "../repl-input-buffer.mjs";
import repl_input_buffer_code from "../repl-input-buffer.lispx";
import repl_code from "../repl.lispx";

/*
 * Init the VM and load the REPL code.
 */

const PROMPT = "* ";

var vm = new VM();
init_repl_input_buffer(vm);

/*
 * Set up Lisp standard output to print to the Node stdout.
 */

vm.STANDARD_OUTPUT.set_value(new vm.JS_console_output_stream(function (output) {
    process.stdout.write(output + "\n");
}));

/*
 * Set up Lisp standard input to come from the input buffer.
 */

var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: PROMPT
});

const input_buffer = new vm.REPL_input_buffer();
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

vm.define_alien_function("repl:%display-prompt", (level) => {
    const lvl = vm.assert_type(level, vm.Number).to_js_number();
    if (lvl === 0) rl.setPrompt(PROMPT);
    else rl.setPrompt("[" + lvl + "] ");
    rl.prompt();
});

process.stdout.write("Welcome to LispX!\n");

vm.eval_js_string(repl_code);
vm.eval_js_string(repl_input_buffer_code);
vm.eval_js_string("(repl:run)");
