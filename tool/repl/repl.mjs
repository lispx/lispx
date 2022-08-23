import { VM } from "../../dist/lispx-vm.min.mjs";
import { init_repl_stream } from "./repl_stream.mjs";
import repl_stream_code from "./repl_stream.lispx";
import repl_code from "./repl.lispx";

const PROMPT = "* ";

$(function() {

    const vm = new VM();
    init_repl_stream(vm);

    const term = $('#terminal').terminal(input_handler, {
        greetings: "Welcome to Nybble Lisp!"
    });
    term.set_prompt(PROMPT);

    const stdout = new vm.JS_console_output_stream(term.echo);
    vm.STANDARD_OUTPUT.set_value(stdout);

    const stdin = new vm.REPL_input_buffer();
    vm.STANDARD_INPUT.set_value(stdin);

    function input_handler(line)
    {
        stdin.add_line(vm.str(line + "\n"));
        /*
         * Force any available output after each input.
         * Note sure if this is really needed anymore but can't hurt.
         */
        stdout.force_output();
    }

    // Adapt REPL prompt based on debug level.
    vm.define_alien_function("repl:%set-debug-level", (level) => {
        const lvl = vm.assert_type(level, vm.Number).to_js_number();
        if (lvl === 0) term.set_prompt(PROMPT);
        else term.set_prompt("[" + lvl + "] ");
    });

    vm.eval_js_string(repl_stream_code);
    vm.eval_js_string(repl_code);
    vm.eval_js_string("(repl:main)");

});
