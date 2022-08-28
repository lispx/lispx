import { VM } from "../../../src/vm.mjs";
import { init_repl_input_buffer } from "../repl-input-buffer.mjs";
import repl_input_buffer_code from "../repl-input-buffer.lispx";
import repl_code from "../repl.lispx";

const PROMPT = "* ";

$(function() {

    const vm = new VM();
    init_repl_input_buffer(vm);

    const term = $('#terminal').terminal(input_handler, {
        greetings: "Welcome to LispX!",
        historySize: 1024,
        keydown: keydown,
        keypress: keypress
    });
    term.set_prompt(PROMPT);

    // Re-use the JS console output stream with our custom output function.
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
    vm.define_alien_function("repl:%display-prompt", (level) => {
        const lvl = vm.assert_type(level, vm.Number).to_js_number();
        if (lvl === 0) term.set_prompt(PROMPT);
        else term.set_prompt("[" + lvl + "] ");
    });

    vm.eval_js_string(repl_input_buffer_code);
    vm.eval_js_string(repl_code);
    vm.eval_js_string("(repl:run)");

});

// Paren matching code adapted from https://terminal.jcubic.pl/examples.php#parenthesis
let position;
let timer;

function keydown() {
    if (position) {
        this.set_position(position);
        position = false;
    }
}

function keypress(e) {
    var term = this;
    if (e.key == ')') {
        setTimeout(function() {
            position = term.get_position();
            var command = term.before_cursor();
            var count = 1;
            var close_pos = position - 1;
            var c;
            while (count > 0) {
                c = command[--close_pos];
                if (!c) {
                    return;
                }
                if (c === '(') {
                    count--;
                } else if (c == ')') {
                    count++;
                }
            }
            if (c == '(') {
                clearTimeout(timer);
                setTimeout(function() {
                    term.set_position(close_pos);
                    timer = setTimeout(function() {
                        term.set_position(position)
                        position = false;
                    }, 200);
                }, 0);
            }
        }, 0);
    } else {
        position = false;
    }
}
