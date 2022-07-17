/*
 * LispX JavaScript Console Output
 * Copyright (c) 2022 Manuel J. Simoni
 */

/*
 * This file implements an output stream that prints to the JS
 * console.
 *
 * The JS console does not allow appending to the current line -- it
 * only supports outputting a full line by itself.  This means that
 * sometimes, such as when the user does a FORCE-OUTPUT and there is
 * some buffered data, we will have to output a full line, even though
 * there is no newline in the actual data written by the user.
 *
 * The code currently does not specifically handle CR or LF output, so
 * printing those might lead to weird results.
 *
 * The code currently does not force the output by itself, so
 * FORCE-OUTPUT (or a function that calls it, like PRINT) must be
 * called from time to time.
 */
export function init_js_console(vm)
{
    vm.JS_console_output_stream = class JS_console_output_stream extends vm.Output_stream
    {
        constructor()
        {
            super();
            /*
             * UTF-8 bytes.
             */
            this.buffer = "";
            /*
             * Remembers if we are at the start of a line.
             */
            this.line_is_fresh = true;
        }

        write_byte(b)
        {
            vm.assert_type(b, "string");
            vm.assert(b.length === 1);
            this.buffer += b;
            this.line_is_fresh = (b === "\n");
            return b;
        }

        fresh_line()
        {
            if (this.line_is_fresh) {
                return vm.f();
            } else {
                this.write_byte("\n");
                return vm.t();
            }
        }

        force_output()
        {
            if (this.buffer.length > 0) {
                console.log(vm.utf8_decode(this.buffer));
                this.line_is_fresh = true;
                this.buffer = "";
            }
            return vm.void();
        }
    };

    vm.define_class("js-console-output-stream", vm.JS_console_output_stream, vm.Output_stream);

    /*
     * Register a JS console output stream as standard output.
     */
    vm.STANDARD_OUTPUT.set_value(new vm.JS_console_output_stream());
};
