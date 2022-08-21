/*
 * REPL Input and Output Streams
 * Copyright (c) 2021 Manuel Simoni
 */

/*
 * Adds REPL streams to the VM.
 */
export function init_repl_stream(vm)
{
    /*
     * Output stream that buffers incoming bytes and sends them to an
     * output function when forced or when a newline is printed.
     */
    vm.REPL_output_stream = class REPL_output_stream extends vm.Output_stream
    {
        /*
         * Constructs a new REPL output stream that outputs to the
         * given function.
         */
        constructor(output_function)
        {
            super();
            this.output_function = vm.assert_type(output_function, "function");
            this.buffer = "";
            /*
             * This variable keeps track of whether we are at the beginning
             * of a fresh line for purposes of fresh_line().
             */
            this.line_is_fresh = true;
        }

        /*
         * Accumulate a byte.  When a newline is written, force the
         * current buffer contents to the output function.
         */
        write_byte(b)
        {
            vm.assert_type(b, "string");
            vm.assert(b.length === 1);
            this.buffer += b;
            if (b === "\n") {
                this.line_is_fresh = true;
                this.force_output();
            } else {
                this.line_is_fresh = false;
            }
            return b;
        }

        /*
         * If the buffer is non-empty, send it to the output function.
         */
        force_output()
        {
            if (this.buffer !== "") {
                const buffer = this.buffer;
                this.buffer = "";
                this.output_function(new vm.String(buffer));
            }
            return vm.void();
        }

        /*
         * Output stream standard method to print a newline if we
         * aren't already on a fresh line.
         */
        fresh_line()
        {
            if (!this.line_is_fresh) {
                this.write_byte("\n");
                return vm.t();
            } else {
                return vm.f();
            }
        }
    };

    /*
     * A REPL input buffer asynchronously receives lines from the user,
     * for example via Node's readline module.  On the Lisp side,
     * we want to have a blocking, synchronous READ function.
     *
     * Every time new data arrives, a wake-up function set by Lisp is
     * called.  It contains a saved continuation.
     *
     * Lisp gets a fresh input stream to read the current contents
     * of the input buffer.
     *
     * Once an object has been successfully read, the input buffer
     * is truncated by the amount of data read.
     *
     * If an object can't be read because there isn't enough data in
     * the buffer yet, Lisp saves the new continuation in the wake-up
     * function again, and we start over.
     *
     * Note that we set an instance of this input buffer class as
     * standard input on the Lisp side, even though it doesn't even
     * implement vm.Input_stream!  But it's no problem, because we
     * don't expose methods like READ-BYTE to Lisp yet, just READ.
     * And READ internally calls the STREAM-READ method, which we
     * specialize for the input buffer class.
     */
    vm.REPL_input_buffer = class REPL_input_buffer extends vm.Object
    {
        /*
         * Construct a new input buffer.
         *
         * Lisp will call the provided display input function when it
         * is ready to read a form.  It should display a prompt
         * symbol like ">" or "*" on the console.
         */
        constructor(display_input_function = () => null)
        {
            super();
            vm.assert_type(display_input_function, "function");
            this.display_input_function = display_input_function;
            this.buffer = "";
            this.wake_up_function = null;
        }

        /*
         * Set the Lisp function that will be called when new data
         * arrives.  Called by Lisp.
         */
        set_wake_up_function(wake_up_function)
        {
            vm.assert_type(wake_up_function, vm.Function);
            this.wake_up_function = wake_up_function;
        }

        /*
         * Add a new line to the input buffer.  Notify Lisp via the
         * wake-up function.
         */
        add_line(line)
        {
            vm.assert_type(line, vm.String);
            this.buffer += line.get_utf8_bytes();
            if (this.wake_up_function !== null) {
                /*
                 * Reset wake-up function to null so we never
                 * accidentally enter the saved continuation twice.
                 */
                const wuf = this.wake_up_function;
                this.wake_up_function = null;
                vm.eval_form(vm.list(wuf));
            } else {
                console.log("No wakeup function - Lisp crashed?");
            }
        }

        /*
         * Get a fresh input stream that reads the current input
         * buffer contents.  Called by Lisp.
         */
        get_input_stream()
        {
            return new vm.String_input_stream(new vm.String(this.buffer));
        }

        /*
         * Remove the bytes that have been successfully read via the
         * input stream from the front of the buffer.  Called by Lisp
         * once it has read an object.
         */
        truncate_input_buffer(input_stream)
        {
            vm.assert_type(input_stream, vm.String_input_stream);
            vm.assert(input_stream.pos > 0);
            this.buffer = this.buffer.slice(input_stream.pos + 1);
        }
    };

    /*
     * Lisp API
     */

    vm.define_class("repl:input-buffer", vm.REPL_input_buffer, vm.Object);

    vm.define_alien_function("repl:%%set-input-buffer-wake-up-function",
                             (buffer, fun) => buffer.set_wake_up_function(fun));

    vm.define_alien_function("repl:%%make-input-buffer-stream",
                             (buffer) => buffer.get_input_stream());

    vm.define_alien_function("repl:%%truncate-input-buffer",
                             (buffer, stream) => buffer.truncate_input_buffer(stream));

    vm.define_alien_function("repl:%%display-waiting-for-input-sign",
                             (buffer) => buffer.display_input_function());

}
