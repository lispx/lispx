/*
 * REPL Input Streams
 * Copyright (c) 2021 Manuel Simoni
 */

/*
 * Special module for treating asynchronous line based input as
 * synchronous.  See companion code in repl-stream.lispx
 */
export function init_repl_stream(vm)
{
    /*
     * A REPL input buffer asynchronously receives lines from the
     * user, On the Lisp side, we want to have a blocking, synchronous
     * READ function so we can write our REPL in direct style, as
     * +DEITY+ intended.
     *
     * Every time new data arrives, a wake-up function set by Lisp is
     * called.  It contains a saved continuation.
     *
     * Lisp gets a fresh input stream to read the current contents
     * of the input buffer.
     *
     * Once an object has been successfully read, the input buffer
     * is truncated by the amount of data read from the stream.
     *
     * If an object can't be read because there isn't enough data in
     * the buffer yet, Lisp saves the continuation in the wake-up
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
         */
        constructor()
        {
            super();
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

        /*
         * Remove all bytes from of the buffer.  Called by Lisp
         * when it gets a READER-ERROR.
         */
        clear_input_buffer()
        {
            this.buffer = "";
        }
    };

    /*
     * Lisp API
     */

    vm.define_class("repl:input-buffer", vm.REPL_input_buffer);

    vm.define_alien_function("repl:%set-input-buffer-wake-up-function",
                             (buffer, fun) => buffer.set_wake_up_function(fun));

    vm.define_alien_function("repl:%make-input-buffer-stream",
                             (buffer) => buffer.get_input_stream());

    vm.define_alien_function("repl:%truncate-input-buffer",
                             (buffer, stream) => buffer.truncate_input_buffer(stream));

    vm.define_alien_function("repl:%clear-input-buffer",
                             (buffer, stream) => buffer.clear_input_buffer(stream));

}
