/*
 * LispX Input and Output Streams
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds stream-related functionality to a VM.
 */
export function init_stream(vm)
{
    /*** Input Streams ***/

    /*
     * Abstract superclass of byte input streams.
     *
     * The API is inspired by Common Lisp's READ-BYTE and UNREAD-BYTE.
     *
     * Bytes are represented as JS strings with one code unit.
     */
    vm.Input_stream = class Lisp_input_stream extends vm.Object
    {
        /*
         * Attempts to read one byte from the stream.
         *
         * If no more bytes are available, and eof_error_p is true,
         * throws an error.  If false, returns eof_value.
         *
         * eof_error_p defaults to true.  eof_value defaults to #VOID.
         */
        read_byte(eof_error_p, eof_value) { vm.abstract_method(); }

        /*
         * Puts the most recently read byte back into the stream.
         *
         * The rules surrounding unreading aren't clearly defined at
         * the moment (see Common Lisp's UNREAD-BYTE for inspiration).
         * To stay on the safe side, always put back only one byte.
         */
        unread_byte(b) { vm.abstract_method(); }

        /*
         * Obtains the next byte in the stream without actually
         * reading it, thus leaving the byte to be read at a
         * later time.
         *
         * If peek_type is true, skips over whitespace.  If false,
         * not.
         */
        peek_byte(peek_type = false, eof_error_p = true, eof_value = vm.void())
        {
            const b = peek_type ? vm.skip_whitespace(this, false) : this.read_byte(false);
            if (b === vm.void()) {
                return vm.eof(eof_error_p, eof_value);
            } else {
                this.unread_byte(b);
                return b;
            }
        }
    };

    /*
     * Input stream that reads UTF-8 bytes from a string in memory.
     */
    vm.String_input_stream = class Lisp_string_input_stream extends vm.Input_stream
    {
        /*
         * Constructs a new string input stream from a Lisp string.
         */
        constructor(string)
        {
            super();
            vm.assert_type(string, vm.String);
            this.bytes = string.get_utf8_bytes();
            this.pos = -1;
        }

        /*
         * See Input_stream.
         */
        read_byte(eof_error_p = true, eof_value = vm.void())
        {
            vm.assert_type(eof_error_p, "boolean");
            vm.assert_type(eof_value, vm.TYPE_ANY);
            if ((this.pos + 1) < this.bytes.length) {
                this.pos++;
                return this.byte_at_pos(this.pos);
            } else {
                return vm.eof(eof_error_p, eof_value);
            }
        }

        /*
         * See Input_stream.
         */
        unread_byte(b)
        {
            vm.assert_type(b, "string");
            if ((this.pos >= 0) && (this.byte_at_pos(this.pos) === b))
                this.pos--;
            else
                throw new vm.Stream_error("Cannot unread byte");
        }

        /*
         * Returns the byte at the given position as a JS string.
         */
        byte_at_pos(pos)
        {
            return this.bytes[pos];
        }
    };

    /*** Output Streams ***/

    /*
     * Abstract superclass of byte output streams.
     *
     * Bytes are represented as JS strings with one code unit.
     */
    vm.Output_stream = class Lisp_output_stream extends vm.Object
    {
        /*
         * Attempts to write one byte to the stream.
         */
        write_byte(b) { vm.abstract_method(); }

        /*
         * Attempts to write the bytes from a string to the stream.
         */
        write_string(str)
        {
            vm.assert_type(str, vm.String);
            const bytes = str.get_utf8_bytes();
            for (let i = 0; i < bytes.length; i++)
                this.write_byte(bytes[i]);
            return str;
        }

        /*
         * Initiates the emptying of any internal buffers but does not
         * wait for completion or acknowledgment to return.
         */
        force_output()
        {
            /* Default implementation does nothing. */
            return vm.void();
        }

        /*
         * Ensure that the following output appears on a new line by
         * itself.  Return true if a newline character was printed,
         * false otherwise.
         */
        fresh_line()
        {
            /* Default implementation always writes newline. */
            this.write_byte("\n");
            return vm.t();
        }
    };

    /*
     * Output stream that writes UTF-8 bytes to a string in memory.
     */
    vm.String_output_stream = class Lisp_string_output_stream extends vm.Output_stream
    {
        /*
         * Constructs a new empty string output stream.
         */
        constructor()
        {
            super();
            this.bytes = "";
        }

        /*
         * Attempts to write one byte to the stream.
         */
        write_byte(b)
        {
            vm.assert_type(b, "string");
            vm.assert(b.length === 1);
            this.bytes += b;
            return b;
        }

        /*
         * Get the current stream contents as a string.
         */
        get_string()
        {
            return new vm.String(this.bytes);
        }
    };

    /*** Errors ***/

    /*
     * Throws an EOF error or returns the eof_value, depending
     * on eof_error_p.
     */
    vm.eof = (eof_error_p, eof_value) =>
    {
        if (eof_error_p)
            throw new vm.End_of_file();
        else
            return eof_value;
    };

    /*
     * Signalled when a stream-related error occurs.
     */
    vm.Stream_error = class Lisp_stream_error extends vm.Error
    {
        constructor(message)
        {
            super(message);
        }
    };

    /*
     * Signalled on EOF.
     */
    vm.End_of_file = class Lisp_end_of_file extends vm.Stream_error
    {
        constructor()
        {
            super("EOF");
        }
    };

    /*** Standard Streams ***/

    vm.STANDARD_INPUT = vm.make_dynamic(vm.void());

    vm.STANDARD_OUTPUT = vm.make_dynamic(vm.void());

    /*** Lisp API ***/

    vm.define_class("input-stream", vm.Input_stream, vm.Object);

    vm.define_class("string-input-stream", vm.String_input_stream, vm.Input_stream);

    vm.define_class("output-stream", vm.Output_stream, vm.Object);

    vm.define_class("string-output-stream", vm.String_output_stream, vm.Output_stream);

    vm.define_class("stream-error", vm.Stream_error, vm.Error, vm.Standard_class);

    vm.define_class("end-of-file", vm.End_of_file, vm.Stream_error, vm.Standard_class);

    vm.define_variable("*standard-input*", vm.STANDARD_INPUT);

    vm.define_variable("*standard-output*", vm.STANDARD_OUTPUT);

    vm.define_alien_function("%%make-string-input-stream", (string) =>
        new vm.String_input_stream(string));

    vm.define_alien_function("%%make-string-output-stream", () =>
        new vm.String_output_stream());

    vm.define_alien_function("%%get-output-stream-string", (sos) =>
        vm.assert_type(sos, vm.String_output_stream).get_string());

    vm.define_alien_function("%%fresh-line", (stream) =>
        vm.assert_type(stream, vm.Output_stream).fresh_line());

};
