/*
 * LispX Reader
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds reading functionality to a virtual machine.
 *
 * ----
 *
 * A note on bytes versus characters: Unlike Common Lisp, LispX
 * currently has no notion of characters, it knows only (UTF-8) bytes.
 * Streams are byte-based, too.
 *
 * The reader code exploits the fact that ASCII characters encode to
 * the same code units in UTF-16 and UTF-8, and uses literals like "("
 * directly.
 *
 * This whole setup works passably because all syntactically
 * meaningful (aka macro) characters like (, ), ", etc are from the
 * ASCII range.  It would not work well if we wanted to be able to do
 * things like use non-ASCII characters as macro characters.
 *
 * The code sometimes uses the term character for a byte, but only
 * for ASCII bytes, so it's not really wrong.
 */
export function init_read(vm)
{
    /*** Reader Algorithm ***/

    /*
     * Cache symbols that get used repeatedly.
     */
    const DOT = vm.sym(".");
    const QUOTE = vm.sym("quote");

    /*
     * Main reader entry point and recursively used read function.
     *
     * Attempts to read one object from the stream.
     *
     * If EOF is reached before an object has been read, and
     * eof_error_p is true, throws an error.  If false, returns
     * eof_value.  eof_error_p defaults to true.  eof_value defaults
     * to #VOID.
     *
     * It throws an error, regardless of eof_error_p, if the stream
     * ends in the middle of an object representation.  For example,
     * if a stream does not contain enough right parentheses to
     * balance the left parentheses in it, read throws an error.
     *
     * The consing dot as a token is invalid at this main entry point,
     * because it makes sense only inside lists, of course.
     */
    vm.read = (stream, eof_error_p = true, eof_value = vm.void()) =>
    {
        vm.assert_type(stream, vm.Input_stream);
        vm.assert_type(eof_error_p, "boolean");
        vm.assert_type(eof_value, vm.TYPE_ANY);

        /*
         * The reason for this dance with a unique EOF value is quite
         * nerdy, which, given the fact that we are talking about a
         * Lisp interpreter, is really nerdy.
         *
         * Why can't we just check that the read object isn't the dot?
         * Well, because the luser might have passed in the dot symbol
         * as their eof_value.  Unlikely?  Heckin yes.  But we're not
         * from New Jersey.
         */
        const unique_eof = {};
        const obj = read_allowing_dot(stream, false, unique_eof);
        if (obj === unique_eof)
            return vm.eof(eof_error_p, eof_value);
        else if (obj === DOT)
            throw new vm.Reader_error("Consing dot not allowed here");
        else
            return obj;
    };

    /*
     * Main body of the reader code without prohibition against the
     * consing dot as a token.
     *
     * This is used solely inside read_list() directly, where the
     * consing dot is allowed; all other places use read(), which
     * wraps around this and disallows the dot.
     */
    function read_allowing_dot(stream, eof_error_p = true, eof_value = vm.void())
    {
        const b = vm.skip_whitespace(stream, false);
        if (b === vm.void()) {
            return vm.eof(eof_error_p, eof_value);
        } else {
            switch (b) {
            case "(":
                return vm.read_delimited_list(stream, ")");
            case "\"":
                return read_string(stream);
            case "'":
                return read_quote(stream);
            case ":":
                return read_namespaced_symbol(stream, vm.KEYWORD_NAMESPACE);
            case "#":
                return read_sharpsign(stream);
            case "|":
                return read_escaped_symbol(stream);
            case ")":
                throw new vm.Reader_error("Unbalanced parenthesis");
            default:
                if (vm.is_macro_character(b)) {
                    return vm.call_reader_macro(stream, b);
                } else {
                    stream.unread_byte(b);
                    return read_token(stream);
                }
            }
        }
    }

    /*
     * Signalled when a syntax error occurs.
     */
    vm.Reader_error = class Lisp_reader_error extends vm.Error
    {
        constructor(message)
        {
            super(message);
        }
    };

    /*** Reading Tokens ***/

    /*
     * Reads a token, that is, either a number or an unescaped symbol.
     *
     * There must be at least one byte in the stream, which is ensured
     * by the caller, read_allowing_dot().
     */
    function read_token(stream)
    {
        let bytes = "";
        while (true) {
            const b = stream.read_byte(false);
            if ((b === vm.void()) || vm.is_whitespace(b)) {
                // The token is terminated by EOF or whitespace.
                break;
            } else if (vm.is_terminating_character(b)) {
                // The token is terminated by a terminating character.
                // Unread it.
                stream.unread_byte(b);
                break;
            } else if (b === "\\") {
                // Escape character.  Throws an EOF error if there is
                // no escaped character following.
                bytes += read_escape_character(stream);
            } else {
                // Normal character.
                bytes += b;
            }
        }
        if (vm.parses_as_number(bytes))
            return vm.num(bytes);
        else
            return vm.intern(new vm.String(bytes));
    }

    /*
     * Reads an escape char after \.
     *
     * Throws an error if EOF is reached.
     */
    function read_escape_character(stream)
    {
        const b = stream.read_byte(true);
        switch (b) {
        case 'n':  return '\n';
        case 't':  return '\t';
        case '"':  return '"';
        case '\\': return '\\';
        case '|':  return '|';
        default:   throw new vm.Reader_error(`Invalid escape character ${b}`);
        }
    }

    /*
     * Returns true if a byte terminates a token, false otherwise.
     */
    vm.is_terminating_character = (b) =>
    {
        switch(b) {
        case "(":
        case ")":
        case ";":
        case "\"":
        case "\'":
        case "|":
            return true;
        default:
            return vm.is_terminating_macro_character(b);
        }
    };

    /*
     * Returns true if the bytes can be parsed as a number, false
     * otherwise.
     */
    vm.parses_as_number = (bytes) =>
    {
        return /^-?\d+(\.\d+)?$/.test(bytes);
    };

    /*** Reading Lists ***/

    /*
     * Reads a list terminated by the end character, typically ")".
     *
     * Throws an EOF error if the list is incomplete.
     */
    vm.read_delimited_list = (stream, end_char) =>
    {
        // We keep track of the list start and end with two mutable
        // references.
        let start = vm.nil();
        let end = start;
        while (true) {
            // Read the next byte.
            const b = vm.skip_whitespace(stream, true);
            if (b === end_char) {
                // It's a closing parenthesis, the list ends.
                return start;
            } else {
                // Otherwise, read the list element.
                stream.unread_byte(b);
                const obj = read_allowing_dot(stream, true);
                if (obj === DOT) {
                    // We have a consing dot.
                    if (start === vm.nil()) {
                        // Can't be the first element of the list.
                        throw new vm.Reader_error("Consing dot at start of list");
                    } else {
                        // The consing dot must be followed by a
                        // single object, which becomes the cdr of the
                        // list.
                        end.set_cdr(vm.read(stream, true));
                        // The list must end now or it's an error.
                        const c = vm.skip_whitespace(stream, true);
                        if (c === end_char)
                            return start;
                        else
                            throw new vm.Reader_error("Multiple objects after consing dot");
                    }
                } else {
                    // We have a normal list element, cons it up.
                    const cons = vm.cons(obj, vm.nil());
                    if (start === vm.nil()) {
                        // It's the first element, remember it in start.
                        start = cons;
                        end = start;
                    } else {
                        // It's a later element, add it at the end.
                        end.set_cdr(cons);
                        end = cons;
                    }
                }
            }
        }
    }

    /*** Reading Strings and Escaped Symbols ***/

    /*
     * Reads a string.
     */
    function read_string(stream)
    {
        return read_delimited(stream, "\"");
    }

    /*
     * Reads an escaped symbol.
     */
    function read_escaped_symbol(stream)
    {
        return vm.intern(read_delimited(stream, "|"));
    }

    /*
     * Reading code for strings and escaped symbols.
     *
     * Returns a string.
     *
     * Throws an error if EOF is reached.
     */
    function read_delimited(stream, end_char)
    {
        let bytes = "";
        while (true) {
            const b = stream.read_byte(true);
            if (b === end_char) {
                break;
            } else if (b === "\\") {
                bytes += read_escape_character(stream);
                continue;
            } else {
                bytes += b;
                continue;
            }
        }
        return new vm.String(bytes);
    }

    /*** Reading Quote ***/

    /*
     * 'FOO reads as (quote FOO).
     */
    function read_quote(stream)
    {
        return vm.list(QUOTE, vm.read(stream, true));
    }

    /*** Reading Sharpsign ***/

    /*
     * The sharpsign (#) introduces various special syntaxes.
     */
    function read_sharpsign(stream)
    {
        const b = stream.read_byte(true);
        if (is_alpha(b)) {
            stream.unread_byte(b);
            return read_constant(stream);
        } else {
            switch (b) {
            case "'":
                return read_namespaced_symbol(stream, vm.FUNCTION_NAMESPACE);
            default:
                throw new vm.Reader_error(`Illegal dispatching character ${b}`);
            }
        }
    }

    /*
     * Test whether a byte can be considered alphabetic for the
     * purposes of parsing sharpsign constants like #T.
     */
    function is_alpha(b)
    {
        return /^[a-z]$/.test(b);
    }

    /*
     * Reads a constant like #T.
     */
    function read_constant(stream)
    {
        const sym = vm.read(stream, true);
        vm.assert_type(sym, vm.Symbol);
        const constant = CONSTANTS[sym.get_string().get_utf8_bytes()];
        if (constant)
            return constant;
        else
            throw new vm.Reader_error("Illegal constant");
    }

    /*
     * If a symbol immediately follows # it must be one of those.
     */
    const CONSTANTS = {
        "t": vm.t(),
        "f": vm.f(),
        "nil": vm.nil(),
        "void": vm.void(),
        "ignore": vm.ignore(),
    };

    /*
     * Reads namespaced symbols, such as #'FUNCTION and :KEYWORD symbols.
     */
    function read_namespaced_symbol(stream, namespace)
    {
        const sym = vm.read(stream, true);
        vm.assert_type(sym, vm.Symbol);
        return sym.to_namespace(namespace);
    }

    /*** Skipping Whitespace and Comments ***/

    /*
     * Skips over whitespace and comments and returns the first
     * non-whitespace byte afterwards.
     */
    vm.skip_whitespace = (stream, eof_error_p = true, eof_value = vm.void()) =>
    {
        while (true) {
            const b = stream.read_byte(false);
            if (b === vm.void()) {
                // We have reached EOF, do the usual handling.
                return vm.eof(eof_error_p, eof_value);
            } else if (vm.is_whitespace(b)) {
                // It's whitespace, go to next byte.
                continue;
            } else if (b === ";") {
                // It starts a line comment.
                line_comment: while (true) {
                    const c = stream.read_byte(false);
                    switch (c) {
                    case vm.void():
                        // We have reached EOF, do the usual handling.
                        return vm.eof(eof_error_p, eof_value);
                    case "\n":
                        // It's a newline, the line comment is over.
                        break line_comment;
                    default:
                        // It's any other byte, the line comment continues.
                        continue line_comment;
                    }
                }
            } else if (b === "#") {
                // It might start a block comment.
                const c = stream.read_byte(false);
                if (c === "|") {
                    // Bingo.  Skip the comment and continue after it.
                    skip_block_comment(stream);
                    continue;
                } else if (c === vm.void()) {
                    // Nope, the EOF comes after the #,
                    // so it's the first non-whitespace byte.
                    return b;
                } else {
                    // Nope, some other byte comes after the #,
                    // so it's the first non-whitespace byte.
                    // Unread the other byte.
                    stream.unread_byte(c);
                    return b;
                }
            } else {
                // We have found the first non-whitespace byte.
                return b;
            }
        }
    }

    /*
     * Skips #| block comments #| which may nest |# |#.
     *
     * The initial #| has already been read when this is called.
     */
    function skip_block_comment(stream)
    {
        while (true) {
            const b = stream.read_byte(true);
            if (b === "|") {
                // Character might introduce ending |#
                if (stream.read_byte(true) === "#")
                    break;
                else
                    continue;
            } else if (b === "#") {
                // Character might introduce nested comment #|
                if (stream.read_byte(true) === "|")
                    skip_block_comment(stream);
                continue;
            } else {
                // Anything else just skip.
                continue;
            }
        }
    }

    /*
     * Returns true if a byte is whitespace, false otherwise.
     */
    vm.is_whitespace = (b) =>
    {
        switch(b) {
        case " ":
        case "\n":
        case "\t":
            return true;
        default:
            return false;
        }
    };

    /*** Macro Characters ***/

    /*
     * This is a very bare-bones facility for extending the syntax.
     */

    const READTABLE = {};

    /*
     * Set function as reader macro for the character.
     *
     * It will be called with the input stream and macro character,
     * and should return an option containing the read object.  (In
     * the future it will also be possible to return nil for things
     * like comments, but currently the option must not be nil.)
     */
    vm.set_macro_character = (b, fun, non_terminating_p = false) =>
    {
        READTABLE[b] = { fun: fun, non_terminating_p: non_terminating_p };
    };

    /*
     * Remove the reader macro for the character.
     */
    vm.unset_macro_character = (b) =>
    {
        delete READTABLE[b];
    };

    /*
     * Return true if the character is a macro character, false otherwise.
     */
    vm.is_macro_character = (b) =>
    {
        return READTABLE[b] !== undefined;
    };

    /*
     * Return true if the character is a terminating macro character,
     * false otherwise.
     */
    vm.is_terminating_macro_character = (b) =>
    {
        return vm.is_macro_character(b) && !READTABLE[b].non_terminating_p;
    };

    /*
     * Call the reader macro for the character.
     */
    vm.call_reader_macro = (stream, b) =>
    {
        // Extract content of option
        return READTABLE[b].fun(stream, b).car();
    };

    /*** Read+Eval Utilities ***/

    /*
     * Reads forms from a stream until EOF and evaluates them
     * in a given environment.
     *
     * The environment defaults to the VM's root environment.
     *
     * Returns the value of the last expression or #VOID if the stream
     * is empty.
     */
    vm.eval_stream = function(stream, env = vm.get_environment())
    {
        vm.assert_type(stream, vm.Input_stream);
        vm.assert_type(env, vm.Environment);
        const unique_eof = {};
        let result = vm.void();
        while (true) {
            const form = vm.read(stream, false, unique_eof);
            if (form === unique_eof) {
                break;
            } else {
                /*
                 * Note that we are using vm.eval_form() instead of
                 * vm.eval() here so that suspensions cause an error.
                 */
                result = vm.eval_form(form, env);
            }
        }
        return result;
    }

    /*
     * Evaluates all forms in a string in a given environment.
     *
     * The environment defaults to the VM's root environment.
     */
    vm.eval_string = function(string, env = vm.get_environment())
    {
        return vm.eval_stream(new vm.String_input_stream(string), env);
    }

    /*
     * Evaluates all forms in a JS string in a given environment.
     *
     * The environment defaults to the VM's root environment.
     */
    vm.eval_js_string = function(js_string, env = vm.get_environment())
    {
        return vm.eval_string(vm.str(js_string), env);
    }

    /*** Lisp API ***/

    vm.define_condition("reader-error", vm.Reader_error, vm.Error);

    vm.define_alien_function("%%read", (stream, eof_error_p, eof_value) => {
        vm.assert_type(eof_error_p, vm.Boolean);
        return vm.read(stream, eof_error_p.to_js_boolean(), eof_value);
    });

};

/*
 * Notes
 * -----
 *
 * Using undefined as eof_value will cause #VOID to be
 * used.  I am ignoring this.
 */
