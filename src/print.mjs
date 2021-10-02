/*
 * LispX Printer
 * Copyright (c) 2021 Manuel Simoni
 */

/*
 * Adds printing functionality to a virtual machine.
 */
export function init_print(vm)
{
    /*** Printer Algorithm ***/

    /*
     * If true, objects are printed so that they can be read back in
     * by READ. If false, objects are printed so that they look good
     * on the teletype.
     */
    vm.PRINT_ESCAPE = vm.make_dynamic(vm.t());

    /*
     * How many levels should be printed before abbreviating with "#"?
     */
    vm.PRINT_LEVEL = vm.make_dynamic(vm.f());

    /*
     * The current level we are printing at, to be compared against
     * *PRINT-LEVEL*.  Increased every time WRITE is entered, so
     * effectively starts at 0 for the outermost object.
     */
    vm.CURRENT_PRINT_LEVEL = vm.make_dynamic(vm.num(-1));

    /*
     * Write object to stream.  Main printer entry point.
     */
    vm.write = (object, stream) =>
    {
        vm.assert_type(object, vm.TYPE_ANY);
        vm.assert_type(stream, vm.Output_stream);
        call_with_increased_current_print_level(() => {
            if (vm.is_lisp_object(object))
                object.write_object(stream);
            else
                vm.write_js_object(object, stream);
        });
        return object;

        /*
         * On every call to WRITE, increase the current print level.
         */
        function call_with_increased_current_print_level(thunk)
        {
            const new_level = vm.add(vm.CURRENT_PRINT_LEVEL.get_value(), vm.num(1));
            vm.progv([vm.CURRENT_PRINT_LEVEL], [new_level], thunk);
        }
    };

    /*
     * All non-Lisp objects print as "#<object>" for now.
     */
    vm.write_js_object = (object, stream) =>
    {
        vm.write_unreadable_object(object, stream);
    };

    /*
     * Write an unreadable object.  If thunk is non-null, it can
     * print extra stuff, cf. CL's PRINT-UNREADABLE-OBJECT.
     */
    vm.write_unreadable_object = (object, stream, thunk = null) =>
    {
        vm.assert_type(object, vm.TYPE_ANY);
        vm.assert_type(stream, vm.Output_stream);
        stream.write_string(vm.str("#<"));
        stream.write_string(vm.class_of(object).get_name().get_string());
        if (thunk !== null) {
            thunk();
        }
        stream.write_byte(">");
    };

    /*** Built-In Object Writing Functions ***/

    vm.Object.prototype.write_object = function(stream)
    {
        vm.write_unreadable_object(this, stream);
    };

    vm.Boolean.prototype.write_object = function(stream)
    {
        stream.write_string((this === vm.t()) ? vm.str("#t") : vm.str("#f"));
    };

    vm.Nil.prototype.write_object = function(stream)
    {
        stream.write_string(vm.str("#nil"));
    };

    vm.Void.prototype.write_object = function(stream)
    {
        stream.write_string(vm.str("#void"));
    };

    vm.Ignore.prototype.write_object = function(stream)
    {
        stream.write_string(vm.str("#ignore"));
    };

    vm.Number.prototype.write_object = function(stream)
    {
        stream.write_string(this.to_string());
    };

    vm.Symbol.prototype.write_object = function(stream)
    {
        if (vm.PRINT_ESCAPE.get_value() === vm.t()) {
            stream.write_string(get_symbol_prefix(this));
            if (symbol_needs_escaping(this)) {
                write_delimited(this.get_string(), stream, "|");
            } else {
                stream.write_string(this.get_string());
            }
        } else {
            stream.write_string(this.get_string());
        }
    };

    vm.String.prototype.write_object = function(stream)
    {
        if (vm.PRINT_ESCAPE.get_value() === vm.t()) {
            write_delimited(this, stream, "\"");
        } else {
            stream.write_string(this);
        }
    };

    /*
     * Writes the contents of strings and escaped symbols.
     */
    function write_delimited(string, stream, delimiter)
    {
        stream.write_byte(delimiter);
        const bytes = string.get_utf8_bytes();
        for (let i = 0; i < bytes.length; i++) {
            const b = bytes[i];
            if ((b === delimiter) || (b === "\\")) {
                stream.write_byte("\\");
                stream.write_byte(b);
            } else {
                stream.write_byte(b);
            }
        }
        stream.write_byte(delimiter);
    }

    /*
     * Returns the prefix identifying the namespace of the symbol.
     */
    function get_symbol_prefix(sym)
    {
        switch (sym.get_namespace()) {
        case vm.VARIABLE_NAMESPACE: return vm.str("");
        case vm.FUNCTION_NAMESPACE: return vm.str("#'");
        case vm.CLASS_NAMESPACE: return vm.str("#$");
        case vm.KEYWORD_NAMESPACE: return vm.str(":");
        default: vm.panic("Unknown symbol namespace");
        }
    }

    /*
     * A symbol needs escaping if it could be parsed as a number, or
     * if it contains whitespace or terminating characters, or the
     * escape character '\'.
     */
    function symbol_needs_escaping(sym)
    {
        const bytes = sym.get_string().get_utf8_bytes();
        if (vm.parses_as_number(bytes))
            return true;
        else
            for (let i = 0; i < bytes.length; i++) {
                const b = bytes[i];
                if (vm.is_whitespace(b)
                    || vm.is_terminating_character(b)
                    || b === "\\")
                    return true;
            }
        return false;
    }

    vm.Cons.prototype.write_object = function(stream)
    {
        function write_cons(cons, stream)
        {
            if (cons.cdr() === vm.nil()) {
                vm.write(cons.car(), stream);
            } else if (cons.cdr() instanceof vm.Cons) {
                vm.write(cons.car(), stream);
                stream.write_byte(" ");
                write_cons(cons.cdr(), stream);
            } else {
                vm.write(cons.car(), stream);
                stream.write_string(vm.str(" . "));
                vm.write(cons.cdr(), stream);
            }
        }
        maybe_abbreviate_object_based_on_current_print_level(stream, () => {
            stream.write_byte("(");
            write_cons(this, stream);
            stream.write_byte(")");
        });
    };

    vm.Standard_object.prototype.write_object = function(stream)
    {
        const prefix = "lisp_slot_";
        maybe_abbreviate_object_based_on_current_print_level(stream, () => {
            vm.write_unreadable_object(this, stream, () => {
                for (const name of Object.getOwnPropertyNames(this).sort()) {
                    if (name.startsWith(prefix)) {
                        stream.write_byte(" ");
                        vm.write(vm.kwd(name.slice(prefix.length)), stream);
                        stream.write_byte(" ");
                        vm.write(this[name], stream);
                    }
                }
            });
        });
    };

    /*
     * Utility for abbreviating objects based on their nesting level.
     *
     * Call the thunk if *PRINT-LEVEL* is false, or if
     * *CURRENT-PRINT-LEVEL* is less than *PRINT-LEVEL*.
     *
     * Otherwise, print "#" to the stream.
     */
    function maybe_abbreviate_object_based_on_current_print_level(stream, thunk)
    {
        const print_level = vm.PRINT_LEVEL.get_value();
        const current_print_level = vm.CURRENT_PRINT_LEVEL.get_value();
        if ((print_level === vm.f())
            || (vm.compare(current_print_level, print_level) < 0)) {
            thunk();
        } else {
            stream.write_byte("#");
        }
    }

    /*** Lisp API ***/

    vm.define_variable("*print-escape*", vm.PRINT_ESCAPE);

    vm.define_variable("*print-level*", vm.PRINT_LEVEL);

    vm.define_alien_function("%%write", (object, stream) => vm.write(object, stream));

};
