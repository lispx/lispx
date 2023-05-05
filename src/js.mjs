/*
 * LispX JavaScript Interface
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds JS interface functionality to a virtual machine.
 */
export function init_js(vm)
{
    /*
     * Accesses a global variable by name.
     */
    vm.js_global = (name) =>
    {
        vm.assert_type(name, vm.String);
        return globalThis[name.to_js_string()];
    };

    /*
     * Sets a global variable by name.
     */
    vm.js_set_global = (name, value) =>
    {
        vm.assert_type(name, vm.String);
        return globalThis[name.to_js_string()] = value;
    };

    /*
     * Calls a JS constructor with arguments.
     *
     * Note that this is not a fat arrow function because we need
     * access to the arguments object (although it's probably possible
     * to use ... syntax and make this a fat arrow function).
     */
    vm.js_new = function(constructor /* , arg1, ..., argN */)
    {
        vm.assert_type(constructor, "function");
        /*
         * See https://stackoverflow.com/a/23190790
         */
        const factory_function = constructor.bind.apply(constructor, arguments);
        return new factory_function();
    };

    /*
     * Returns a named property of an object.
     */
    vm.js_get = (object, prop_name) =>
    {
        vm.assert_type(prop_name, vm.String);
        return object[prop_name.to_js_string()];
    };

    /*
     * Returns an indexed element of a JS array.
     */
    vm.js_elt = (js_array, index) =>
    {
        vm.assert(Array.isArray(js_array));
        vm.assert_type(index, vm.Number);
        return js_array[index.to_js_number()];
    };

    /*
     * Invokes a JS method on an object from Lisp.
     */
    vm.apply_js_method = (receiver, method_name, args) =>
    {
        vm.assert_type(method_name, vm.String);
        vm.assert_type(args, vm.List);
        /*
         * Could throw a more helpful error here if the method isn't found.
         */
        const method = vm.assert_type(receiver[method_name.to_js_string()], "function");
        return method.apply(receiver, vm.list_to_array(args))
    };

    /*
     * Transforms a JS boolean into a Lisp one.
     */
    vm.to_lisp_boolean = (js_bool) =>
    {
        return vm.assert_type(js_bool, "boolean") ? vm.t() : vm.f();
    };

    /*
     * Makes a JS function callable as a Lisp one.
     */
    vm.to_lisp_function = (js_fun) =>
    {
        return vm.alien_function(js_fun, "anonymous JS function");
    };

    /*
     * Makes a Lisp operator callable as a JS function.
     */
    vm.to_js_function = (operator) =>
    {
        vm.assert_type(operator, vm.Operator);
        return function()
        {
            var args = vm.array_to_list(Array.prototype.slice.call(arguments));
            return vm.operate(operator, args, vm.make_environment());
        };
    };

    /*
     * Output stream that prints to the JS console.
     *
     * The JS console does not allow appending to the current line --
     * it only supports outputting a full line by itself.  This means
     * that sometimes, such as when the user does a FORCE-OUTPUT and
     * there is some buffered data, we will have to output a full
     * line, even though there might be no newline in the actual data
     * written by the user.
     *
     * The code currently does not specifically handle CR or LF
     * output, so printing those might lead to weird results.
     *
     * The code currently does not force the output by itself, so
     * FORCE-OUTPUT (or a function that calls it, like PRINT) must be
     * called from time to time.
     */
    vm.JS_console_output_stream = class Lisp_js_console_output_stream extends vm.Output_stream
    {
        /*
         * The output_function can be overridden for testing and
         * printing to other line-based systems, such as the REPL
         * terminal.
         */
        constructor(output_function = console.log)
        {
            super();
            this.buffer = "";
            this.output_function = output_function;
        }

        /*
         * See stream.mjs for the documentation of the output stream API
         * methods.
         */

        write_byte(b)
        {
            vm.assert_type(b, "string");
            vm.assert(b.length === 1);
            this.buffer += b;
            return b;
        }

        fresh_line()
        {
            /*
             * If the buffer is empty, or the last byte is a newline,
             * we don't need to do anything.
             */
            if ((this.buffer.length === 0)
                || (this.buffer[this.buffer.length - 1] === "\n")) {
                return vm.f();
            } else {
                this.write_byte("\n");
                return vm.t();
            }
        }

        force_output()
        {
            if (this.buffer.length > 0) {
                this.output_function(vm.utf8_decode(this.buffer));
                this.buffer = "";
            }
            return vm.void();
        }
    };

    /*** Lisp API ***/

    vm.define_constant("+js-true+", true);

    vm.define_constant("+js-false+", false);

    vm.define_constant("+js-null+", null);

    vm.define_constant("+js-undefined+", undefined);

    vm.define_alien_function("%js-global", vm.js_global);

    vm.define_alien_function("%js-set-global", vm.js_set_global);

    vm.define_alien_function("%js-new", vm.js_new);

    vm.define_alien_function("%js-get", vm.js_get);

    vm.define_alien_function("%js-elt", vm.js_elt);

    vm.define_alien_function("%to-lisp-boolean", vm.to_lisp_boolean);

    vm.define_alien_function("%to-js-boolean", (bool) =>
        vm.assert_type(bool, vm.Boolean).to_js_boolean());

    vm.define_alien_function("%to-lisp-number", (js_num) =>
        vm.num(vm.assert_type(js_num, "number")));

    vm.define_alien_function("%to-js-number", (num) =>
        vm.assert_type(num, vm.Number).to_js_number());

    vm.define_alien_function("%to-lisp-string", (js_str) =>
        vm.str(vm.assert_type(js_str, "string")));

    vm.define_alien_function("%to-js-string", (str) =>
        vm.assert_type(str, vm.String).to_js_string());

    vm.define_alien_function("%to-lisp-function", vm.to_lisp_function);

    vm.define_alien_function("%to-js-function", vm.to_js_function);

    vm.define_alien_function("%list-to-js-array", vm.list_to_array);

    vm.define_alien_function("%js-array-to-list", vm.array_to_list);

    vm.define_alien_function("%apply-js-method", vm.apply_js_method);

    vm.define_alien_function("%js-log", (...objects) => console.log(...objects));

    vm.define_alien_function("%sleep", (ms) => {
        vm.assert_type(ms, vm.Number);
        return new Promise(resolve => setTimeout(resolve, ms.to_js_number()));
    });

    vm.define_class("js-console-output-stream", vm.JS_console_output_stream, vm.Output_stream);

    /*
     * Register a JS console output stream as standard output.
     */
    vm.STANDARD_OUTPUT.set_value(new vm.JS_console_output_stream());

};
