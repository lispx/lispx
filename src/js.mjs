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
     * Transforms a JS boolean into a Lisp one.
     */
    vm.to_lisp_boolean = (js_bool) =>
    {
        return vm.assert_type(js_bool, "boolean") ? vm.t() : vm.f();
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
     * Accesses a global variable by name.
     */
    vm.js_global = (name) =>
    {
        vm.assert_type(name, vm.String);
        return globalThis[name.to_js_string()];
    };

    /*
     * Calls a JS constructor with arguments.
     *
     * Note that this is not a fat arrow function because we need
     * access to the arguments object.
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
     * Creates a new array.
     */
    vm.js_array = () =>
    {
        return [];
    }

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

    /*** Lisp API ***/

    vm.define_constant("+js-true+", true);

    vm.define_constant("+js-false+", false);

    vm.define_constant("+js-null+", null);

    vm.define_constant("+js-undefined+", undefined);

    vm.define_alien_function("%%to-lisp-boolean", vm.to_lisp_boolean);

    vm.define_alien_function("%%to-js-boolean", (bool) =>
        vm.assert_type(bool, vm.Boolean).to_js_boolean());

    vm.define_alien_function("%%to-lisp-number", (js_num) =>
        vm.num(vm.assert_type(js_num, "number")));

    vm.define_alien_function("%%to-js-number", (num) =>
        vm.assert_type(num, vm.Number).to_js_number());

    vm.define_alien_function("%%to-lisp-string", (js_str) =>
        vm.str(vm.assert_type(js_str, "string")));

    vm.define_alien_function("%%to-js-string", (str) =>
        vm.assert_type(str, vm.String).to_js_string());

    vm.define_alien_function("%%to-lisp-function", vm.to_lisp_function);

    vm.define_alien_function("%%to-js-function", vm.to_js_function);

    vm.define_alien_function("%%js-global", vm.js_global);

    vm.define_alien_function("%%js-new", vm.js_new);

    vm.define_alien_function("%%js-get", vm.js_get);

    vm.define_alien_function("%%js-array", vm.js_array);

    vm.define_alien_function("%%apply-js-method", vm.apply_js_method);

    vm.define_alien_function("%%js-log", (...objects) => console.log(...objects));

};
