/*
 * LispX Sequence and List Processing Utilities
 * Copyright (c) 2021, 2022 Manuel J. Simoni
 */

/*
 * Adds sequence and list processing utilities to a VM.
 *
 * These are not needed for the internal functioning of the VM, and
 * could be implemented in Lisp, so they reside in their own module.
 */
export function init_seq(vm)
{
    /*
     * Creates a list from its arguments so that the last argument
     * becomes the list's final cdr.  Cf. Common Lisp's LIST*.
     */
    vm.list_star = (...objects) =>
    {
        const len = objects.length;
        let list = (len >= 1) ? objects[len - 1] : vm.nil();
        for (let i = len - 1; i > 0; i--)
            list = vm.cons(objects[i - 1], list);
        return list;
    };

    /*
     * Concatenate two lists.  The first one must be proper and is
     * copied.  The second one is not copied (and doesn't even have to
     * be a list). It becomes the cdr of the final cons of the first
     * list, or is returned directly if the first list is empty.
     */
    vm.append = (list1, list2) =>
    {
        vm.assert_type(list1, vm.List);
        if (list1 === vm.nil())
            return list2;
        else
            return vm.cons(list1.car(), vm.append(list1.cdr(), list2));
    };

    /*
     * Returns the length of a list.
     */
    vm.list_length = (list) =>
    {
        vm.assert_type(list, vm.List);
        if (list === vm.nil())
            return 0;
        else
            return 1 + vm.list_length(list.cdr());
    };

    /*
     * Returns the tail of list that would be obtained by calling cdr
     * n times in succession.
     */
    vm.nthcdr = (n, list) =>
    {
        vm.assert_type(list, vm.List);
        if (n === 0) {
            return list;
        } else {
            if (list === vm.nil())
                throw new vm.Out_of_bounds_error();
            else
                return vm.nthcdr(n - 1, list.cdr());
        }
    };

    /*
     * Creates a new list by calling a function on every element of a list.
     */
    vm.mapcar = (fun, list) =>
    {
        if (list === vm.nil())
            return vm.nil();
        else
            return mapcar_aux(fun, list, vm.nil());
    };

    function mapcar_aux(fun, list, accumulator)
    {
        do accumulator = vm.cons(fun(list.car()), accumulator);
        while ((list = list.cdr()) !== vm.nil());
        return vm.reverse(accumulator);
    }

    /*
     * Calls a function on every list element for effect.
     */
    vm.mapc = (fun, list) =>
    {
        if (list !== vm.nil()) {
            fun(list.car());
            vm.mapc(fun, list.cdr());
        }
        return list;
    };

    /*** Common Lisp's SUBSEQ ***/

    /*
     * SUBSEQ for lists.
     */
    vm.list_subseq = (list, start, end = undefined) =>
    {
        vm.assert_type(list, vm.List);
        vm.assert_type(start, "number");

        const tail = vm.nthcdr(start, list);
        if (end === undefined)
            return tail;
        else
            return take_n(tail, end - start);

        function take_n(list, n)
        {
            if (n === 0) {
                return vm.nil();
            } else {
                if (list === vm.nil())
                    throw new vm.Out_of_bounds_error();
                else
                    return vm.cons(list.car(), take_n(list.cdr(), n - 1));
            }
        }
    };

    /*
     * SUBSEQ for strings.
     */
    vm.string_subseq = (string, start, end = undefined) =>
    {
        vm.assert_type(string, vm.String);
        const utf8_bytes = string.get_utf8_bytes();
        return new vm.String(vm.slice_subseq(utf8_bytes, start, end));
    };

    /*
     * Implements SUBSEQ for objects that have a .length property and
     * a slice() method, like JS strings, arrays, and TypedArrays.
     */
    vm.slice_subseq = (sliceable, start, end = undefined) =>
    {
        vm.assert_type(start, "number");
        if (start > sliceable.length) throw new vm.Out_of_bounds_error();
        if (end === undefined) {
            return sliceable.slice(start);
        } else {
            if (end > sliceable.length) throw new vm.Out_of_bounds_error();
            return sliceable.slice(start, end);
        }
    };

    /*
     * Produces an option (one-element list) holding the object.
     */
    vm.some = (object) =>
    {
        return vm.list(object);
    };

    /*
     * Extract the contents of an option, or if it is nil, call a
     * thunk to obtain a default value.  If no thunk is specified,
     * produce void.
     */
    vm.optional = (option, default_thunk = () => vm.void()) =>
    {
        if (option === vm.nil())
            return default_thunk();
        else
            return vm.elt(option, 0);
    };

    /*
     * Signalled when an indexing operation is out of bounds.
     */
    vm.Out_of_bounds_error = class Lisp_out_of_bounds_error extends vm.Error
    {
        constructor()
        {
            super("Out of bounds");
        }
    };

    /*
     * Utility for turning the end argument to Lisp SUBSEQ into a JS
     * number or undefined.
     */
    function canonicalize_end(end) {
        if (end === vm.void()) return undefined;
        else return vm.assert_type(end, vm.Number).to_js_number();
    }

    vm.define_alien_function("%list*", (...objects) => vm.list_star(...objects));

    vm.define_alien_function("%append", (list1, list2) => vm.append(list1, list2));

    vm.define_alien_function("%list-length", (list) => vm.num(vm.list_length(list)));

    vm.define_alien_function("%nth", (num, list) =>
        vm.elt(list, vm.assert_type(num, vm.Number).to_js_number()));

    vm.define_alien_function("%nthcdr", (num, list) =>
        vm.nthcdr(vm.assert_type(num, vm.Number).to_js_number(), list));

    vm.define_alien_function("%list-subseq", (list, start, end) =>
        vm.list_subseq(list,
                       vm.assert_type(start, vm.Number).to_js_number(),
                       canonicalize_end(end)));

    vm.define_alien_function("%string-subseq", (string, start, end) =>
        vm.string_subseq(string,
                         vm.assert_type(start, vm.Number).to_js_number(),
                         canonicalize_end(end)));

    vm.define_alien_function("%reverse", (list) => vm.reverse(list));

    vm.define_condition("out-of-bounds-error", vm.Out_of_bounds_error, vm.Error);

};
