/*
 * LispX List Processing Utilities
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds list processing utilities to a VM.
 *
 * These are not needed for the internal functioning of the VM, and
 * could be implemented in Lisp, so they reside in their own module.
 */
export function init_list(vm)
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
     * Creates a list that is a copy of the subsequence of the list
     * bounded by start and end.
     */
    vm.list_subseq = (list, start, end = -1) =>
    {
        const tail = vm.nthcdr(start, list);
        if (end === -1)
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
     * Produces an option (one-element list) holding the object.
     */
    vm.some = (object) =>
    {
        return vm.list(object);
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

    vm.define_alien_function("%%list*", (...objects) => vm.list_star(...objects));

    vm.define_alien_function("%%append", (list1, list2) => vm.append(list1, list2));

    vm.define_alien_function("%%length", (list) => vm.num(vm.list_length(list)));

    vm.define_alien_function("%%nthcdr", (num, list) =>
        vm.nthcdr(vm.assert_type(num, vm.Number).to_js_number(), list));

    vm.define_alien_function("%%subseq", (list, start, end) =>
        vm.list_subseq(list,
                       vm.assert_type(start, vm.Number).to_js_number(),
                       vm.assert_type(end, vm.Number).to_js_number()));

    vm.define_condition("out-of-bounds-error", vm.Out_of_bounds_error, vm.Error);

};
