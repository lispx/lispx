import { assert } from "chai";

import { VM } from "lispx-vm";

const vm = new VM();

/*
 * These tests can be somewhat cursory because we have additional
 * testing of the exported Lisp functions.
 */

describe("Sequence & List Utilities", () => {

    it("Test list_star().", () => {

        assert(vm.equal(vm.list_star(),
                        vm.nil()));
        assert(vm.equal(vm.list_star(vm.num(1)),
                        vm.num(1)));
        assert(vm.equal(vm.list_star(vm.num(1), vm.num(2)),
                        vm.cons(vm.num(1), vm.num(2))));
        assert(vm.equal(vm.list_star(vm.num(1), vm.num(2), vm.num(3)),
                        vm.cons(vm.num(1), vm.cons(vm.num(2), vm.num(3)))));

    });

    it("Test nthcdr().", () => {

        assert(vm.equal(vm.nthcdr(0, vm.list(1, 2, 3)),
                        vm.list(1, 2, 3)));
        assert(vm.equal(vm.nthcdr(1, vm.list(1, 2, 3)),
                        vm.list(2, 3)));
        assert(vm.equal(vm.nthcdr(2, vm.list(1, 2, 3)),
                        vm.list(3)));
        assert(vm.equal(vm.nthcdr(3, vm.list(1, 2, 3)),
                        vm.nil()));
        assert.throws(() => vm.nthcdr(4, vm.list(1, 2, 3)), "Out of bounds");

    });

    it("Test list_length().", () => {

        assert(vm.equal(vm.list_length(vm.nil()), 0));
        assert(vm.equal(vm.list_length(vm.list(1, 2, 3)), 3));

    });

    it("Test mapcar().", () => {

        assert(vm.equal(vm.nil(), vm.mapcar((elt) => false, vm.nil())));

        const list = vm.list(vm.num(1), vm.num(2), vm.num(3));
        const expected = vm.list(vm.num(2), vm.num(3), vm.num(4));
        assert.deepEqual(expected, vm.mapcar((elt) => vm.add(elt, vm.one()), list));

    });

    it("Test mapc().", () => {

        assert(vm.equal(vm.nil(), vm.mapc((elt) => false, vm.nil())));

        let array = [];
        const list = vm.list(vm.num(1), vm.num(2), vm.num(3));
        assert(list === vm.mapc((elt) => array.push(elt), list));
        assert.deepEqual(array, [vm.num(1), vm.num(2), vm.num(3)]);

    });

    it("Test list_subseq().", () => {

        assert(vm.equal(vm.list_subseq(vm.list(1, 2, 3), 0),
                        vm.list(1, 2, 3)));
        assert(vm.equal(vm.list_subseq(vm.list(1, 2, 3), 0, 1),
                        vm.list(1)));
        assert(vm.equal(vm.list_subseq(vm.list(1, 2, 3), 2),
                        vm.list(3)));
        assert(vm.equal(vm.list_subseq(vm.list(1, 2, 3), 1, 2),
                        vm.list(2)));

    });

    it("Test string_subseq().", () => {

        assert(vm.equal(vm.string_subseq(vm.str("123"), 0),
                        vm.str("123")));
        assert(vm.equal(vm.string_subseq(vm.str("123"), 0, 1),
                        vm.str("1")));
        assert(vm.equal(vm.string_subseq(vm.str("123"), 2),
                        vm.str("3")));
        assert(vm.equal(vm.string_subseq(vm.str("123"), 1, 2),
                        vm.str("2")));

    });

    it("Test append().", () => {

        assert(vm.equal(vm.append(vm.nil(), vm.nil()), vm.nil()));
        assert(vm.equal(vm.append(vm.list(1), vm.nil()), vm.list(1)));
        assert(vm.equal(vm.append(vm.nil(), vm.list(1)), vm.list(1)));
        assert(vm.equal(vm.append(vm.nil(), vm.list(1, 2, 3)), vm.list(1, 2, 3)));
        assert(vm.equal(vm.append(vm.list(1, 2, 3), vm.nil()), vm.list(1, 2, 3)));
        assert(vm.equal(vm.append(vm.list(1, 2), vm.list(3, 4, 5)), vm.list(1, 2, 3, 4, 5)));
        assert(vm.equal(vm.append(vm.list(1, 2, 3), 4), vm.list_star(1, 2, 3, 4)));

    });

    it("Test some().", () => {

        assert(vm.equal(vm.some(vm.num(12)), vm.list(vm.num(12))));

    });

});
