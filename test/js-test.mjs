import { assert } from "chai";

import { VM } from "lispx-vm";

const vm = new VM();

describe("JavaScript Interface", () => {

    it("Test to_lisp_boolean().", () => {

        assert.equal(vm.to_lisp_boolean(true), vm.t());
        assert.equal(vm.to_lisp_boolean(false), vm.f());

        assert.throws(() => vm.to_lisp_boolean(12), "Type assertion failed");

    });

    it("Test apply_js_method().", () => {

        assert.equal(vm.apply_js_method(9.656, vm.str("toFixed"), vm.list(2)),
                     "9.66");

    });

    it("Test js_global().", () => {

        assert(Math === vm.js_global(vm.str("Math")));

    });

    it("Test js_new() and js_get().", () => {

        class Point
        {
            constructor(x, y)
            {
                this.x = x;
                this.y = y;
            }
        }

        const pt = vm.js_new(Point, 1, 2);

        assert.equal(pt.x, 1);
        assert.equal(pt.y, 2);

        assert.equal(vm.js_get(pt, vm.str("x")), 1);
        assert.equal(vm.js_get(pt, vm.str("y")), 2);

    });

    it("Test to_js_function().", () => {

        const js_fun = vm.to_js_function(vm.get_environment().lookup(vm.fsym("list")));
        vm.assert_type(js_fun, "function");
        const list = js_fun(vm.num(1), vm.num(2));
        vm.assert_type(list, vm.List);
        assert(vm.equal(vm.elt(list, 0), vm.num(1)));
        assert(vm.equal(vm.elt(list, 1), vm.num(2)));

    });

    it("Test to_lisp_function().", () => {

        const js_fun = () => 123;
        const lisp_fun = vm.to_lisp_function(js_fun);
        vm.assert_type(lisp_fun, vm.Operator);
        vm.get_environment().put(vm.fsym("some-fun"), lisp_fun);
        assert.equal(123, vm.eval(vm.list(vm.sym("some-fun"))));

    });

    it("Test js_elt().", () => {
        const array = [1, 2, 3];
        assert.equal(1, vm.js_elt(array, vm.num(0)));
        assert.equal(2, vm.js_elt(array, vm.num(1)));
        assert.throws(() => vm.js_elt("foo", vm.num(1)), "Assertion failed");
        assert.throws(() => vm.js_elt(array, vm.f()), "expected number");
    });

});
