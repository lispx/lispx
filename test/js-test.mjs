import { assert } from "chai";

import { make_vm } from "lispx-vm";

const vm = make_vm();

function* call_lisp_from_generator(prog)
{
    let action = () => vm.eval_form_core(vm.read(new vm.String_input_stream(vm.str(prog))));
    while (true) {
        const result = action();
        if (result instanceof vm.Suspension) {
            const resumption = yield result;
            action = () => resumption.resume();
        } else {
            return result;
        }
    }
}

function call_generator_from_lisp(generator, resumption = null)
{
    const result = generator.next(resumption);
    if (result.done) {
        return result.value;
    } else {
        return result.value.suspend((resumption) => call_generator_from_lisp(generator, resumption));
    }
}

describe("JavaScript Interface", () => {

    it("Test promises.", () => {

        function* do_something()
        {
            const value1 = yield* call_lisp_from_generator("(progn (sleep 1) (sleep 1) 10)");
            const value2 = yield* call_lisp_from_generator("(progn (sleep 1) (sleep 1) 100)");
            return vm.add(value1, value2);
        }

        vm.define_alien_function("do-something", () => call_generator_from_lisp(do_something()));
        return vm.eval_js_string("(coroutine (print (do-something)))");
        // prints 110

    });

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

        const js_fun = vm.to_js_function(vm.get_user_environment().lookup(vm.fsym("list")));
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
        vm.get_user_environment().put(vm.fsym("some-fun"), lisp_fun);
        assert.equal(123, vm.eval(vm.list(vm.sym("some-fun"))));

    });

    it("Test js_elt().", () => {
        const array = [1, 2, 3];
        assert.equal(1, vm.js_elt(array, vm.num(0)));
        assert.equal(2, vm.js_elt(array, vm.num(1)));
        assert.throws(() => vm.js_elt("foo", vm.num(1)), "Assertion failed");
        assert.throws(() => vm.js_elt(array, vm.f()), "expected number got #f");
    });

    it("Test JS console output.", () => {

        function test_output(expected_result, expected_output_lines, thunk)
        {
            const output_lines = [];
            function output_function(line) { output_lines.push(line); };
            const stream = new vm.JS_console_output_stream(output_function);
            assert.deepEqual(thunk(stream), expected_result);
            assert.deepEqual(output_lines, expected_output_lines);
        }

        test_output(vm.f(), [], (stream) => {
            return stream.fresh_line()
        });
        test_output(vm.f(), [], (stream) => {
            stream.fresh_line();
            return stream.fresh_line();
        });
        test_output(vm.void(), [], (stream) => {
            return stream.force_output()
        });
        test_output(vm.void(), [], (stream) => {
            stream.fresh_line();
            return stream.force_output()
        });
        test_output(vm.str("foo"), [], (stream) => {
            return stream.write_string(vm.str("foo"));
        });
        test_output(vm.void(), ["foo"], (stream) => {
            stream.write_string(vm.str("foo"));
            return stream.force_output();
        });
        test_output(vm.void(), ["foobar"], (stream) => {
            stream.write_string(vm.str("foo"));
            stream.write_string(vm.str("bar"));
            return stream.force_output();
        });
        test_output(vm.void(), ["foobar", "foobar"], (stream) => {
            stream.write_string(vm.str("foo"));
            stream.write_string(vm.str("bar"));
            stream.force_output();
            stream.write_string(vm.str("foo"));
            stream.write_string(vm.str("bar"));
            return stream.force_output();
        });
        test_output(vm.void(), ["foo\nbar", "foobar"], (stream) => {
            stream.write_string(vm.str("foo"));
            assert.deepEqual(stream.fresh_line(), vm.t());
            assert.deepEqual(stream.fresh_line(), vm.f());
            stream.write_string(vm.str("bar"));
            stream.force_output();
            stream.write_string(vm.str("foo"));
            stream.write_string(vm.str("bar"));
            return stream.force_output();
        });
        test_output(vm.void(), ["\n"], (stream) => {
            stream.write_string(vm.str("\n"));
            return stream.force_output();
        });
        test_output(vm.void(), ["\n"], (stream) => {
            stream.write_string(vm.str("\n"));
            assert.deepEqual(stream.fresh_line(), vm.f());
            return stream.force_output();
        });
        test_output(vm.void(), ["\nfoo\nbar", "quux"], (stream) => {
            stream.write_string(vm.str("\n"));
            assert.deepEqual(stream.fresh_line(), vm.f());
            assert.deepEqual(vm.str("foo"), stream.write_string(vm.str("foo")));
            stream.write_string(vm.str("\n"));
            assert.deepEqual(stream.fresh_line(), vm.f());
            assert.deepEqual(vm.str("bar"), stream.write_string(vm.str("bar")));
            stream.force_output();
            assert.deepEqual(vm.str("quux"), stream.write_string(vm.str("quux")));
            return stream.force_output();
        });

    });

});
