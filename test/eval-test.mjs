import { assert } from "chai";

import { VM } from "lispx-vm";

import test_util_code from "./test-util.lispx";
import boot_test_code from "./boot-test.lispx";
import control_test_code from "./control-test.lispx";
import stream_test_code from "./stream-test.lispx";
import read_test_code from "./read-test.lispx";
import print_test_code from "./print-test.lispx";
import js_test_code from "./js-test.lispx";
import hierarchy_test_code from "./hierarchy-test.lispx";

const vm = time("Boot LispX", () => new VM());

/*
 * Utilities.
 */

function time(name, fun)
{
    const start = new Date().getTime();
    const result = fun();
    const end = new Date().getTime();
    const time = end - start;
    console.log(name + ": "  + time + "ms");
    return result;
}

function make_child_environment()
{
    return vm.make_environment(vm.get_environment());
}

function quote(expr)
{
    return vm.list(vm.sym("quote"), expr);
}

describe("Evaluation & Operation", () => {

    it("Built-in operators are defined.", () => {

        const operators = [
            "%%vau",
            "%%def",
            "%%progn",
            "%%if",
            "%%loop",
            "%%catch",
            "%%unwind-protect",
        ];

        for (const name of operators) {
            const op = vm.get_environment().lookup(vm.fsym(name));
            assert.equal(op.get_name(), vm.sym(name));
            vm.assert_type(op, vm.Built_in_operator);
        }

    });

    it("Built-in functions are defined.", () => {

        const functions = [
            "%%*",
            "%%+",
            "%%-",
            "%%/",
            "%%<",
            "%%<=",
            "%%=",
            "%%>",
            "%%>=",
            "%%boundp",
            "%%car",
            "%%cdr",
            "%%class-name",
            "%%class-of",
            "%%class-symbol",
            "%%cons",
            "%%eq",
            "%%eval",
            "%%function-symbol",
            "%%intern",
            "%%invoke-method",
            "%%keyword-symbol",
            "%%length",
            "%%list*",
            "%%make-environment",
            "%%make-instance",
            "%%make-standard-class",
            "%%nth",
            "%%nthcdr",
            "%%panic",
            "%%progv",
            "%%push-delim-subcont",
            "%%push-prompt",
            "%%push-subcont-barrier",
            "%%put-method",
            "%%reverse",
            "%%set-slot-value",
            "%%slot-bound-p",
            "%%slot-value",
            "%%some",
            "%%subclassp",
            "%%subseq",
            "%%take-subcont",
            "%%throw",
            "%%typep",
            "%%unwrap",
            "%%variable-symbol",
            "%%wrap",
        ];

        for (const name of functions) {
            const op = vm.get_environment().lookup(vm.fsym(name));
            assert.equal(op.get_name(), vm.sym(name));
            vm.assert_type(op, vm.Function);
        }

    });

    it("Many objects evaluate to themselves.", () => {

        const examples = [
            vm.num(1),
            vm.kwd("foo"),
            vm.t(),
            vm.f(),
            vm.nil(),
            vm.void(),
            vm.ignore(),
            // Plain JS objects, too.
            12,
            true,
            false,
            undefined,
            { hello: "world" },
            [1,2],
        ];

        for (const ex of examples)
            for (const eval_fun of [vm.eval, vm.eval_form])
                assert.equal(eval_fun(ex), ex);

    });

    it("Symbols evaluate to the value they are bound to.", () => {

        const examples = [
            [vm.csym("object"), vm.lisp_class(vm.Object)],
            [vm.csym("class"), vm.lisp_class(vm.Class)],
        ];

        for (const [symbol, value] of examples)
            for (const eval_fun of [vm.eval, vm.eval_form])
                assert.equal(eval_fun(symbol), value);

    });

    it("Evaluating an unbound symbol causes an error.", () => {

        for (const eval_fun of [vm.eval, vm.eval_form]) {
            assert.throws(() => eval_fun(vm.sym("this-is-not-bound")),
                          "Unbound variable: this-is-not-bound");
            assert.throws(() => eval_fun(vm.fsym("this-is-not-bound")),
                          "Unbound function: this-is-not-bound");
            assert.throws(() => eval_fun(vm.csym("this-is-not-bound")),
                          "Unbound class: this-is-not-bound");
        }

    });

    it("Evaluating a cons whose car is not an operator causes an error.", () => {

        const examples = [ vm.nil(), undefined, "foo", vm.str("foo"), vm.num(1) ];

        for (const ex of examples)
            for (const eval_fun of [vm.eval, vm.eval_form])
                assert.throws(() => eval_fun(vm.list(ex)),
                              "Type assertion failed");

    });

    it("The environment to evaluate in can be specified.", () => {

        const env = make_child_environment();
        env.put(vm.sym("foo"), vm.num(12));
        for (const eval_fun of [vm.eval, vm.eval_form])
            assert(vm.equal(eval_fun(vm.sym("foo"), env),
                            vm.num(12)));

    });

    it("vm.eval_form() doesn't swallow suspensions.", () => {

        const form = vm.list(vm.sym("take-subcont"), vm.str("prompt"), vm.sym("k"));
        assert.throws(() => vm.eval_form(form), "Prompt not found");
        assert.instanceOf(vm.eval(form), vm.Suspension);

    });

    it("%%EVAL uses the root environment if no environment is specified.", () => {

        assert.instanceOf(vm.eval(vm.list(vm.sym("%%eval"), quote(vm.csym("object")))),
                          vm.Class);

    });

    it("Symbols in the operator position are looked up in the function namespace.", () => {

        const env = make_child_environment();
        env.put(vm.fsym("foo"), vm.eval_js_string("(%%vau #ignore #ignore 100)"));
        assert(vm.equal(vm.eval_js_string("(foo)", env), vm.num(100)));

    });

    it("Non-symbol expressions in the operator position are evaluated normally.", () => {

        assert(vm.equal(vm.eval_js_string("((%%vau #ignore #ignore 200))"), vm.num(200)));

    });

    it("Test vm.operate().", () => {

        const list_op = vm.get_environment().lookup(vm.fsym("list"));
        assert(vm.equal(vm.operate(list_op, vm.nil()), vm.nil()));
        assert(vm.equal(vm.operate(list_op, vm.list(vm.num(1))), vm.list(vm.num(1))));

    });

    it("The environment to operate in can be specified.", () => {

        const def_op = vm.get_environment().lookup(vm.fsym("%%def"));
        const env = make_child_environment();
        vm.operate(def_op, vm.list(vm.sym("x"), vm.num(1)), env);
        assert(vm.equal(env.lookup(vm.sym("x")), vm.num(1)));

    });

    it("match() binds symbols to their operands.", () => {

        const env = vm.make_environment();
        const rhs = vm.num(12);
        const result = vm.match(vm.sym("x"), rhs, env);
        assert(vm.equal(result, rhs));
        assert(vm.equal(env.lookup(vm.sym("x")), vm.num(12)));

    });

    it("match() doesn't allow keyword definiends.", () => {

        assert.throws(() => vm.match(vm.kwd("x"), vm.nil()),
                      "Match error");

    });

    it("match() recursively matches conses.", () => {

        const env = vm.make_environment();
        const rhs = vm.cons(vm.num(12), vm.num(33));
        const result = vm.match(vm.cons(vm.sym("x"), vm.sym("y")), rhs, env);
        assert(vm.equal(rhs, result));
        assert(vm.equal(env.lookup(vm.sym("x")), vm.num(12)));
        assert(vm.equal(env.lookup(vm.sym("y")), vm.num(33)));

        assert.throws(() => vm.match(vm.cons(vm.sym("x"), vm.sym("y")), vm.t(), env),
                      "Match error");

    });

    it("match() requires the operand of #NIL to be #NIL.", () => {

        const env = vm.make_environment();
        assert(vm.equal(vm.match(vm.nil(), vm.nil(), env), vm.nil()));

        assert.throws(() => vm.match(vm.nil(), vm.t(), env),
                      "Match error");

    });

    it("#IGNORE ignores its operand in match().", () => {

        const env = vm.make_environment();
        assert(vm.equal(vm.match(vm.ignore(), vm.num(1), env), vm.num(1)));

    });

    it("Other definiends in match() are an error.", () => {

        const env = vm.make_environment();
        assert.throws(() => vm.match(vm.num(1), vm.void(), env),
                      "Type assertion failed");
        assert.throws(() => vm.match(vm.t(), vm.void(), env),
                      "Type assertion failed");

    });

});

describe("Fexprs", () => {

    it("Fexprs are anonymous.", () => {

        assert.equal(vm.eval_js_string("(%%vau #ignore #ignore #ignore)").get_name(),
                     vm.sym("anonymous operator"));

    });

});

describe("%%VAU", () => {

    it("%%VAU constructs simple fexprs.", () => {

        const fexpr = vm.eval_js_string("(%%vau #ignore #ignore 12)");
        vm.assert_type(fexpr, vm.Fexpr);
        assert(vm.equal(vm.eval(vm.list(fexpr)), vm.num(12)));

    });

    it("%%VAU sets fexpr properties.", () => {

        const def_env = make_child_environment();
        const fexpr = vm.eval_js_string("(%%vau x y z)", def_env);
        vm.assert_type(fexpr, vm.Fexpr);
        assert(vm.equal(fexpr.param_tree, vm.sym("x")));
        assert(vm.equal(fexpr.env_param, vm.sym("y")));
        assert(vm.equal(fexpr.body_form, vm.sym("z")));
        assert(vm.equal(fexpr.def_env, def_env));

    });

    it("%%VAU evaluates the body form and passes on errors.", () => {

        assert.throws(() => vm.eval_js_string("((%%vau #ignore #ignore z))"),
                      "Unbound variable: z");

    });

    it("%%VAU receives the operands.", () => {

        assert(vm.equal(vm.eval_js_string("((%%vau (x y z) #ignore z) 1 2 3)"),
                        vm.num(3)));

    });

    it("%%VAU throws for illegal definiends.", () => {

        assert.throws(() => vm.eval_js_string("(%%vau 1 #ignore #ignore)"),
                      "Type assertion failed");
        assert.throws(() => vm.eval_js_string("(%%vau #ignore 1 #ignore)"),
                      "Type assertion failed");

    });

    it("%%VAU binds the environment parameter.", () => {

        const dyn_env = make_child_environment();
        assert(vm.equal(vm.eval_js_string("((%%vau #ignore env env))", dyn_env),
                        dyn_env));

    });

});

describe("%%DEF", () => {

    it("%%DEF evaluates the expression and matches the definiend.", () => {

        const env = make_child_environment();
        const fexpr = vm.eval_js_string("(%%def #'some-fexpr (%%vau x #ignore 12))", env);
        vm.assert_type(fexpr, vm.Fexpr);
        assert(vm.equal(vm.eval_js_string("(some-fexpr)", env), vm.num(12)));
        assert(vm.equal(env.lookup(vm.fsym("some-fexpr")), fexpr));
        assert.throws(() => vm.get_environment().lookup(vm.fsym("some-fexpr")),
                      "Unbound function: some-fexpr");

    });

    it("%%DEF throws for illegal definiends.", () => {

        assert.throws(() => vm.eval_js_string("(%%def 1 #ignore)"),
                      "Type assertion failed");
        assert.throws(() => vm.eval_js_string("(%%def #t #ignore)"),
                      "Type assertion failed");

    });

    it("%%DEF passes on errors from the expression.", () => {

        assert.throws(() => vm.eval_js_string("(%%def #ignore x)"),
                      "Unbound variable: x");

    });

});

describe("%%PROGN", () => {

    it("%%PROGN evaluates its operands and returns the result of the last.", () => {

        const env = make_child_environment();
        env.put(vm.sym("x"), vm.num(3));
        assert(vm.equal(vm.eval_js_string("(%%progn 1 2 x)", env),
                        vm.num(3)));

    });

    it("%%PROGN evaluates to #VOID if there are no operands.", () => {

        assert(vm.equal(vm.eval_js_string("(%%progn)"),
                        vm.void()));

    });

    it("%%PROGN passes on errors from the operands.", () => {

        assert.throws(() => vm.eval_js_string("(%%progn 1 x 2)"),
                      "Unbound variable: x");

    });

});

describe("%%IF", () => {

    it("%%IF evaluates the test and sub-expressions.", () => {

        const env = make_child_environment();

        env.put(vm.sym("x"), vm.t());
        env.put(vm.sym("y"), vm.f());

        env.put(vm.sym("a"), vm.num(1));
        env.put(vm.sym("b"), vm.num(2));

        assert(vm.equal(vm.eval_js_string("(%%if x a b)", env), vm.num(1)));
        assert(vm.equal(vm.eval_js_string("(%%if y a b)", env), vm.num(2)));

    });

    it("%%IF requires a boolean test.", () => {

        assert.throws(() => vm.eval_js_string("(%%if 1 2 3)"),
                      "Type assertion failed");

    });

    it("%%IF passes on errors from evaluating the subexpressions.", () => {

        assert.throws(() => vm.eval_js_string("(%%if x 2 3)"),
                      "Unbound variable: x");
        assert.throws(() => vm.eval_js_string("(%%if #t y 3)"),
                      "Unbound variable: y");
        assert.throws(() => vm.eval_js_string("(%%if #f 2 z)"),
                      "Unbound variable: z");

    });

    it("%%IF only evaluates one of consequent and alternative, not both.", () => {

        // X is unbound.
        assert(vm.equal(vm.eval_js_string("(%%if #t 1 x)"), vm.num(1)));
        assert(vm.equal(vm.eval_js_string("(%%if #f x 1)"), vm.num(1)));

    });

});

describe("Wrapping and unwrapping.", () => {

    it("A function has the same name as its wrapped operator.", () => {

        const wrapped = vm.alien_function(() => vm.t(), "foo");
        const fun = vm.wrap(wrapped);
        assert.equal(fun.get_name(), vm.sym("foo"));
        assert.equal(vm.eval(vm.list(wrapped)), vm.t());

    });

    it("Functions can be unwrapped.", () => {

        const wrapped = vm.alien_function(() => vm.t());
        assert.equal(vm.wrap(wrapped).unwrap(), wrapped);

    });

    it("Wrapping induces argument evaluation.", () => {

        const env = make_child_environment();
        env.put(vm.sym("x"), vm.num(1));
        env.put(vm.sym("y"), vm.num(2));
        env.put(vm.fsym("f"), vm.wrap(vm.eval_js_string("(%%vau args #ignore args)")));

        assert(vm.equal(vm.eval_js_string("(f x y)", env),
                        vm.list(vm.num(1), vm.num(2))));

    });

    it("Wrap requires its argument to be an operator.", () => {

        assert.throws(() => vm.wrap(1), "Type assertion failed");
        assert.throws(() => vm.wrap(vm.t()), "Type assertion failed");

    });

    it("Functions can be (multiply) wrapped.", () => {

        const doubly_wrapped = vm.wrap(vm.wrap(vm.alien_function(() => vm.t())));
        assert.equal(vm.eval(vm.list(doubly_wrapped)), vm.t());

    });

});

describe("Alien Operators", () => {

    it("Alien operators can be defined from JS functions.", () => {

        const op = vm.alien_operator((a, b, c) => {
            assert.equal(a, vm.sym("x"));
            assert.equal(b, vm.sym("y"));
            assert.equal(c, vm.sym("z"));
            return vm.t();
        }, "foo");

        assert(vm.equal(op.get_name(), vm.sym("foo")));
        assert(op instanceof vm.Built_in_operator);

        const result = vm.operate(op, vm.list(vm.sym("x"), vm.sym("y"), vm.sym("z")));
        assert(vm.equal(result, vm.t()));

    });

    it("Alien functions can be defined from JS functions.", () => {

        const op = vm.alien_function((a, b, c) => a + b + c, "foo");

        assert(vm.equal(op.get_name(), vm.sym("foo")));
        assert(op instanceof vm.Function);
        assert(op.unwrap() instanceof vm.Built_in_operator);

        const env = make_child_environment();
        env.put(vm.sym("x"), 1);
        env.put(vm.sym("y"), 2);
        env.put(vm.sym("z"), 3);
        const result = vm.operate(op, vm.list(vm.sym("x"), vm.sym("y"), vm.sym("z")), env);
        assert(vm.equal(result, 6));

    });

    it("Alien operators and functions can be anonymous.", () => {

        const op = vm.alien_operator(() => {});
        assert(vm.equal(op.get_name(), vm.sym("anonymous built-in operator")));

        const fun = vm.alien_function(() => {});
        assert(vm.equal(op.get_name(), vm.sym("anonymous built-in operator")));

    });

});

describe("Generic Functions", () => {

    it("Test put_method() and INVOKE_METHOD().", () => {

        // Define a method M1 that returns the receiver on the class OBJECT.
        const method_name = vm.sym("m1");
        const method = vm.alien_operator((...args) => args[0]);
        vm.lisp_class(vm.Object).put_method(method_name, method);

        // Call it on a string and a number.
        const receiver1 = vm.str("foo");
        const receiver2 = vm.num(1);
        for (const receiver of [receiver1, receiver2]) {
            const method_args = vm.list(receiver);
            assert(vm.equal(vm.INVOKE_METHOD(vm.list(method_name, method_args),
                                             vm.get_environment()),
                            receiver));

            // Test an unbound method.
            assert.throws(() => vm.INVOKE_METHOD(vm.list(vm.sym("m2"), method_args),
                                                 vm.get_environment()),
                          "Unbound method: m2");
        }

        // Override the method for strings.
        const str_method = vm.alien_operator((...args) => vm.str("quux"));
        vm.lisp_class(vm.String).put_method(method_name, str_method);

        // Test that it returns the new result for strings...
        assert(vm.equal(vm.INVOKE_METHOD(vm.list(method_name, vm.list(receiver1)),
                                         vm.get_environment()),
                        vm.str("quux")));
        // ...and still the old one for numbers.
        assert(vm.equal(vm.INVOKE_METHOD(vm.list(method_name, vm.list(receiver2)),
                                         vm.get_environment()),
                        receiver2));

    });

});

describe("Panicking", () => {

    it("Panics can not be caught by condition handlers.", () => {

        assert.throws(() => vm.eval_js_string("(panic #void)"),
                      "LISP panic!");

        const env = make_child_environment();
        env.put(vm.sym("cause"), new Error("it happened"));

        assert.throws(() => vm.eval_js_string("(panic cause)", env),
                      "LISP panic: it happened");

        assert.throws(() =>
            vm.eval_js_string(`(block b
                                 (handler-bind ((object (lambda (e) (return-from b))))
                                   (panic cause)))`, env),
            "LISP panic: it happened");

    });

    it("Panics do not trigger UNWIND-PROTECT.", () => {

        const env = make_child_environment();
        env.put(vm.sym("cause"), new Error("it happened"));

        assert.throws(() => vm.eval_js_string(`(unwind-protect (panic cause)
                                                 this-var-is-unbound)`, env),
                      "LISP panic: it happened");

    });

    it("ERROR panics if there is no handler.", () => {

        const env = make_child_environment();
        env.put(vm.sym("cause"), new Error("foo"));

        assert.throws(() => vm.eval_js_string("(error cause)", env),
                      "LISP panic: foo");

    });

});
