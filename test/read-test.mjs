import { assert } from "chai";

import { VM } from "lispx-vm";

const vm = new VM();

function string_input_stream(js_string)
{
    return new vm.String_input_stream(vm.str(js_string));
}

describe("Reader", () => {

    it("Whitespace is correctly parsed.", () => {

        /*
         * This tests the situation where we there is only
         * whitespace, and no more object to read, before the EOF.
         */

        const examples = [
            "", "    ", "  \n", "\n   \t \n   ",
            ";", "   ;", "; hello", ";;;;; headline", "\t\n;foo",
            "; foo \n ; bar \n", ";;;; foo\n\t ",
            "#||#", "#||##||#", "#|#|hello|#|#\n;foo\n\n"
        ];

        for (const ex of examples) {
            assert.throws(() => vm.read(string_input_stream(ex)), "EOF");
            assert.equal(vm.read(string_input_stream(ex), false), vm.void());
            assert.equal(vm.read(string_input_stream(ex), false, -1), -1);
            /*
             * Check whether dot is allowed as eof_value. See read().
             */
            assert.equal(vm.read(string_input_stream(ex), false, vm.sym(".")),
                         vm.sym("."));
        }

        /*
         * Append an object after a newline to the example strings and test
         * that it is read.
         */

        for (const ex of examples) {
            const st = string_input_stream(ex + "\n1");
            assert(vm.equal(vm.read(st), vm.num(1)));
        }

    });

    it("Objects are correctly parsed.", () => {

        const examples = {

            // Numbers.
            "1":
            vm.num("1"),
            "-1":
            vm.num("-1"),
            "100000000000000000.1378912739821739821":
            vm.num("100000000000000000.1378912739821739821"),
            "-100000000000000000.1378912739821739821":
            vm.num("-100000000000000000.1378912739821739821"),

            // Symbols.
            "a":
            vm.sym("a"),
            "hello-world":
            vm.sym("hello-world"),
            "Iñtërnâtiônàlizætiøn":
            vm.sym("Iñtërnâtiônàlizætiøn"),

            // Escape characters in symbols.
            "a\\nb":
            vm.sym("a\nb"),
            "a\\\\b":
            vm.sym("a\\b"),
            "\\t\\n\\\\\\|\\\"":
            vm.sym("\t\n\\|\""),

            // Stuff that's almost a number but parses as a symbol.
            "1.":
            vm.sym("1."),
            ".1":
            vm.sym(".1"),
            ".1.":
            vm.sym(".1."),
            "1.a":
            vm.sym("1.a"),

            // Keyword symbols.
            ":foo":
            vm.sym("foo").to_keyword_symbol(),
            ":|foo|":
            vm.sym("foo").to_keyword_symbol(),

            // Function symbols.
            "#'foo":
            vm.sym("foo").to_function_symbol(),
            "#'|foo|":
            vm.sym("foo").to_function_symbol(),

            // Class symbols.
            "#^foo":
            vm.sym("foo").to_class_symbol(),
            "#^|foo|":
            vm.sym("foo").to_class_symbol(),

            // Strings.
            '""':
            vm.str(""),
            '"foo"':
            vm.str("foo"),
            '"foo bar"':
            vm.str("foo bar"),
            '"foo\\nbar"':
            vm.str("foo\nbar"),
            '"foo\nbar"':
            vm.str("foo\nbar"),
            '"foo\\nbar"':
            vm.str("foo\\nbar"),
            '"foo\\nbar"':
            vm.str("foo\nbar"),
            '"\\t\\n\\\\\\|\\\""':
            vm.str("\t\n\\|\""),
            '"Iñtërnâtiônàlizætiøn"':
            vm.str("Iñtërnâtiônàlizætiøn"),

            // Escaped symbols.
            '||':
            vm.sym(""),
            '|foo|':
            vm.sym("foo"),
            '|foo bar|':
            vm.sym("foo bar"),
            '|foo\nbar|':
            vm.sym("foo\nbar"),
            '|foo\\nbar|':
            vm.sym("foo\nbar"),
            '|foo\nbar|':
            vm.sym("foo\nbar"),
            '|\\t\\n\\\\\\|\\\"|':
            vm.sym("\t\n\\|\""),
            '|Iñtërnâtiônàlizætiøn|':
            vm.sym("Iñtërnâtiônàlizætiøn"),

            // Constants.
            "#t":
            vm.t(),
            "#f":
            vm.f(),
            "#nil":
            vm.nil(),
            "#void":
            vm.void(),
            "#ignore":
            vm.ignore(),

            // Lists.
            "()":
            vm.nil(),
            "( \n )":
            vm.nil(),
            "(1)":
            vm.list(vm.num(1)),
            "(1 . 2)":
            vm.cons(vm.num(1), vm.num(2)),
            "(1 2)":
            vm.list(vm.num(1), vm.num(2)),
            "(1 2 . 3)":
            vm.list_star(vm.num(1), vm.num(2), vm.num(3)),
            "(())":
            vm.list(vm.nil()),
            "( ( ) )":
            vm.list(vm.nil()),
            "( ( ) . ( ) )":
            vm.cons(vm.nil(), vm.nil()),

            // Line comments inside lists.
            "(;\n)":
            vm.nil(),
            "(1 . ;;;\n 1)":
            vm.cons(vm.num(1), vm.num(1)),
            "(;\n 1 ;\n . ;\n 1 ;\n)":
            vm.cons(vm.num(1), vm.num(1)),
            "(1 ;\n 2 ;\n 3 ;\n)":
            vm.list(vm.num(1), vm.num(2), vm.num(3)),

            // Block comments inside lists.
            "(#||#)":
            vm.nil(),
            "(1 . #||# 1)":
            vm.cons(vm.num(1), vm.num(1)),
            "(#||# 1 . #||# 1)":
            vm.cons(vm.num(1), vm.num(1)),
            "(#||# 1 . #||# 1 #||#)":
            vm.cons(vm.num(1), vm.num(1)),
            "(1 2 #||# 3)":
            vm.list(vm.num(1), vm.num(2), vm.num(3)),

            // Quote.
            "'foo":
            vm.list(vm.sym("quote"), vm.sym("foo")),
            "'()":
            vm.list(vm.sym("quote"), vm.nil()),
            "'(1 . 2)":
            vm.list(vm.sym("quote"), vm.cons(vm.num(1), vm.num(2))),
            "' foo-can-have-whitespace":
            vm.list(vm.sym("quote"), vm.sym("foo-can-have-whitespace")),
        };

        /*
         * Try parsing each object.
         *
         * Here each object gets ended by the EOF.
         */

        for (const [string, object] of Object.entries(examples)) {
            assert(vm.equal(vm.read(string_input_stream(string)), object));
        }

        /*
         * Try the same but append some whitespace before the object.
         */

        for (const [string, object] of Object.entries(examples)) {
            const s = "\n;foo\n  " + string;
            assert(vm.equal(vm.read(string_input_stream(s)), object));
        }

        /*
         * Try the same but append some whitespace before and after
         * the object.
         */

        for (const [string, object] of Object.entries(examples)) {
            const s = "\n;foo\n  " + string + " \n";
            assert(vm.equal(vm.read(string_input_stream(s)), object));
        }

        /*
         * Try the same but append whitespace before and a terminating
         * character after the object.
         */

        for (const term_char of ["(", ")", ";", "\"", "\'", "|"]) {
            for (const [string, object] of Object.entries(examples)) {
                const s = "\n;foo\n  " + string + term_char;
                const stream = string_input_stream(s);
                assert(vm.equal(vm.read(stream), object));

                // Test that the terminating character has been unread.
                // (To be pedantic, in the case of lists and delimited
                // objects like strings, it has never been read in the
                // first place.)
                assert.equal(stream.read_byte(), term_char);
            }
        }

    });

    it("List parsing errors are reported informatively.", () => {

        const examples = {
            ".": "Consing dot not allowed here",
            "#||#.": "Consing dot not allowed here",
            "#||# .": "Consing dot not allowed here",
            "(.": "Consing dot at start of list",
            "(#||#.": "Consing dot at start of list",
            "( .": "Consing dot at start of list",
            "(#||# .": "Consing dot at start of list",
            "(1 . 1 1)": "Multiple objects after consing dot",
            // Would be better to report "consing dot not allowed here"
            // but I won't complain.
            "(1 . 1 . 1)": "Multiple objects after consing dot",
            ")": "Unbalanced parenthesis"
        };

        for (const [string, message] of Object.entries(examples)) {
            assert.throws(() => vm.read(string_input_stream(string)),
                          message);
        }

    });

    it("Incomplete objects at EOF always cause an error.", () => {

        /*
         * A stream that ends with an incomplete object.
         *
         * Test that an EOF error is thrown regardless of eof_error_p.
         */

        const examples = [
            '"foo', "bla\\", "|foo", "|foo\\", "foo\\",
            "'", "'(",
            "(", "((", "(()", "(1 .", "(1 . (",
            "#|", "#||", "(#|", "(#||", "(#||#",
            "#|#|", "#|#||", "#|#||#",
        ];

        for (const e of examples) {

            const stream1 = string_input_stream(e);
            assert.throws(() => vm.read(stream1), "EOF");

            const stream2 = string_input_stream(e);
            assert.throws(() => vm.read(stream2, false), "EOF");

            const stream3 = string_input_stream(e);
            assert.throws(() => vm.read(stream3, false, -1), "EOF");

        }

    });

    it("Invalid escape characters lose.", () => {

        const msg = "Invalid escape character x";
        assert.throws(() => vm.read(string_input_stream("\\x")), msg);
        assert.throws(() => vm.read(string_input_stream('"\\x"')), msg);
        assert.throws(() => vm.read(string_input_stream("|\\x|")), msg);

    });

    it("Sharpsign at EOF causes an error.", () => {

        const msg = "EOF";
        assert.throws(() => vm.read(string_input_stream("#")), msg);
        assert.throws(() => vm.read(string_input_stream("#'")), msg);
        assert.throws(() => vm.read(string_input_stream("#'\n")), msg);
        assert.throws(() => vm.read(string_input_stream("#';\n;")), msg);

    });

    it("Unknown alphabetic characters after sharpsign cause an error.", () => {

        const msg = "Illegal constant";
        assert.throws(() => vm.read(string_input_stream("#foo")), msg);
        assert.throws(() => vm.read(string_input_stream("#b")), msg);

    });

    it("Unknown other characters after sharpsign cause an error.", () => {

        const msg = "Illegal dispatching character";
        assert.throws(() => vm.read(string_input_stream("#_")), msg);
        assert.throws(() => vm.read(string_input_stream("# ")), msg);

    });

});

describe("Reader Macros", () => {

    it("Test set_macro_character() with terminating char.", () => {

        assert.isFalse(vm.is_macro_character("~"));
        assert.isFalse(vm.is_terminating_macro_character("~"));

        vm.set_macro_character("~", function(stream, b) {
            assert.equal(b, "~");
            // Note that reader macro has to return option.
            return vm.some(vm.list(vm.sym("blargh"), vm.read(stream)));
        }, false);

        assert(vm.is_macro_character("~"));
        assert(vm.is_terminating_macro_character("~"));

        assert(vm.equal(vm.read(string_input_stream("(1 ~foo ~bar 2)")),
                        vm.read(string_input_stream("(1 (blargh foo) (blargh bar) 2)"))));

        assert(vm.equal(vm.read(string_input_stream("(foo~bar)")),
                        vm.read(string_input_stream("(foo (blargh bar))"))));

        assert(vm.equal(vm.read(string_input_stream("(|foo|~bar)")),
                        vm.read(string_input_stream("(foo (blargh bar))"))));

        assert.throws(() => vm.read(string_input_stream("~")), "EOF");
        assert.throws(() => vm.read(string_input_stream("(foo ~")), "EOF");
        assert.throws(() => vm.read(string_input_stream("(foo ~)")), "Unbalanced parenthesis");

        vm.unset_macro_character("~");
        assert.isFalse(vm.is_macro_character("~"));
        assert.isFalse(vm.is_terminating_macro_character("~"));

    });

    it("Test set_macro_character() with non-terminating char.", () => {

        assert.isFalse(vm.is_macro_character("~"));
        assert.isFalse(vm.is_terminating_macro_character("~"));

        vm.set_macro_character("~", function(stream, b) {
            assert.equal(b, "~");
            // Note that reader macro has to return option.
            return vm.some(vm.list(vm.sym("blargh"), vm.read(stream)));
        }, true);

        assert(vm.is_macro_character("~"));
        assert.isFalse(vm.is_terminating_macro_character("~"));

        assert(vm.equal(vm.read(string_input_stream("(1 ~foo ~bar 2)")),
                        vm.read(string_input_stream("(1 (blargh foo) (blargh bar) 2)"))));

        assert(vm.equal(vm.read(string_input_stream("(foo~bar)")),
                        vm.read(string_input_stream("(foo~bar)"))));

        assert(vm.equal(vm.read(string_input_stream("(|foo|~bar)")),
                        vm.read(string_input_stream("(foo (blargh bar))"))));

        assert.throws(() => vm.read(string_input_stream("~")), "EOF");
        assert.throws(() => vm.read(string_input_stream("(foo ~")), "EOF");
        assert.throws(() => vm.read(string_input_stream("(foo ~)")), "Unbalanced parenthesis");

        vm.unset_macro_character("~");

    });

    it("Test read_delimited_list().", () => {

        assert(vm.equal(vm.read_delimited_list(string_input_stream("1 2 3 ]"), "]"),
                        vm.read(string_input_stream("(1 2 3)"))));
        assert(vm.equal(vm.read_delimited_list(string_input_stream("1 2 . 3 ]"), "]"),
                        vm.read(string_input_stream("(1 2 . 3)"))));

    });

});

describe("eval_{stream,string,js_string}", () => {

    it("Test eval_{stream,string,js_string}().", () => {

        const eval_x_funs = [
            (js_string, env) => {
                const stream = new vm.String_input_stream(vm.str(js_string));
                return vm.eval_stream(stream, env);
            },
            (js_string, env) => vm.eval_string(vm.str(js_string), env),
            (js_string, env) => vm.eval_js_string(js_string, env)
        ];

        for (const eval_x_fun of eval_x_funs) {

            /*
             * Test that it evaluates forms and returns the final result.
             */

            assert(vm.equal(eval_x_fun(""), vm.void()));
            assert(vm.equal(eval_x_fun("; Hello"), vm.void()));
            assert(vm.equal(eval_x_fun("1 2 3"), vm.num(3)));

            /*
             * Test that it uses the VM's root environment by default.
             */

            assert(eval_x_fun("#'%%vau") instanceof vm.Built_in_operator);

            /*
             * Test that another environment can be supplied.
             */
            assert.throws(() => eval_x_fun("#'%%vau", vm.make_environment()),
                          "Unbound function: %%vau");

            /*
             * Test that suspensions are not swallowed.
             */
            assert.throws(() => eval_x_fun("1 (take-subcont 'p k k) 2"),
                          "Prompt not found");

        }

    });

});
