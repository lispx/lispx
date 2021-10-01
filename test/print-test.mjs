import { assert } from "chai";

import { VM } from "lispx-vm";

const vm = new VM();

function string_output_stream()
{
    return new vm.String_output_stream();
}

function write_to_string(object)
{
    const st = new vm.String_output_stream();
    vm.write(object, st);
    return st.get_string();
}

function write_to_js_string(object)
{
    return write_to_string(object).to_js_string();
}

describe("Printer", () => {

    it("Test writing objects.", () => {

        const examples = {
            // JS objects write stupidly for now
            "#<object>": undefined,
            "#<object>": null,
            "#<object>": true,
            "#<object>": 12,
            "#<object>": "foo",
            "#<object>": {},
            "#<object>": [],
            // Unreadable objects
            "#<string-output-stream>": string_output_stream(),
            // Standard objects
            "#<error>":
            new vm.Error(),
            "#<type-error :datum 12 :expected-type boolean>":
            new vm.Type_error(vm.num(12), vm.sym("boolean")),
            // Constants
            "#t": vm.t(),
            "#f": vm.f(),
            "#nil": vm.nil(),
            "#void": vm.void(),
            "#ignore": vm.ignore(),
            // Numbers
            "-111111111111.111111111": vm.num("-111111111111.111111111"),
            "0": vm.num("0"),
            "111111111111.111111111": vm.num("111111111111.111111111"),
            // Strings
            "\"foo-bar\"": vm.str("foo-bar"),
            // Symbols
            "foo-bar": vm.sym("foo-bar"),
            "|foo bar|": vm.sym("foo bar"),
            "Iñtërnâtiônàlizætiøn":
            vm.sym("Iñtërnâtiônàlizætiøn"),
            "|Iñtërnâtiônàlizætiøn Iñtërnâtiônàlizætiøn|":
            vm.sym("Iñtërnâtiônàlizætiøn Iñtërnâtiônàlizætiøn"),
            // Keywords
            ":foo-bar": vm.kwd("foo-bar"),
            ":|foo bar|": vm.kwd("foo bar"),
            // Function symbols
            "#'foo-bar": vm.fsym("foo-bar"),
            "#'|foo bar|": vm.fsym("foo bar"),
            // Class symbols
            "#$foo-bar": vm.csym("foo-bar"),
            "#$|foo bar|": vm.csym("foo bar"),
            // Conses
            "(1)": vm.list(vm.num(1)),
            "((1))": vm.list(vm.list(vm.num(1))),
            "(1 2 3)": vm.list(vm.num(1), vm.num(2), vm.num(3)),
            "(1 2 . 3)": vm.list_star(vm.num(1), vm.num(2), vm.num(3)),
        };

        for (const [string, object] of Object.entries(examples)) {
            assert(vm.equal(string, write_to_js_string(object)));
        }

    });

    it("Test writing objects whose output depends on *PRINT-ESCAPE*.", () => {

        // Lisp object, *PRINT-ESCAPE* = #T, *PRINT-ESCAPE* = #F
        const examples = [
            // Strings
            [vm.str("foo"), "\"foo\"", "foo"],
            [vm.str("fo\"o"), "\"fo\\\"o\"", "fo\"o"],
            [vm.str("fo\no"), "\"fo\no\"", "fo\no"],
            [vm.str("fo\\o"), "\"fo\\\\o\"", "fo\\o"],
            [vm.str("foo|bar"), "\"foo|bar\"", "foo|bar"],
            // Normal symbols
            [vm.sym("foo"), "foo", "foo"],
            [vm.sym("foo-bar"), "foo-bar", "foo-bar"],
            [vm.sym("let*"), "let*", "let*"],
            // Symbols that parse as numbers
            [vm.sym("0"), "|0|", "0"],
            [vm.sym("12"), "|12|", "12"],
            [vm.sym("-12.12"), "|-12.12|", "-12.12"],
            // Symbols that contain whitespace
            [vm.sym("foo bar"), "|foo bar|", "foo bar"],
            [vm.sym("fo\no"), "|fo\no|", "fo\no"],
            // Symbols that contain terminating characters
            [vm.sym("fo|o"), "|fo\\|o|", "fo|o"],
            [vm.sym("fo(o"), "|fo(o|", "fo(o"],
            [vm.sym("fo\"o"), "|fo\"o|", "fo\"o"],
            // Symbols that contain the escape character
            [vm.sym("fo\\o"), "|fo\\\\o|", "fo\\o"],
        ];

        for (const [object, escaped, not_escaped] of examples) {
            const escaped_result = vm.progv([vm.PRINT_ESCAPE], [vm.t()],
                                            () => write_to_js_string(object));
            assert.strictEqual(escaped, escaped_result);
            const not_escaped_result = vm.progv([vm.PRINT_ESCAPE], [vm.f()],
                                                () => write_to_js_string(object));
            assert.strictEqual(not_escaped, not_escaped_result);
        }

    });

});
