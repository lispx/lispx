import { assert } from "chai";

import { VM } from "lispx-vm";

const vm = new VM();

function string_input_stream(js_string)
{
    return new vm.String_input_stream(vm.str(js_string));
}

function string_output_stream()
{
    return new vm.String_output_stream();
}

describe("String Input Streams", () => {

    it("UTF-8 bytes can be read from a string.", () => {

        const st = string_input_stream("bar");

        assert.throws(() => st.unread_byte("x"), "Cannot unread");

        assert.equal(st.peek_byte(false, false, -1), "b");

        assert.equal(st.peek_byte(false, false, -1), "b");

        assert.equal(st.read_byte(false, -1), "b");

        assert.throws(() => st.unread_byte("x"), "Cannot unread");

        st.unread_byte("b");

        assert.equal(st.peek_byte(false, false, -1), "b");

        assert.equal(st.read_byte(false, -1), "b");

        assert.equal(st.peek_byte(false, false, -1), "a");

        assert.equal(st.read_byte(false, -1), "a");

        assert.equal(st.peek_byte(false, false, -1), "r");

        assert.equal(st.read_byte(false, -1), "r");

        assert.equal(st.peek_byte(false, false, -1), -1);

        assert.equal(st.peek_byte(false, false, -1), -1);

        assert.equal(st.read_byte(false, -1), -1);

        st.unread_byte("r");

        assert.equal(st.peek_byte(false, false, -1), "r");

        assert.equal(st.read_byte(false, -1), "r");

        assert.equal(st.read_byte(false, -1), -1);

        assert.throws(() => st.peek_byte(false, true), "EOF");

        assert.throws(() => st.read_byte(true), "EOF");

        assert.throws(() => st.read_byte(true), "EOF");

        assert.equal(st.peek_byte(false, false, -1), -1);

        assert.equal(st.read_byte(false, -1), -1);

        st.unread_byte("r");

        assert.equal(st.peek_byte(false, false, -1), "r");

        assert.equal(st.read_byte(false, -1), "r");

        assert.equal(st.peek_byte(false, false, -1), -1);

        assert.equal(st.read_byte(false, -1), -1);

    });

    it("read_byte() has convenient default arguments.", () => {

        const st = string_input_stream("bar");

        assert.equal(st.read_byte(), "b");
        assert.equal(st.read_byte(), "a");
        assert.equal(st.read_byte(), "r");

        assert.equal(st.read_byte(false, -1), -1);
        assert.equal(st.read_byte(false), vm.void());
        assert.throws(() => st.read_byte(), "EOF");

    });

    it("Test skipping whitespace in peek_byte().", () => {

        const st = string_input_stream("  b a r");

        assert.equal(st.peek_byte(false), " ");
        assert.equal(st.read_byte(), " ");
        assert.equal(st.peek_byte(false), " ");
        assert.equal(st.peek_byte(true), "b");
        assert.equal(st.read_byte(), "b");
        assert.equal(st.peek_byte(false), " ");
        assert.equal(st.peek_byte(true), "a");
        assert.equal(st.read_byte(), "a");
        assert.equal(st.peek_byte(true), "r");
        assert.equal(st.read_byte(), "r");
        assert.equal(st.peek_byte(true, false, -1), -1);
        assert.equal(st.peek_byte(false, false, -1), -1);

    });

});

describe("String Output Streams", () => {

    it("UTF-8 bytes can be written to a string.", () => {

        const st = string_output_stream();

        assert(vm.equal(st.get_string(), vm.str("")));

        assert.equal(st.write_byte("a"), "a");

        assert(vm.equal(st.get_string(), vm.str("a")));

        assert.equal(st.write_byte("b"), "b");

        assert(vm.equal(st.get_string(), vm.str("ab")));

        assert(vm.equal(st.write_string(vm.str("Iñtërnâtiônàlizætiøn")),
                        vm.str("Iñtërnâtiônàlizætiøn")));

        assert(vm.equal(st.get_string(), vm.str("abIñtërnâtiônàlizætiøn")));

        assert.equal(st.write_byte("c"), "c");

        assert(vm.equal(st.get_string(), vm.str("abIñtërnâtiônàlizætiønc")));

    });

    it("Test force_output()", () => {

        /*
         * force_output() is a no-op for string output streams,
         * just test that it's implemented.
         */

        const st = string_output_stream();

        assert(vm.equal(st.force_output(), vm.void()));

    });

    it("Test fresh_line()", () => {

        const st = string_output_stream();

        assert(vm.equal(st.fresh_line(), vm.t()));

        assert.equal(st.write_byte("a"), "a");

        assert(vm.equal(st.fresh_line(), vm.t()));

        assert(vm.equal(st.get_string(), vm.str("\na\n")));

    });

});
