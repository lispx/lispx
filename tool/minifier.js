/*
 * A minifier for LispX files that plugs into Webpack as a loader.
 *
 * Removes comments, docstrings, and whitespace from the Lisp source.
 */

const VM = require("../dist/lispx-vm.umd.js").VM;
const vm = new VM();

module.exports = function (source)
{
    const results = [];
    const stream = new vm.String_input_stream(vm.str(source));
    let form;
    while ((form = vm.read(stream, false)) !== vm.void()) {
        results.push(vm.write_to_js_string(form));
    }
    return results.join("");
};
