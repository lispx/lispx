/*
 * A minifier for LispX files that plugs into Webpack as a loader.
 *
 * Removes comments, docstrings, and whitespace from the Lisp source.
 */

const VM = require("../dist/lispx-vm.umd.js").VM;
const vm = new VM();

module.exports = function (source)
{
    return source;
};
