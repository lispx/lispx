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
        results.push(vm.write_to_js_string(minify(form)));
    }
    return results.join("");
};

function minify(form)
{
    if (form instanceof vm.Cons) {
        const car = form.car();
        if (car instanceof vm.Symbol) {
            const name = car.get_string().to_js_string();
            const minifier = MINIFIERS[name];
            if (minifier) {
                return minifier(form)
            }
        }
    }
    return form;
}

const MINIFIERS = {
    "def": minify_def,
    "%def": minify_def,
    "defdynamic": minify_defdynamic,
    "defun": minify_defun,
    "defmacro": minify_defun,
    "defmethod": minify_defun,
    "defexpr": minify_defexpr
};

// (def name value docstring?)
//  0   1    2     3
function minify_def(form)
{
    return vm.list_subseq(form, 0, 3);
}

// (defdynamic name value-and-docstring?)
//  0          1    2
function minify_defdynamic(form)
{
    if (vm.list_length(form) > 2)
        return vm.list_subseq(form, 0, 3);
    else
        return form;
}

// (defun name params docstring? rest ...)
//  0     1    2      3          4
function minify_defun(form)
{
    return minify_defunlike_form(form, 3);
}

// (defexpr name params env-param docstring? rest ...)
//  0       1    2      3         4          5
function minify_defexpr(form)
{
    return minify_defunlike_form(form, 4);
}

function minify_defunlike_form(form, docstring_idx)
{
    // Only remove docstring if it's not the actual only body form.
    if (vm.list_length(form) > (docstring_idx + 1)) {
        return vm.append(vm.list_subseq(form, 0, docstring_idx),
                         vm.list_subseq(form, docstring_idx + 1));
    } else {
        return form;
    }
}
