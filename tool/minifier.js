/*
 * A minifier for LispX files that plugs into Webpack as a loader.
 *
 * Removes comments, docstrings, and most whitespace from the Lisp
 * source.
 */

const VM = require("../dist/lispx-vm.umd.js").VM;
const vm = new VM();

/*
 * Gets Lisp source string as input, returns minified string.
 */
module.exports = function (source)
{
    vm.assert_type(source, "string");
    const results = [];
    const stream = new vm.String_input_stream(vm.str(source));
    const eof = {};
    let form;
    while ((form = vm.read(stream, false, eof)) !== eof) {
        results.push(vm.write_to_js_string(minify(form)));
    }
    // Need to add a space or otherwise two consecutive symbols at the
    // toplevel would get concatenated.  Could be cleverer and take
    // into consideration whether next form starts with terminating
    // macro char, and drop the space if it does.  (We could also dive
    // into nested forms and remove spaces from there).
    return results.join(" ");
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
    "defexpr": minify_defexpr,
    "defgeneric": minify_defgeneric,
    "defclass": minify_defclass
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

// (defgeneric name (args) . properties)
//  0          1    2        3
function minify_defgeneric(form)
{
    return vm.list_subseq(form, 0, 3);
}

// (defclass name (superclass?) slot-specs . properties)
//  0        1    2             3            4
function minify_defclass(form)
{
    return vm.list_subseq(form, 0, 4);
}

/*
 * Self-test -- this should really be run from somewhere else but I
 * don't know where atm (since it's Node code, and the normal test
 * suite is usually run in the browser only), so we just always run it
 * "in prod".
 */
const input = `
(defun foo ())
(defun bar () "Not a docstring") ; a comment
(defun quux () "A docstring" 100 1000)
(def x 1 "A docstring")
(def x 2) (def x 3)
(%def x 1 "A docstring")
x
y
(defdynamic x)
12
(foo)
(defdynamic y 2)
; a comment
(defdynamic z 3 "A docstring")
(defclass a () ())

; Some whitespace
                    x

         (defclass a () () (:documentation "A docstring"))

(defgeneric foo (self) (:documentation "A docstring"))
(defgeneric foo (self))
(defmethod foo ((self a)) "A docstring" 12)
(defmethod foo ((self a)) "Not a docstring")
(defmethod foo ((self a)))
(defexpr foo () env)
(defexpr foo () env "Not a docstring")
(defexpr foo () env "A docstring" 100 1000)
`;

const expected = [
    `(defun foo ())`,
    `(defun bar () "Not a docstring")`,
    `(defun quux () 100 1000)`,
    `(def x 1)`,
    `(def x 2)`,
    `(def x 3)`,
    `(%def x 1)`,
    `x`,
    `y`,
    `(defdynamic x)`,
    `12`,
    `(foo)`,
    `(defdynamic y 2)`,
    `(defdynamic z 3)`,
    `(defclass a () ())`,
    `x`,
    `(defclass a () ())`,
    `(defgeneric foo (self))`,
    `(defgeneric foo (self))`,
    `(defmethod foo ((self a)) 12)`,
    `(defmethod foo ((self a)) "Not a docstring")`,
    `(defmethod foo ((self a)))`,
    `(defexpr foo () env)`,
    `(defexpr foo () env "Not a docstring")`,
    `(defexpr foo () env 100 1000)`,
].join(" ");

if (expected !== module.exports(input))
    throw "minifier borken";
