/*
 * Test Utilities.
 */

import { assert } from "chai";

/*
 * Verify that a class is properly set up.
 */
export function check_class(vm, name, js_class, js_superclass, js_metaclass)
{
    check_class_name(vm, js_class, name);
    check_superclass(vm, js_class, js_superclass);
    check_metaclass(vm, js_class, js_metaclass);
    check_class_linkage(vm, js_class);
};

/*
 * Verify that a class is properly named and registered in the root environment.
 */
export function check_class_name(vm, js_class, name)
{
    const lisp_class = vm.lisp_class(js_class);
    const name_sym = vm.sym(name);

    // The name is an ordinary symbol in the variable namespace.
    assert.equal(lisp_class.get_name(), name_sym);

    // The symbol it's registered under in the environment is a
    // symbol in the class namespace.
    assert.equal(vm.get_environment().lookup(name_sym.to_class_symbol()),
                 lisp_class);
};

/*
 * Verify that a class is properly connected with its superclass.
 */
export function check_superclass(vm, js_class, js_superclass)
{
    assert.equal(vm.lisp_class(js_class).get_superclass(),
                 vm.lisp_class(js_superclass));

    assert(js_class.prototype instanceof js_superclass);
};

/*
 * Verify that a class is properly connected with its metaclass.
 */
export function check_metaclass(vm, js_class, js_metaclass)
{
    // Thanks to our nice object system, the following things
    // are both true:

    // 1) The Lisp class metaobject points to the Lisp
    // metaclass metaobject.
    assert.equal(vm.class_of(vm.lisp_class(js_class)),
                 vm.lisp_class(js_metaclass));

    // 2) The Lisp class metaobject is also an instance of the
    // JS metaclass.
    assert.instanceOf(vm.lisp_class(js_class),
                      js_metaclass);

    // (and of course of CLASS and OBJECT, too)
    assert.instanceOf(vm.lisp_class(js_class),
                      vm.Class);
    assert.instanceOf(vm.lisp_class(js_class),
                      vm.Object);
};

/*
 * Verify that a JS class is properly linked to its class metaobject.
 */
export function check_class_linkage(vm, js_class)
{
    assert.equal(vm.lisp_class(js_class).get_js_class(),
                 js_class);
};
