/*
 * Test Utilities.
 */

import { assert } from "chai";

/*
 * Verify that a class is properly connected with its superclass.
 */
export function check_superclass(vm, js_class, js_superclass)
{
    assert.equal(vm.lisp_class(js_class).get_superclass(),
                 vm.lisp_class(js_superclass));

    assert(js_class.prototype instanceof js_superclass);
}

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
}
