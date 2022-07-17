/*
 * LispX Virtual Machine
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Arbitrary-precision decimals.
 */
import Big from "big.js";

/*
 * Import VM modules.
 *
 * Currently, all modules are always loaded, but in the future it will
 * be possible to make them optional, so that one can obtain e.g. a
 * smaller VM without IO support.
 */
import { init_eval } from "./eval.mjs";
import { init_control } from "./control.mjs";
import { init_seq } from "./seq.mjs";
import { init_stream } from "./stream.mjs";
import { init_read } from "./read.mjs";
import { init_print } from "./print.mjs";
import { init_js } from "./js.mjs";
import { init_js_console } from "./js-console.mjs";

/*
 * A build system contraption loads the contents of the files into the
 * variables as strings.
 */
import boot_code from "./boot.lispx";
import cond_sys_code from "./cond-sys.lispx";
import stream_code from "./stream.lispx";
import read_code from "./read.lispx";
import print_code from "./print.lispx";
import js_code from "./js.lispx";

/*
 * A virtual machine is a Lisp interpreter.
 *
 * Multiple independent VMs can exist in the same JavaScript
 * program.
 */
export class VM
{
    /*
     * Creates a new VM.
     */
    constructor()
    {
        /*
         * Set up class hierarchy and other internal data structures.
         */
        init_vm(this);

        /*
         * Load modules.
         */
        init_eval(this);
        init_control(this);
        init_seq(this);
        init_stream(this);
        init_read(this);
        init_print(this);
        init_js(this);
        init_js_console(this);

        /*
         * Evaluate the bootstrap code.
         */
        this.eval_js_string(boot_code);
        this.eval_js_string(cond_sys_code);
        this.eval_js_string(stream_code);
        this.eval_js_string(read_code);
        this.eval_js_string(print_code);
        this.eval_js_string(js_code);
    }

    /*
     * Creates a Lisp string from a JavaScript string.
     */
    str(js_string)
    {
        return new this.String(this.utf8_encode(js_string));
    }

    /*
     * Gets or creates an interned symbol from a JavaScript string.
     *
     * The symbol's namespace defaults to the variable namespace.
     */
    sym(js_string, namespace = this.VARIABLE_NAMESPACE)
    {
        return this.intern(this.str(js_string), namespace);
    }

    /*
     * Gets or creates an interned function symbol from a JavaScript string.
     */
    fsym(js_string)
    {
        return this.sym(js_string, this.FUNCTION_NAMESPACE);
    }

    /*
     * Gets or creates an interned class symbol from a JavaScript string.
     */
    csym(js_string)
    {
        return this.sym(js_string, this.CLASS_NAMESPACE);
    }

    /*
     * Gets or creates an interned keyword symbol from a JS string.
     */
    kwd(js_string)
    {
        return this.sym(js_string, this.KEYWORD_NAMESPACE);
    }

    /*
     * Gets or creates an interned symbol from a Lisp string.
     *
     * The symbol's namespace defaults to the variable namespace.
     */
    intern(string, namespace = this.VARIABLE_NAMESPACE)
    {
        this.assert_type(string, this.String);
        this.assert_type(namespace, "string");
        const key = this.Symbol.make_key(string, namespace);
        const sym = this.symbols[key];
        if (sym !== undefined)
            return sym;
        else
            return this.symbols[key] = new this.Symbol(string, namespace);
    }

    /*
     * Creates a new number from a JS string or number.
     */
    num(js_string_or_number)
    {
        this.assert_type(js_string_or_number, this.type_or("string", "number"));
        return new this.Number(new Big(js_string_or_number));
    }

    /*
     * Constructs a new pair.
     */
    cons(car, cdr)
    {
        return new this.Cons(car, cdr);
    }

    /*
     * Constructs a new list from its arguments.
     */
    list(...objects)
    {
        return this.array_to_list(objects);
    }

    /*
     * Creates a new environment.
     *
     * If the parent environment is non-null, the new environment
     * will inherit bindings from it.
     */
    make_environment(parent = null)
    {
        return new this.Environment(parent);
    }

    /*
     * Value equality.
     *
     * Lisp classes can implement the equal_same_type() method to hook
     * into this.
     *
     * For non-Lisp objects, or Lisp objects of different classes,
     * it uses ===.
     */
    equal(a, b)
    {
        if (this.is_lisp_object(a) && this.is_lisp_object(b))
            if (this.class_of(a) === this.class_of(b))
                return a.equal_same_type(b);
        return a === b;
    }

    /*
     * Ordering.  Returns a value less than 0, equal to 0, or greater
     * than 0 depending on whether a is smaller than, equal to, or
     * larger than b.
     *
     * Lisp classes can implement the compare_same_type() method to hook
     * into this.
     *
     * For non-Lisp objects, or Lisp objects of different classes,
     * it throws an error.
     */
    compare(a, b)
    {
        if (this.is_lisp_object(a) && this.is_lisp_object(b))
            if (this.class_of(a) === this.class_of(b))
                return a.compare_same_type(b);
        throw this.make_type_error(b, this.class_of(a).get_js_class());
    }

    /*
     * Returns true if an object is a Lisp object, false if it is a
     * JavaScript object.
     */
    is_lisp_object(obj)
    {
        return obj instanceof this.Object;
    }

    /*
     * Returns the Lisp class (metaobject) of an object.
     *
     * For non-Lisp objects, returns OBJECT, so this can be called on
     * any object.
     */
    class_of(obj)
    {
        if (this.is_lisp_object(obj))
            return obj.lisp_class;
        else
            return this.lisp_class(this.Object);
    }

    /*
     * Returns the Lisp class (metaobject) of a JS class.
     */
    lisp_class(js_class)
    {
        this.assert_type(js_class, "function");
        this.assert(this.has_lisp_class(js_class));
        return js_class.prototype.lisp_class;
    }

    /*
     * Returns true if a JS class has a Lisp class metaobject, false
     * otherwise.  This can be used to discover whether the class
     * belongs to the VM or is an unrelated JS class.
     */
    has_lisp_class(js_class)
    {
        return this.has_type(js_class.prototype.lisp_class, this.Class);
    }

    /*
     * Returns the VM's root environment;
     */
    get_environment()
    {
        return this.environment;
    }
}

/*
 * Initializes a virtual machine.
 */
function init_vm(vm)
{
    /*** Objects ***/

    /*
     * The root class of the class hierarchy.
     */
    vm.Object = class Lisp_object
    {
        /*
         * Classes can override this method to provider value equality
         * semantics.
         *
         * Don't call directly, use vm.equal() instead.
         *
         * Other is guaranteed to have the same class as this object.
         *
         * Defaults to reference equality (===).
         */
        equal_same_type(other)
        {
            return this === other;
        }

        /*
         * Classes can override this method to provider ordering
         * semantics.
         *
         * Don't call directly, use vm.compare() instead.
         *
         * Other is guaranteed to have the same class as this object.
         *
         * Throws an error by default.
         */
        compare_same_type(other)
        {
            vm.abstract_method();
        }
    };

    /*
     * UTF-8 string.
     */
    vm.String = class Lisp_string extends vm.Object
    {
        /*
         * Creates a new Lisp string from UTF-8 bytes encoded as
         * UTF-16 code units.
         *
         * Use vm.str() instead unless you know what you are doing.
         */
        constructor(utf8_bytes)
        {
            super();
            vm.assert_type(utf8_bytes, "string");
            this.utf8_bytes = utf8_bytes;
        }

        /*
         * Strings are equal if their UTF-8 bytes are equal.
         */
        equal_same_type(other)
        {
            return this.get_utf8_bytes() === other.get_utf8_bytes();
        }

        /*
         * Creates a UTF-16 JavaScript string from a Lisp string.
         */
        to_js_string()
        {
            return vm.utf8_decode(this.get_utf8_bytes());
        }

        /*
         * Returns the UTF-8 bytes of a Lisp string, encoded as UTF-16
         * code units.
         */
        get_utf8_bytes()
        {
            return this.utf8_bytes;
        }
    };

    /*
     * Takes a string containing UTF-16 code units and returns a string in
     * which every code unit represents a UTF-8 byte.
     */
    vm.utf8_encode = (js_string) =>
    {
        vm.assert_type(js_string, "string");
        return unescape(encodeURIComponent(js_string));
    };

    /*
     * Takes a string containing UTF-8 bytes encoded as UTF-16 code units,
     * and returns a bona fide UTF-16 string.
     */
    vm.utf8_decode = (bytes) =>
    {
        vm.assert_type(bytes, "string");
        return decodeURIComponent(escape(bytes));
    };

    /*
     * Symbol, an identifier.
     *
     * A symbol has a name, which is a string, and a namespace.
     *
     * The namespaces are variable, function, class, and keyword.
     *
     * Namespaces allow us to have objects of different type with the
     * same name.  For example, there can be an ordinary variable,
     * a function, and a class called LIST in the same environment.
     *
     * The keyword namespace is slightly different: symbols in it
     * evaluate to themselves.
     */
    vm.Symbol = class Lisp_symbol extends vm.Object
    {
        /*
         * Creates a new, uninterned symbol with a string as name, and
         * a namespace.
         *
         * Do not use this directly, always use vm.sym(), as uninterned
         * symbols are not currently fully supported. (In the common
         * Lisp semantics, uninterned symbols can be used e.g. as variable
         * names, and will be distinct from each other, even if they have
         * the same name.  Our current implementation does not support this,
         * however, and uses symbol names as keys, so distinct uninterned
         * symbols with the same name would clash.  The benefit of this is
         * easy introspectability of environments in the JS developer tools.)
         */
        constructor(string, namespace)
        {
            super();
            this.string = vm.assert_type(string, vm.String);
            this.namespace = vm.assert_type(namespace, "string");
        }

        /*
         * Returns the name string of the symbol.
         */
        get_string()
        {
            return this.string;
        }

        /*
         * Returns the namespace of the symbol.
         */
        get_namespace()
        {
            return this.namespace;
        }

        /*
         * Return the symbol with the same name in another namespace.
         *
         * Prefer one of the to_x_symbol() methods, below.
         */
        to_namespace(namespace)
        {
            vm.assert_type(namespace, "string");
            return vm.intern(this.get_string(), namespace);
        }

        /*
         * Return the symbol with the same name in the variable namespace.
         */
        to_variable_symbol()
        {
            return this.to_namespace(vm.VARIABLE_NAMESPACE);
        }

        /*
         * Return the symbol with the same name in the function namespace.
         */
        to_function_symbol()
        {
            return this.to_namespace(vm.FUNCTION_NAMESPACE);
        }

        /*
         * Return the symbol with the same name in the class namespace.
         */
        to_class_symbol()
        {
            return this.to_namespace(vm.CLASS_NAMESPACE);
        }

        /*
         * Return the symbol with the same name in the keyword namespace.
         */
        to_keyword_symbol()
        {
            return this.to_namespace(vm.KEYWORD_NAMESPACE);
        }

        /*
         * The symbol key is a string under which the symbol is
         * stored in associative data structures like the symbol table
         * and environments.
         */
        get_key()
        {
            return vm.Symbol.make_key(this.get_string(), this.get_namespace());
        }

        /*
         * Utility method that creates a symbol key.
         */
        static make_key(string, namespace)
        {
            return string.get_utf8_bytes() + "_" + namespace;
        }
    }

    /*
     * The namespaces.
     */
    vm.VARIABLE_NAMESPACE = "variable";
    vm.FUNCTION_NAMESPACE = "function";
    vm.CLASS_NAMESPACE = "class";
    vm.KEYWORD_NAMESPACE = "keyword";

    /*
     * Arbitrary-precision decimal number.
     */
    vm.Number = class Lisp_number extends vm.Object
    {
        /*
         * Construct a new number from a Big.
         *
         * Use vm.num() instead of calling this directly.
         */
        constructor(big)
        {
            super();
            vm.assert_type(big, Big);
            this.big = big;
        }

        /*
         * Numbers are equal if their underlying Bigs are equal.
         */
        equal_same_type(other)
        {
            return this.get_big().eq(other.get_big());
        }

        /*
         * Numbers compare by comparing their underlying Bigs.
         */
        compare_same_type(other)
        {
            return this.get_big().cmp(other.get_big());
        }

        /*
         * Transforms a Lisp number into a JS one.
         */
        to_js_number()
        {
            return this.get_big().toNumber();
        }

        /*
         * Transforms a Lisp number into a string.
         */
        to_string()
        {
            return vm.str(this.get_big().toFixed());
        }

        /*
         * Internal accessor.
         */
        get_big() { return this.big; }
    }

    vm.add = (a, b) =>
    {
        vm.assert_type(a, vm.Number);
        vm.assert_type(b, vm.Number);
        return new vm.Number(a.get_big().add(b.get_big()));
    };

    vm.subtract = (a, b) =>
    {
        vm.assert_type(a, vm.Number);
        vm.assert_type(b, vm.Number);
        return new vm.Number(a.get_big().minus(b.get_big()));
    };

    vm.multiply = (a, b) =>
    {
        vm.assert_type(a, vm.Number);
        vm.assert_type(b, vm.Number);
        return new vm.Number(a.get_big().times(b.get_big()));
    };

    vm.divide = (a, b) =>
    {
        vm.assert_type(a, vm.Number);
        vm.assert_type(b, vm.Number);
        return new vm.Number(a.get_big().div(b.get_big()));
    };

    /*
     * Mr. Boole's fabulous invention.
     */
    vm.Boolean = class Lisp_boolean extends vm.Object
    {
        /*
         * Construct a new boolean from a JS boolean.
         *
         * Use vm.t() or vm.f() instead of calling this.
         */
        constructor(js_bool)
        {
            super();
            this.bool = vm.assert_type(js_bool, "boolean");
        }

        /*
         * Transforms a Lisp boolean into a JS one.
         */
        to_js_boolean()
        {
            return this.bool;
        }
    }

    /*
     * Superclass of conses and #NIL.
     */
    vm.List = class Lisp_list extends vm.Object {};

    /*
     * A pair of two values, the most fundamental data structure.
     */
    vm.Cons = class Lisp_cons extends vm.List
    {
        /*
         * Construct a new cons.
         *
         * Use vm.cons() instead of calling this.
         */
        constructor(car, cdr)
        {
            super();
            this._car = car;
            this._cdr = cdr;
        }

        /*
         * Conses are equal if their car and cdr are equal.
         */
        equal_same_type(other)
        {
            return vm.equal(this.car(), other.car()) && vm.equal(this.cdr(), other.cdr());
        }

        /*
         * Contents of the address part of the register.
         */
        car() { return this._car; }
        set_car(car) { this._car = car; }

        /*
         * Contents of the decrement part of the register.
         */
        cdr() { return this._cdr; }
        set_cdr(cdr) { this._cdr = cdr; }
    }

    /*
     * #NIL, the empty list.
     */
    vm.Nil = class Lisp_nil extends vm.List
    {
        /*
         * Construct #NIL.
         *
         * Use vm.nil() instead of calling this.
         */
        constructor()
        {
            super();
        }
    }

    /*
     * #VOID, the value that is used to indicate that no
     * interesting value was produced, e.g. by an empty (PROGN).
     */
    vm.Void = class Lisp_void extends vm.Object
    {
        /*
         * Construct #VOID.
         *
         * Use vm.void() instead of calling this.
         */
        constructor()
        {
            super();
        }
    }

    /*
     * #IGNORE, used on the left hand side of a definition
     * if no binding is desired.
     */
    vm.Ignore = class Lisp_ignore extends vm.Object
    {
        /*
         * Construct #IGNORE.
         *
         * Use vm.ignore() instead of calling this.
         */
        constructor()
        {
            super();
        }
    }

    /*
     * A lexical environment maps symbols to values.
     *
     * It may have a parent environment, in which bindings are looked
     * up if they are not found.
     */
    vm.Environment = class Lisp_environment extends vm.Object
    {
        /*
         * Creates an environment with an optional parent.
         *
         * The bindings object inherits from the parent's bindings,
         * if it exists.
         *
         * Use vm.make_environment() instead of calling this directly.
         */
        constructor(parent = null)
        {
            super();
            if (parent !== null)
                vm.assert_type(parent, vm.Environment);
            this.parent = parent;
            this.bindings = Object.create(
                (parent === null) ? null : parent.get_bindings()
            );
        }

        /*
         * Assigns a value to a symbol in this environment.
         */
        put(symbol, value)
        {
            vm.assert_type(symbol, vm.Symbol);
            this.bindings[symbol.get_key()] = value;
        }

        /*
         * Returns the value of a symbol in this environment,
         * or one of its parent environments.
         *
         * Throws an error if the symbol is not bound.
         */
        lookup(symbol)
        {
            /*
             * There are some mild shenanigans ahead because we want to
             * achieve a trifecta of objectives:
             *
             * - Environments should be able to store any JS value
             *   whatsoever, including undefined.
             *
             * - Accessing an unbound variable should throw (and not return
             *   undefined as in JS).
             *
             * - Lookup should use JS's optimized code for looking up
             *   things in prototype hierarchies.
             *
             * Optimize for the common case that the symbol exists and
             * is not bound to undefined.
             */
            vm.assert_type(symbol, vm.Symbol);
            const key = symbol.get_key();
            const val = this.bindings[key];
            if (val !== undefined) {
                return val;
            } else {
                /*
                 * We got undefined, so now we need to check whether
                 * the symbol is bound to undefined, or doesn't exist.
                 */
                if (key in this.bindings) {
                    /*
                     * The symbol exists, so its value is indeed undefined.
                     */
                    return undefined;
                } else {
                    /*
                     * The symbol is unbound.
                     */
                    throw new vm.Unbound_symbol_error(symbol, this);
                }
            }
        }

        /*
         * Returns true if a symbol is bound in this environment
         * or one of its ancestors, false otherwise.
         */
        is_bound(symbol)
        {
            vm.assert_type(symbol, vm.Symbol);
            const key = symbol.get_key();
            return key in this.bindings;
        }

        /*
         * Internal accessor.
         */
        get_bindings() { return this.bindings; }
    }

    /*** Classes ***/

    /*
     * Superclass of all class metaobjects.
     *
     * This class is abstract; all concrete classes are instances of
     * either BUILT-IN-CLASS or STANDARD-CLASS.
     */
    vm.Class = class Lisp_class extends vm.Object
    {
        /*
         * Constructs a new class with the given name, JS constructor
         * function, and superclass.  Superclass can be null, but only
         * in case of the root of the class hierarchy, OBJECT.
         */
        constructor(name, js_class, superclass)
        {
            super();
            this.name = vm.assert_type(name, vm.Symbol);
            this.js_class = vm.assert_type(js_class, "function");
            this.superclass = vm.assert_type(superclass, vm.type_or(vm.TYPE_NULL, vm.Class));
        }

        /*
         * Creates or updates a method in this class.
         */
        add_method(name, method)
        {
            vm.assert_type(method, vm.Operator);
            const key = this.method_key(name);
            this.get_js_class().prototype[key] = method;
        }

        /*
         * Searches for a method in this class or a superclass.
         *
         * Throws an error if the method is not found.
         */
        find_method(name)
        {
            const key = this.method_key(name);
            const method = this.get_js_class().prototype[key];
            if (method !== undefined)
                return method;
            else
                throw new vm.Unbound_method_error(this, name);
        }

        /*
         * Internal method that constructs the key under which a
         * method is stored in the JS class.
         */
        method_key(method_name)
        {
            vm.assert_type(method_name, vm.Symbol);
            const bytes = method_name.get_string().get_utf8_bytes();
            return "lisp_method_" + bytes;
        }

        /*
         * Accessors.
         */
        get_name() { return this.name; }
        get_js_class() { return this.js_class; }
        get_superclass() { return this.superclass; }
    };

    /*
     * Metaclass of built-in classes like STRING and CONS.
     */
    vm.Built_in_class = class Lisp_built_in_class extends vm.Class
    {
        constructor(name, js_class, superclass)
        {
            super(name, js_class, superclass);
        }
    };

    /*
     * Metaclass of classes defined with DEFCLASS by the user.
     */
    vm.Standard_class = class Lisp_standard_class extends vm.Class
    {
        constructor(name, js_class, superclass)
        {
            super(name, js_class, superclass);
        }
    };

    /*
     * Superclass of classes defined with DEFCLASS by the user.
     */
    vm.Standard_object = class Lisp_standard_object extends vm.Object
    {
        /*
         * Retrieves the value of a slot of a standard object.
         */
        slot_value(slot_name)
        {
            const slot_key = this.slot_key(slot_name);
            if (this.hasOwnProperty(slot_key))
                return this[slot_key];
            else
                throw new vm.Unbound_slot_error(this, slot_name);
        }

        /*
         * Updates the value of a slot of a standard object.
         */
        set_slot_value(slot_name, slot_value)
        {
            const slot_key = this.slot_key(slot_name);
            return this[slot_key] = slot_value;
        }

        /*
         * Returns true if a slot is bound in this object,
         * false otherwise.
         */
        is_slot_bound(slot_name)
        {
            const slot_key = this.slot_key(slot_name);
            return slot_key in this;
        }

        /*
         * Internal method that constructs the key under
         * which a slot value is stored in the object.
         */
        slot_key(slot_name)
        {
            vm.assert_type(slot_name, vm.Symbol);
            const bytes = slot_name.get_string().get_utf8_bytes();
            return "lisp_slot_" + bytes;
        }
    };

    /*
     * Creates an instance of a standard class.
     *
     * Slots are initialized from the slot initializers, an array of
     * even length, alternately containing slot names and values.
     */
    vm.make_instance = (cls, ...slot_inits) =>
    {
        vm.assert_type(cls, vm.Standard_class);
        vm.assert((slot_inits.length % 2) === 0);
        const obj = new (cls.get_js_class())();
        for (let i = 0; i < slot_inits.length; i = i + 2) {
            const name = slot_inits[i];
            const value = slot_inits[i + 1];
            obj.set_slot_value(name, value);
        }
        return obj;
    };

    /*
     * Check if a class is a subclass of another class.
     *
     * A class is considered a subclass of itself.
     */
    vm.is_subclass = (sub_class, super_class) =>
    {
        vm.assert_type(sub_class, vm.Class);
        vm.assert_type(super_class, vm.Class);
        if (sub_class === super_class) {
            return true;
        } else {
            const js_sub_class = sub_class.get_js_class();
            const js_super_class = super_class.get_js_class();
            return js_sub_class.prototype instanceof js_super_class;
        }
    };

    /*
     * Creates a new standard class dynamically.
     */
    vm.make_standard_class = (name, lisp_super) =>
    {
        vm.assert_type(name, vm.Symbol);
        vm.assert_type(lisp_super, vm.Standard_class);
        const js_super = lisp_super.get_js_class();
        const js_class = class extends js_super
        {
            constructor()
            {
                super();
            }
        };
        const js_name = name.get_string().to_js_string();
        Object.defineProperty(js_class, "name", { value: js_name });
        const lisp_class = vm.bless_class(name, js_class, js_super, vm.Standard_class);
        return lisp_class;
    };

    /*
     * Change an existing standard class's superclass.
     */
    vm.reinitialize_standard_class = (lisp_class, lisp_super) =>
    {
        vm.assert_type(lisp_class, vm.Standard_class);
        vm.assert_type(lisp_super, vm.Standard_class);
        const js_class = lisp_class.get_js_class();
        const js_super = lisp_super.get_js_class();
        Object.setPrototypeOf(js_class.prototype, js_super.prototype);
        return lisp_class;
    };

    /*** Conditions ***/

    /*
     * Superclass of all objects that are signalled by Lisp.
     */
    vm.Condition = class Lisp_condition extends vm.Standard_object {};

    /*
     * Superclass of all Lisp errors.
     */
    vm.Error = class Lisp_error extends vm.Condition
    {
        constructor(message)
        {
            super();

            /*
             * Kludge to get a stack trace even though
             * we're not extending JS's Error.
             */
            const error = new Error(message);
            Object.getOwnPropertyNames(error)
                .forEach((name) => {
                    const desc = Object.getOwnPropertyDescriptor(error, name);
                    Object.defineProperty(this, name, desc);
                });
        }
    };

    /*
     * Signalled when an object doesn't match an expected type.
     *
     * The expected type should be a symbolic Lisp type specifier.
     *
     * See make_type_error().
     */
    vm.Type_error = class Lisp_type_error extends vm.Error
    {
        constructor(datum, expected_type)
        {
            super("Type assertion failed: expected " + vm.write_to_js_string(expected_type));
            this.lisp_slot_datum = datum;
            this["lisp_slot_expected-type"] = expected_type;
        }
    };

    /*
     * Creates a new type error and converts the expected type to a
     * symbolic Lisp type specifier.
     */
    vm.make_type_error = (datum, expected_type) =>
    {
        return new vm.Type_error(datum, vm.to_lisp_type_spec(expected_type));
    };

    /*
     * Signalled when a symbol cannot be found in an environment.
     */
    vm.Unbound_symbol_error = class Lisp_unbound_symbol_error extends vm.Error
    {
        constructor(symbol, env)
        {
            vm.assert_type(symbol, vm.Symbol);
            vm.assert_type(env, vm.Environment);

            const name = symbol.get_string().to_js_string();
            const namespace = symbol.get_namespace();
            super(`Unbound ${namespace}: ${name}`);

            this.lisp_slot_symbol = symbol;
            this.lisp_slot_environment = env;
        }
    };

    /*
     * Signalled when a slot cannot be found in a standard object.
     */
    vm.Unbound_slot_error = class Lisp_unbound_slot_error extends vm.Error
    {
        constructor(obj, slot_name)
        {
            vm.assert_type(obj, vm.Standard_object);
            vm.assert_type(slot_name, vm.Symbol);

            const name = slot_name.get_string().to_js_string();
            super(`Unbound slot: ${name}`);

            this.lisp_slot_object = obj;
            this["lisp_slot_slot-name"] = slot_name;
        }
    };

    /*
     * Signalled when a method cannot be found in a class.
     */
    vm.Unbound_method_error = class Lisp_unbound_method_error extends vm.Error
    {
        constructor(cls, method_name)
        {
            vm.assert_type(cls, vm.Class);
            vm.assert_type(method_name, vm.Symbol);

            const name = method_name.get_string().to_js_string();
            super(`Unbound method: ${name}`);

            this.lisp_slot_class = cls;
            this["lisp_slot_method-name"] = method_name;
        }
    };

    /*
     * Signalled when an assertion failed.
     */
    vm.Assertion_error = class Lisp_assertion_error extends vm.Error
    {
        constructor(message)
        {
            super(message || "Assertion failed");
        }
    };

    /*
     * If the boolean is false, throws an error with a message.
     */
    vm.assert = (bool, message = "Assertion failed") =>
    {
        if (!bool) throw new vm.Assertion_error(message);
    };

    /*
     * Used in the bodies of abstract methods.
     */
    vm.abstract_method = () =>
    {
        throw new vm.Error("You called an abstract method. Congratulations!");
    };

    /*** Type Specs ***/

    /*
     * We use a system of type specifiers to check the types of
     * arguments passed to JS functions.
     *
     * Type specs can also be translated to symbolic Lisp data
     * for error reporting.
     */

    /*
     * Returns true if a datum matches a type spec, false otherwise.
     *
     * A type spec can be a:
     *
     * - String: in this case, the datum's typeof must be equal to the
     *   type spec.
     *
     * - Function: the type spec must be the JS class of a Lisp class
     *   and the datum must be instanceof the type spec.
     *
     * - Object with a custom_check method: the custom_check methods gets
     *   called to determine whether the datum matches.
     */
    vm.has_type = (datum, type_spec) =>
    {
        if (typeof(type_spec) === "string") {
            return typeof(datum) === type_spec;
        } else if (typeof(type_spec) === "function") {
            return datum instanceof type_spec;
        } else if (type_spec && type_spec.custom_check) {
            return type_spec.custom_check(datum);
        } else {
            throw new vm.Error("Unknown type spec");
        }
    };

    /*
     * A type spec that matches any datum.
     */
    vm.TYPE_ANY = {
        custom_check(datum)
        {
            return true;
        },
        to_lisp_type_spec()
        {
            return vm.sym("object");
        }
    };

    /*
     * A type spec that matches null.
     */
    vm.TYPE_NULL = {
        custom_check(datum)
        {
            return datum === null;
        },
        to_lisp_type_spec()
        {
            return vm.str("null");
        }
    };

    /*
     * A type spec that matches any of its constituent type specs.
     */
    vm.type_or = (...elements) =>
    {
        return {
            custom_check(datum)
            {
                for (let i = 0; i < elements.length; i++)
                    if (vm.has_type(datum, elements[i]))
                        return true;
                return false;
            },
            to_lisp_type_spec()
            {
                return vm.cons(vm.sym("or"),
                               vm.array_to_list(elements.map(vm.to_lisp_type_spec)));
            }
        }
    };

    /*
     * Checks that a datum matches a type spec with vm.has_type(),
     * and throws an error if it doesn't.  Returns the datum.
     */
    vm.assert_type = (datum, type_spec) =>
    {
        if (vm.has_type(datum, type_spec)) {
            return datum;
        } else {
            throw vm.make_type_error(datum, type_spec);
        }
    };

    /*
     * Transforms a type spec into a symbolic representation used on
     * the Lisp side.
     *
     * A symbolic Lisp type specifier is one of:
     *
     * - (Lisp) String: in this case the type specifier describes
     *   a JavaScript typeof check.  As a special case, "null" stands
     *   for the JS null value.
     *
     * - Symbol: the type specifier describes a Lisp class by name.
     *
     * - List: compound type specifier.  Currently supported are
     *   OR type specifiers of the form
     *   (or <type-spec-1> ... <type-spec-N>).
     */
    vm.to_lisp_type_spec = (type_spec) =>
    {
        if (typeof(type_spec) === "string") {
            return vm.str(type_spec);
        } else if (typeof(type_spec) === "function") {
            if (vm.has_lisp_class(type_spec))
                return vm.lisp_class(type_spec).get_name();
            else
                /*
                 * Temporary hack: we don't have a symbolic type spec
                 * format for non-VM, ordinary JS classes yet.  For
                 * now, we just use the class itself as its type spec.
                 */
                return type_spec;
        } else if (type_spec && type_spec.to_lisp_type_spec) {
            return type_spec.to_lisp_type_spec();
        } else {
            throw new vm.Error("Unknown type spec");
        }
    };

    /*** Lists ***/

    /*
     * Returns the list element at a given position.
     */
    vm.elt = (list, i) =>
    {
        vm.assert_type(list, vm.Cons);
        vm.assert_type(i, "number");
        if (i === 0)
            return list.car();
        else
            return vm.elt(list.cdr(), i - 1);
    };

    /*
     * Turns a list into a JS array.
     */
    vm.list_to_array = (list) =>
    {
        vm.assert_type(list, vm.List);
        const array = [];
        while (list !== vm.nil()) {
            array.push(list.car());
            list = list.cdr();
        }
        return array;
    };

    /*
     * Turns a JS array into a list.
     */
    vm.array_to_list = (array) =>
    {
        vm.assert(Array.isArray(array), "Not an array");
        let list = vm.nil();
        for (let i = array.length; i > 0; i--)
            list = vm.cons(array[i - 1], list);
        return list;
    };

    /*
     * Reverses a list.
     */
    vm.reverse = (list) =>
    {
        vm.assert_type(list, vm.List);
        let rev_list = vm.nil();
        while (list !== vm.nil()) {
            rev_list = vm.cons(list.car(), rev_list);
            list = list.cdr();
        }
        return rev_list;
    };

    /*** Internal VM Data Structures ***/

    /*
     * The root, or global, environment.
     */
    vm.environment = vm.make_environment();

    /*
     * Symbol table, maps symbol keys to interned symbols.
     */
    vm.symbols = Object.create(null);

    /*
     * The peanut gallery.
     */
    const T = new vm.Boolean(true);
    const F = new vm.Boolean(false);
    const NIL = new vm.Nil();
    const VOID = new vm.Void();
    const IGNORE = new vm.Ignore();
    const ZERO = vm.num(0);
    const ONE = vm.num(1);

    /*
     * Wrap them in functions so typos cause an error, and don't
     * result in undefined.
     */
    vm.t = () => T;
    vm.f = () => F;
    vm.nil = () => NIL;
    vm.void = () => VOID;
    vm.ignore = () => IGNORE;
    vm.zero = () => ZERO;
    vm.one = () => ONE;

    /*** Class Metaobjects ***/

    /*
     * Creates a class metaobject for a JS class and registers the
     * class in the root environment.
     *
     * Note that the class gets registered in the class namespace,
     * while its name property is an ordinary variable symbol.
     * We could have used the class namespace for both, but it's
     * more readable if class names are just ordinary symbols.
     */
    vm.define_class = (name, js_class, js_super = vm.Object, js_meta = vm.Built_in_class) =>
    {
        const name_sym = vm.sym(name);
        const lisp_class = vm.bless_class(name_sym, js_class, js_super, js_meta);
        vm.get_environment().put(name_sym.to_class_symbol(), lisp_class);
    };

    /*
     * Creates a class metaobject for a JS class.
     */
    vm.bless_class = (name_sym, js_class, js_super = vm.Object, js_meta = vm.Built_in_class) =>
    {
        vm.assert_type(name_sym, vm.Symbol);
        vm.assert_type(js_class, "function");
        vm.assert_type(js_super, vm.type_or(vm.TYPE_NULL, "function"));
        vm.assert_type(js_meta, "function");
        const lisp_super = js_super ? vm.lisp_class(js_super) : null;
        const lisp_class = new js_meta(name_sym, js_class, lisp_super);
        js_class.prototype.lisp_class = lisp_class;
        return lisp_class;
    };

    /*
     * Defines a condition class.  Used so that STANDARD-CLASS doesn't
     * have to be specified explicitly as metaclass.
     */
    vm.define_condition = (name, js_class, js_super) =>
    {
        vm.define_class(name, js_class, js_super, vm.Standard_class);
    };

    /*** Lisp API ***/

    vm.define_class("object", vm.Object, null);

    vm.define_class("string", vm.String);

    vm.define_class("symbol", vm.Symbol);

    vm.define_class("number", vm.Number);

    vm.define_class("boolean", vm.Boolean);

    vm.define_class("list", vm.List);

    vm.define_class("cons", vm.Cons, vm.List);

    vm.define_class("nil", vm.Nil, vm.List);

    vm.define_class("void", vm.Void);

    vm.define_class("ignore", vm.Ignore);

    vm.define_class("environment", vm.Environment);

    vm.define_class("class", vm.Class);

    vm.define_class("built-in-class", vm.Built_in_class, vm.Class);

    vm.define_class("standard-class", vm.Standard_class, vm.Class);

    vm.define_class("standard-object", vm.Standard_object, vm.Object, vm.Standard_class);

    vm.define_condition("condition", vm.Condition, vm.Standard_object);

    vm.define_condition("error", vm.Error, vm.Condition);

    vm.define_condition("type-error", vm.Type_error, vm.Error);

    vm.define_condition("unbound-symbol-error", vm.Unbound_symbol_error, vm.Error);

    vm.define_condition("unbound-slot-error", vm.Unbound_slot_error, vm.Error);

    vm.define_condition("unbound-method-error", vm.Unbound_method_error, vm.Error);

    vm.define_condition("assertion-error", vm.Assertion_error, vm.Error);

};
