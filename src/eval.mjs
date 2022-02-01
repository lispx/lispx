/*
 * LispX Evaluation
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds evaluation-related functions and classes to a virtual machine.
 */
export function init_eval(vm)
{
    /*** Evaluation & Operation Core ***/

    /*
     * Evaluate a form in an environment.
     *
     * If the form is neither a symbol nor a cons, it evaluates to
     * itself.
     *
     * The environment defaults to the VM's root environment.
     *
     * Prefer vm.eval_form() unless you are prepared to handle
     * suspensions.
     */
    vm.eval = (form, env = vm.get_environment()) =>
    {
        return vm.trap_exceptions(() => {
            vm.assert_type(env, vm.Environment);
            if (form instanceof vm.Symbol)
                return evaluate_symbol(form, env);
            else if (form instanceof vm.Cons)
                return evaluate_cons(form, env);
            else
                return form;
        });
    };

    /*
     * Evaluate a form, like vm.eval(), but throw an error if the
     * code captures a continuation.  This should usually be used
     * when calling Lisp from JavaScript.
     */
    vm.eval_form = (form, env = vm.get_environment()) =>
    {
        const result = vm.eval(form, env);
        if (result instanceof vm.Suspension)
            throw new vm.Prompt_not_found_error(result.prompt);
        else
            return result;
    };

    /*
     * Symbols evaluate to themselves if they are keywords, or to the
     * value they are bound to in the environment otherwise.
     */
    function evaluate_symbol(sym, env)
    {
        if (sym.get_namespace() === vm.KEYWORD_NAMESPACE)
            return sym;
        else
            return env.lookup(sym);
    }

    /*
     * Conses evaluate by first evaluating their car, which should
     * result in an operator.
     *
     * Then they tell the operator to operate on their cdr.
     */
    function evaluate_cons(cons, env)
    {
        // (See control.mjs for the definition of vm.bind().)
        return vm.bind(() => evaluate_operator(cons.car(), env),
                       (operator) => vm.operate(operator, cons.cdr(), env));
    }

    /*
     * Evaluate the operator position (car) of a cons.
     *
     * If it's a symbol, look it up in the function namespace.
     * This is the fundamental rule of Lisp-2 goodness.
     *
     * But if it's not a symbol, just evaluate it normally.
     * This gives us the same convenience as a Lisp-1.
     */
    function evaluate_operator(operator_form, env)
    {
        if (operator_form instanceof vm.Symbol)
            return env.lookup(operator_form.to_function_symbol());
        else
            return vm.eval(operator_form, env);
    }

    /*
     * Cause an operator to operate on an operand in the given
     * environment.
     *
     * The environment defaults to the VM's root environment.
     */
    vm.operate = (operator, operand, env = vm.get_environment()) =>
    {
        return vm.trap_exceptions(() => {
            vm.assert_type(operator, vm.Operator);
            vm.assert_type(operand, vm.TYPE_ANY);
            vm.assert_type(env, vm.Environment);
            return operator.operate(operand, env);
        });
    };

    /*** Definiends ***/

    /*
     * A definiend is what appears on the left-hand side of a
     * definition (DEF) or as a parameter in a fexpr or function.
     *
     * A plain definiend may either be a symbol or #IGNORE.
     *
     * A definiend tree allows arbitrary nesting of definiends.
     */
    const DEFINIEND = vm.type_or(vm.Symbol, vm.Ignore);
    const DEFINIEND_TREE = vm.type_or(vm.Symbol, vm.Ignore, vm.List);

    /*
     * Matches a definiend ("left-hand side") against a value
     * ("right-hand side") and places resulting bindings into the given
     * environment.  This is used everywhere names are bound to values,
     * such as in DEF, LET, fexpr and function parameters, etc.
     *
     * A non-keyword symbol as a definiend creates a new binding for
     * the whole value.
     *
     * A keyword symbol as a definiend requires the value to be equal
     * to itself, and does not produce a binding.  This yields a simple
     * mechanism for passing keyword arguments to functions.
     *
     * A cons definiend's car and cdr are treated as nested definiends
     * and recursively matched against the value's car and cdr, which
     * must be a cons, too.
     *
     * #NIL as a definiend requires the value to be #NIL, too.
     *
     * #IGNORE as a definiend simply ignores the value.
     *
     * Using any other object as a definiend signals an error.
     *
     * Returns the value.
     */
    vm.match = (definiend, value, env) =>
    {
        if (definiend instanceof vm.Symbol) {
            if (definiend.get_namespace() === vm.KEYWORD_NAMESPACE) {
                if (definiend !== value) {
                    throw new vm.Match_error(definiend, value);
                }
            } else {
                env.put(definiend, value);
            }
        } else if (definiend instanceof vm.Cons) {
            if (value instanceof vm.Cons) {
                vm.match(definiend.car(), value.car(), env);
                vm.match(definiend.cdr(), value.cdr(), env);
            } else {
                throw new vm.Match_error(definiend, value);
            }
        } else if (definiend === vm.nil()) {
            if (value !== vm.nil()) {
                throw new vm.Match_error(definiend, value);
            }
        } else if (definiend === vm.ignore()) {
            // Ignore.
        } else {
            throw vm.make_type_error(definiend, DEFINIEND_TREE);
        }
        return value;
    };

    /*
     * This error is signalled when a definiend cannot be matched
     * against a value.
     */
    vm.Match_error = class Lisp_match_error extends vm.Error
    {
        constructor(definiend, value)
        {
            super("Match error");
            this.lisp_slot_definiend = definiend;
            this.lisp_slot_value = value;
        }
    };

    /*** Operators ***/

    /*
     * Superclass of everything that computes.
     *
     * An operator receives an operand and should do something with it
     * in some environment.  Usually, the operand will be a list, but
     * this is not strictly required by the semantics.
     *
     * There are three types of operators:
     *
     * 1) Built-in operators that are written in JavaScript.
     *
     * 2) Fexprs that are written in Lisp.
     *
     * 3) Functions that wrap around an underlying operator.
     */
    vm.Operator = class Lisp_operator extends vm.Object
    {
        /*
         * Compute on the operand in the environment and return a result.
         */
        operate(operand, env) { vm.abstract_method(); }

        /*
         * Subclasses should override this with an informative result.
         */
        get_name() { return vm.sym("anonymous operator"); }
    };

    /*
     * A fexpr is an operator that does not evaluate its operand by
     * default.
     */
    vm.Fexpr = class Lisp_fexpr extends vm.Operator
    {
        /*
         * Constructs a new fexpr with the given parameter tree,
         * environment parameter, body form, and definition environment.
         *
         * The parameter tree is used to destructure the operand.
         *
         * The environment parameter is bound to the lexical environment
         * in which the fexpr is called (aka the "dynamic environment").
         *
         * The body form is the expression that gets evaluated.
         *
         * The definition environment remembers the lexical environment
         * in which the fexpr was created (aka the "static environment").
         * The body form gets evaluated in a child environment of the
         * definition environment.
         */
        constructor(param_tree, env_param, body_form, def_env)
        {
            super();
            this.param_tree = vm.assert_type(param_tree, DEFINIEND_TREE);
            this.env_param = vm.assert_type(env_param, DEFINIEND);
            this.body_form = vm.assert_type(body_form, vm.TYPE_ANY);
            this.def_env = vm.assert_type(def_env, vm.Environment);;
        }

        /*
         * The body form of a fexpr gets evaluated in a fresh child
         * environment of the static environment in which the fexpr
         * was defined (this gives the usual lexical scope semantics
         * where the form can access bindings from outer
         * environments).
         *
         * The operand gets matched against the fexpr's parameter
         * tree.  The dynamic environment in which the fexpr is called
         * gets matched against the fexpr's environment parameter.
         * Resulting bindings are placed into the child environment.
         */
        operate(operand, dyn_env)
        {
            const child_env = vm.make_environment(this.def_env);
            vm.match(this.param_tree, operand, child_env);
            vm.match(this.env_param, dyn_env, child_env);
            return vm.eval(this.body_form, child_env);
        }
    };

    /*
     * A function is an operator what wraps around another operator
     * (typically a built-in-operator or fexpr, but another function
     * is also possible), and evaluates the operands before passing
     * them on to the wrapped operator.
     *
     * The evaluated operands of a function are called arguments.
     *
     * Note that a function's operand must be a list, while this
     * is not required for operators in general.
     */
    vm.Function = class Lisp_function extends vm.Operator
    {
        /*
         * Constructs a new function with the given underlying operator.
         */
        constructor(operator)
        {
            super();
            vm.assert_type(operator, vm.Operator);
            this.wrapped_operator = operator;
        }

        /*
         * Evaluate the operands to yield a list of arguments, and
         * then call the underlying, wrapped operator with the
         * arguments.
         */
        operate(operands, env)
        {
            return vm.bind(() => eval_args(operands, vm.nil()),
                           (args) => vm.operate(this.wrapped_operator, args, env));

            function eval_args(todo, done)
            {
                if (todo === vm.nil())
                    return vm.reverse(done);
                else
                    return vm.bind(() => vm.eval(todo.car(), env),
                                   (arg) => eval_args(todo.cdr(), vm.cons(arg, done)));
            }
        }

        /*
         * Returns the wrapped operator underlying the function.
         */
        unwrap()
        {
            return this.wrapped_operator;
        }

        /*
         * Returns the name of the function, which is the name
         * of the underlying operator.
         */
        get_name()
        {
            return this.unwrap().get_name();
        }
    };

    /*
     * Wraps a function around an operator.
     */
    vm.wrap = (operator) => new vm.Function(operator);

    /*
     * Built-in operators are operators that are implemented in JS.
     */
    vm.Built_in_operator = class Lisp_built_in_operator extends vm.Operator
    {
        constructor(operate_function, name = "anonymous built-in operator")
        {
            super();
            vm.assert_type(operate_function, "function");
            vm.assert_type(name, "string");
            this.operate_function = operate_function;
            this.name = vm.sym(name);
        }

        operate(operands, env)
        {
            return this.operate_function(operands, env);
        }

        get_name() { return this.name; }
    };

    /*
     * Creates a new built-in operator with the given underlying
     * JS function and name.
     */
    vm.built_in_operator = (fun, name) => new vm.Built_in_operator(fun, name);

    /*
     * Creates a new built-in function, that is, a wrapped built-in operator.
     */
    vm.built_in_function = (fun, name) => vm.wrap(vm.built_in_operator(fun, name));

    /*
     * Creates a new alien operator, a kind of built-in operator.
     *
     * Despite their name, alien operators are just a slight variation
     * on built-in operators.
     *
     * Their underlying JS function does not, as in the case with
     * normal operators, receive the operand and environment as its
     * two JS arguments, but rather receives the whole list of
     * operands as its JS arguments.  It does not receive the
     * environment as an argument.
     *
     * ---
     *
     * Why aren't alien operators just called JS operators?  Well, we
     * also need alien functions (see below), which would need to be
     * called JS functions for consistency.  But that would obviously
     * be highly confusing.
     */
    vm.alien_operator = (fun, name) =>
    {
        vm.assert_type(fun, "function");
        function operate_function(operands, ignored_env)
        {
            return fun.apply(null, vm.list_to_array(operands));
        }
        return vm.built_in_operator(operate_function, name);
    };

    /*
     * Creates a new alien function, that is, a wrapped alien operator.
     *
     * Alien functions are the main mechanism for exposing JS
     * functions to Lisp.
     */
    vm.alien_function = (fun, name) => vm.wrap(vm.alien_operator(fun, name));

    /*** The Built-In Operators ***/

    /*
     * (%%vau param-tree env-param body-form) => fexpr
     *
     * Built-in operator that creates a new fexpr with the given
     * parameter tree, environment parameter, and body form.
     *
     * The dynamic environment of the call to %%VAU becomes
     * the static environment of the created fexpr.
     */
    vm.VAU = (operands, dyn_env) =>
    {
        const param_tree = vm.elt(operands, 0);
        const env_param = vm.elt(operands, 1);
        const body_form = vm.elt(operands, 2);
        const def_env = dyn_env;
        return new vm.Fexpr(param_tree, env_param, body_form, def_env);
    };

    /*
     * (%%def definiend expression) => result
     *
     * Built-in operator that evaluates the expression and matches the
     * definiend against the result value.  Bindings are placed into
     * the dynamic environment in which %%DEF is called.
     *
     * Returns the value.
     */
    vm.DEF = (operands, env) =>
    {
        const definiend = vm.elt(operands, 0);
        const expression = vm.elt(operands, 1);

        return vm.bind(() => vm.eval(expression, env),
                       (result) => vm.match(definiend, result, env));
    };

    /*
     * (%%progn . forms) => result
     *
     * Built-in operator that evaluates the forms from left to right
     * and returns the result of the last one.
     *
     * Returns #VOID if there are no forms.
     */
    vm.PROGN = (forms, env) =>
    {
        if (forms === vm.nil())
            return vm.void();
        else
            return progn(forms);

        function progn(forms)
        {
            return vm.bind(() => vm.eval(forms.car(), env),
                           (result) => {
                               if (forms.cdr() === vm.nil())
                                   return result;
                               else
                                   return progn(forms.cdr());
                           });
        }
    };

    /*
     * (%%if test consequent alternative) => result
     *
     * First, evaluates the test expression which must yield a
     * boolean.
     *
     * Then, evaluates either the consequent or alternative expression
     * depending on the result of the test expression, and returns its
     * result.
     */
    vm.IF = (operands, env) =>
    {
        const test = vm.elt(operands, 0);
        const consequent = vm.elt(operands, 1);
        const alternative = vm.elt(operands, 2);

        return vm.bind(() => vm.eval(test, env),
                       (result) => {
                           vm.assert_type(result, vm.Boolean);
                           if (result == vm.t())
                               return vm.eval(consequent, env);
                           else
                               return vm.eval(alternative, env);
                       });
    };

    /*** Generic Functions ***/

    /*
     * (%%invoke-method method-name method-args) => result
     *
     * Built-in function used in the implementation of generic
     * functions that invokes a method.
     *
     * The first element of method-args must be the receiver object.
     */
    vm.INVOKE_METHOD = (args, env) =>
    {
        const method_name = vm.assert_type(vm.elt(args, 0), vm.Symbol);
        const method_args = vm.assert_type(vm.elt(args, 1), vm.Cons);
        const receiver = method_args.car();
        const receiver_class = vm.class_of(receiver);
        const method = receiver_class.lookup_method(receiver, method_name);
        /*
         * The dynamic environment isn't really needed here, this could also
         * use a fresh, empty environment.
         */
        return vm.operate(method, method_args, env);
    };

    /*** Exception Trapping and Panicking ***/

    /*
     * All exceptions - except nonlocal exits (see control.mjs) and
     * panics (see below) - that happen during evaluation are caught
     * by this function.
     *
     * If the user defined error handler, ERROR, is defined in the
     * VM's root environment, it is called with the exception as
     * argument.  This unifies the JS exception system and the Lisp
     * condition system.
     *
     * If the user defined error handler is not defined (as is the
     * case during boot), the exception gets wrapped in a panic and
     * rethrown.
     */
    vm.trap_exceptions = (thunk) =>
    {
        try {
            return thunk();
        } catch (e) {
            if ((e instanceof vm.Nonlocal_exit) || (e instanceof vm.Panic))
                throw e;
            else
                return vm.call_user_error_handler(e);
        }
    };

    /*
     * Calls the user error handler function, ERROR, defined in Lisp
     * with an exception, or panics with the exception if it is not
     * defined.
     */
    vm.call_user_error_handler = (exception) =>
    {
        const env = vm.get_environment();
        const sym = vm.fsym("error");
        if (env.is_bound(sym)) {
            return vm.operate(env.lookup(sym), vm.list(exception), vm.make_environment());
        } else {
            vm.panic(exception);
        }
    };

    /*
     * A panic is an error that is not caught by the usual exception
     * trapping mechanism.
     */
    vm.Panic = class Panic extends Error
    {
        constructor(cause)
        {
            if (cause && cause.message)
                super("LISP panic: " + cause.message);
            else
                super("LISP panic!");
            this.cause = cause;
        }
    };

    /*
     * Throws a new panic with the given exception as cause.
     */
    vm.panic = (exception) =>
    {
        throw new vm.Panic(exception);
    };

    /*** Definition Utilities ***/

    /*
     * Registers a global variable in the VM's root environment.
     */
    vm.define_variable = (name, object) =>
    {
        vm.get_environment().put(vm.sym(name), object);
    };

    /*
     * Registers a constant in the VM's root environment.
     */
    vm.define_constant = (name, object) =>
    {
        vm.define_variable(name, object);
    };

    /*
     * Registers an operator in the VM's root environment.
     */
    vm.define_operator = (operator) =>
    {
        vm.get_environment().put(operator.get_name().to_function_symbol(),
                                 operator);
    };

    /*
     * Shorthand for registering a built-in operator in the VM's root environment.
     */
    vm.define_built_in_operator = (name, fun) =>
    {
        vm.define_operator(vm.built_in_operator(fun, name));
    };

    /*
     * Shorthand for registering a built-in function in the VM's root environment.
     */
    vm.define_built_in_function = (name, js_fun) =>
    {
        vm.define_operator(vm.built_in_function(js_fun, name));
    };

    /*
     * Shorthand for registering an alien function in the VM's root environment.
     */
    vm.define_alien_function = (name, js_fun) =>
    {
        vm.define_operator(vm.alien_function(js_fun, name));
    };

    /*** Lisp API ***/

    vm.define_class("operator", vm.Operator);

    vm.define_class("built-in-operator", vm.Built_in_operator, vm.Operator);

    vm.define_class("fexpr", vm.Fexpr, vm.Operator);

    vm.define_class("function", vm.Function, vm.Operator);

    vm.define_condition("match-error", vm.Match_error, vm.Error);

    vm.define_built_in_operator("%%vau", vm.VAU);

    vm.define_built_in_operator("%%def", vm.DEF);

    vm.define_built_in_operator("%%progn", vm.PROGN);

    vm.define_built_in_operator("%%if", vm.IF);

    vm.define_alien_function("%%wrap", (operator) => vm.wrap(operator));

    vm.define_alien_function("%%unwrap", (fun) => vm.assert_type(fun, vm.Function).unwrap());

    vm.define_alien_function("%%eval", (expr, env) => vm.eval(expr, env));

    vm.define_alien_function("%%eq", (a, b) => vm.to_lisp_boolean(a === b));

    vm.define_alien_function("%%=", (a, b) => vm.to_lisp_boolean(vm.equal(a, b)));

    vm.define_alien_function("%%<", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) < 0));

    vm.define_alien_function("%%>", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) > 0));

    vm.define_alien_function("%%<=", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) <= 0));

    vm.define_alien_function("%%>=", (a, b) => vm.to_lisp_boolean(vm.compare(a, b) >= 0));

    vm.define_alien_function("%%+", (a, b) => vm.add(a, b));

    vm.define_alien_function("%%-", (a, b) => vm.subtract(a, b));

    vm.define_alien_function("%%*", (a, b) => vm.multiply(a, b));

    vm.define_alien_function("%%/", (a, b) => vm.divide(a, b));

    vm.define_alien_function("%%cons", (car, cdr) => vm.cons(car, cdr));

    vm.define_alien_function("%%car", (cons) => vm.assert_type(cons, vm.Cons).car());

    vm.define_alien_function("%%cdr", (cons) => vm.assert_type(cons, vm.Cons).cdr());

    vm.define_alien_function("%%reverse", (list) => vm.reverse(list));

    vm.define_alien_function("%%intern", (string) => vm.intern(string));

    vm.define_alien_function("%%class-of", (obj) => vm.class_of(obj));

    vm.define_alien_function("%%typep", (obj, cls) =>
        vm.to_lisp_boolean(vm.is_subclass(vm.class_of(obj), cls)));

    vm.define_alien_function("%%make-instance", (cls, ...slot_inits) =>
        vm.make_instance(cls, ...slot_inits));

    vm.define_alien_function("%%slot-value", (obj, slot_name) =>
        vm.assert_type(obj, vm.Standard_object).slot_value(slot_name));

    vm.define_alien_function("%%set-slot-value", (obj, slot_name, slot_value) =>
        vm.assert_type(obj, vm.Standard_object).set_slot_value(slot_name, slot_value));

    vm.define_alien_function("%%slot-bound-p", (obj, slot_name) =>
        vm.to_lisp_boolean(vm.assert_type(obj, vm.Standard_object).is_slot_bound(slot_name)));

    vm.define_alien_function("%%put-method", (cls, name, method) =>
        vm.assert_type(cls, vm.Class).put_method(name, method));

    vm.define_built_in_function("%%invoke-method", vm.INVOKE_METHOD);

    vm.define_alien_function("%%make-standard-class", (name, super_class) =>
        vm.make_standard_class(name, super_class));

    vm.define_alien_function("%%class-name", (cls) =>
        vm.assert_type(cls, vm.Class).get_name());

    vm.define_alien_function("%%subclassp", (sub_cls, super_cls) =>
        vm.to_lisp_boolean(vm.is_subclass(sub_cls, super_cls)));

    vm.define_alien_function("%%make-environment", (parent = null) =>
        vm.make_environment(parent));

    vm.define_alien_function("%%boundp", (sym, env) =>
        vm.to_lisp_boolean(vm.assert_type(env, vm.Environment).is_bound(sym)));

    vm.define_alien_function("%%symbol-name", (sym) =>
        vm.assert_type(sym, vm.Symbol).get_string());

    vm.define_alien_function("%%variable-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_variable_symbol());

    vm.define_alien_function("%%function-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_function_symbol());

    vm.define_alien_function("%%class-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_class_symbol());

    vm.define_alien_function("%%keyword-symbol", (sym) =>
        vm.assert_type(sym, vm.Symbol).to_keyword_symbol());

    vm.define_alien_function("%%panic", (exception) => vm.panic(exception));

};
