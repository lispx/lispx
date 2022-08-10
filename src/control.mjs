/*
 * LispX Delimited Control
 * Copyright (c) 2021 Manuel J. Simoni
 */

/*
 * Adds multi-prompt delimited control, delimited dynamic binding, and
 * continuation barriers to a virtual machine.
 *
 * The API follows the delimcc library:
 * `http://okmij.org/ftp/continuations/caml-shift-journal.pdf'
 * and the paper Delimited Dynamic Binding:
 * `http://okmij.org/ftp/papers/DDBinding.pdf'.
 *
 * Also implements continuation-aware first-order effects: sequencing,
 * looping, nonlocal exits, and unwind protection.
 *
 * ---
 *
 * We are implementing delimited continuations in the context of a
 * tree-walking interpreter.  The main idea of the implementation is
 * to run directly on the host language stack most of the time, and
 * only create on-heap Lisp continuations when they are captured.
 *
 * In the regular case, Lisp operators return their result as their
 * normal JS return value.  But when a capture is triggered, a
 * suspension helper object is returned instead.  This suspension
 * contains the desired prompt to which the capture should abort to,
 * as well as a user-supplied Lisp handler function that gets called
 * with the captured continuation once the prompt is reached.  The
 * suspension also contains the continuation frames captured so far.
 *
 * Every built-in Lisp operator must detect when one of its inner
 * sub-expressions returns a suspension instead of a regular result.
 * In this case, the operator must suspend itself also by adding any
 * required continuation frames to the suspension, and passing on the
 * suspension outwards to its caller.  This process ends when the
 * desired prompt is reached, at which point the user-supplied handler
 * is called.
 *
 * To reinstate a continuation, a resumption helper object is created.
 * Analogous to how a suspension gets passed outwards from the point
 * of continuation capture to the prompt, a resumption gets passed
 * inwards from the point where continuation composition is triggered.
 * A resumption contains the continuation frames that must be put back
 * from the heap onto the JS stack, as well as a user-supplied Lisp
 * handler function that is called inside the newly composed context
 * when all frames have been reinstated.
 *
 * Every built-in Lisp operator is written in such a way that it can
 * either be called normally, or during continuation resumption.  If
 * called normally, the operator will do its regular business.  If
 * called during resumption however, it will receive a resumption
 * helper object.  The operator will tell the resumption object to put
 * the remaining on-heap continuation frames stored in it back onto
 * the JS stack before proceeding.  This process ends when the
 * innermost continuation frame that originally triggered continuation
 * capture is reached, at which point the user-supplied handler is
 * called.
 */
export function init_control(vm)
{
    /*** Continuations ***/

    /*
     * A continuation is organized as a stack of continuation frames.
     * The innermost frame corresponds to the %TAKE-SUBCONT
     * expression that triggered continuation capture.  The outermost
     * frame corresponds to the expression that appeared directly
     * within the prompt-pushing expression (%PUSH-PROMPT or
     * %PUSH-DELIM-SUBCONT).  The prompt-pushing expression itself
     * is not included in the continuation.
     *
     * Every continuation frame contains a work function (a JavaScript
     * closure) that knows how to resume that particular frame when
     * the continuation is reinstated.  Every built-in Lisp operator
     * creates different kinds of work functions for the continuation
     * frames it creates.
     */
    vm.Continuation = class Lisp_continuation extends vm.Object
    {
        /*
         * Constructs a continuation frame with the operator-specific
         * work function and an inner continuation frame.
         *
         * The inner continuation frame is null for the innermost
         * frame created by the %TAKE-SUBCONT expression that triggered
         * continuation capture.
         */
        constructor(work_fun, inner, trace)
        {
            super();
            vm.assert_type(work_fun, "function");
            vm.assert_type(inner, vm.type_or(vm.TYPE_NULL, vm.Continuation));
            this.work_fun = work_fun;
            this.inner = inner;
            this.trace = trace;
        }
    };

    /*
     * A trace may be attached to a continuation frame for debugging
     * purposes and showing the stack trace.  Traces are not needed
     * operationally, and not all frames have traces.
     */
    class Trace
    {
        constructor(expr, env)
        {
            this.expr = expr;
            this.env = env;
        }
    }

    /*
     * Construct a trace with the given expression and environment.
     */
    vm.trace = (expr, env) => new Trace(expr, env);

    /*
     * A suspension is a helper object created during the capture
     * (creation) of a continuation.
     *
     * It gets passed outwards from the %TAKE-SUBCONT expression that
     * triggers the capture until a %PUSH-PROMPT or
     * %PUSH-DELIM-SUBCONT with a matching prompt is reached.  Every
     * intervening Lisp expression adds one or more continuation
     * frames to the suspension on the way out.  Once the
     * %PUSH-PROMPT or %PUSH-DELIM-SUBCONT is reached, the
     * suspension's handler gets called with the captured
     * continuation.
     *
     * Suspensions are implementation-level objects, and are never
     * visible to Lisp.
     */
    vm.Suspension = class Suspension
    {
        /*
         * Constructs a new suspension.
         *
         * The continuation will be captured outwards to the prompt.
         *
         * The user-defined suspension handler will get called with
         * the captured continuation.
         */
        constructor(prompt, handler)
        {
            vm.assert_type(prompt, vm.TYPE_ANY);
            vm.assert_type(handler, vm.Function);
            this.prompt = prompt;
            this.handler = handler;
            this.continuation = null;
        }

        /*
         * Destructively adds a new outer continuation frame with the
         * given work function to the suspension as we move outwards
         * during continuation creation.
         */
        suspend(work_fun, trace)
        {
            vm.assert_type(work_fun, "function");
            this.continuation = new vm.Continuation(work_fun, this.continuation, trace);
            return this;
        }
    };

    /*
     * A resumption is a helper object created during the composition
     * (reinstatement) of a continuation.
     *
     * It gets passed inwards until the innermost continuation frame
     * is reached.  At each step, the operator-specific work function
     * of intervening frames is called.  Once the innermost frame is
     * reached, the user-defined resumption handler function gets
     * called and takes over control.
     *
     * Resumptions are implementation-level objects, and are never
     * visible to Lisp.
     */
    vm.Resumption = class Resumption
    {
        /*
         * Creates a new resumption.
         *
         * The continuation will get composed with the current stack,
         * frame by frame, as we move inwards.
         *
         * The user-defined resumption handler will get called after
         * the continuation has been reinstated.
         */
        constructor(continuation, handler)
        {
            vm.assert_type(continuation, vm.Continuation);
            vm.assert_type(handler, vm.Function);
            this.continuation = continuation;
            this.handler = handler;
        }

        /*
         * Destructively removes the outer frame of a continuation
         * from the resumption and calls its work function as we move
         * inwards during continuation reinstatement.
         */
        resume()
        {
            const continuation = this.continuation;
            this.continuation = continuation.inner;
            return continuation.work_fun(this);
        }
    };

    /*** Bind ***/

    /*
     * Sequences a thunk and a function in a continuation-aware
     * manner: the function receives the result of the thunk as its
     * argument.
     *
     * This is used in eval.mjs for all operators whose semantics are
     * straightforward and only require sequential execution.
     *
     * The trace is attached to the continuation frame for debugging.
     */
    vm.bind = (first, second, trace) =>
    {
        vm.assert_type(first, "function");
        vm.assert_type(second, "function");
        return do_bind(first, second, trace);
    };

    /*
     * Work function for bind().
     *
     * Note the resumption parameter.  Do_bind(), like all work
     * functions, is written so that it can be called in two ways:
     *
     * - Directly from bind(), with a null resumption.  In this case it
     *   will evaluate the first thunk.
     *
     * - As a work function of a suspended continuation frame.  This happens
     *   if the first thunk earlier captured a continuation.  In this case,
     *   we resume into the continuation with resume().
     *
     * Due to its simplicity, do_bind() is a good example to learn
     * about the protocol that continuation frames / work functions
     * must support.  The work functions of the more complicated
     * operators, below, follow this same protocol.
     */
    function do_bind(first, second, trace, resumption = null)
    {
        /*
         * Evaluate first thunk.
         */
        let val;
        if (resumption instanceof vm.Resumption)
            /*
             * First thunk previously captured a continuation.  Resume
             * into it.
             */
            val = resumption.resume();
        else
            /*
             * We are evaluating the first thunk for the first time.
             */
            val = first();
        /*
         * Check result of first thunk.
         */
        if (val instanceof vm.Suspension)
            /*
             * The first thunk captured a continuation.
             *
             * We need to suspend now, too.  We do this by pushing a
             * work function closure onto the continuation,
             * that will restart later where we left off.
             */
            return val.suspend((resumption) =>
                do_bind(first, second, trace, resumption),
                trace
            );
        else
            /*
             * The first thunk returned normally.
             * Pass its result to the second function.
             */
            return second(val);
    }

    /*** Delimited Control Operators ***/

    /*
     * (%take-subcont prompt handler) => |
     *
     * Built-in function that initiates continuation capture.  It
     * aborts outwards to the given prompt and calls the suspension
     * handler with the captured continuation.
     */
    function TAKE_SUBCONT(args, env)
    {
        var prompt = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        var handler = vm.assert_type(vm.elt(args, 1), vm.Function);

        /*
         * Create the suspension.
         *
         * This is the first event of continuation capture.
         */
        const suspension = new vm.Suspension(prompt, handler);

        /*
         * Push the innermost continuation frame onto the
         * suspension.
         *
         * Its work function will call the user-defined
         * resumption handler when resumed.
         */
        return suspension.suspend((resumption) =>
            /*
             * This is the final event of continuation composition.
             *
             * The resumption handler passed in from the outside takes
             * over control in the place where the continuation
             * was originally captured.
             */
            vm.operate(resumption.handler, vm.nil(), env)
        );
    }

    /*
     * (%push-prompt prompt thunk) => result
     *
     * Built-in function that pushes a prompt.  A user-supplied thunk
     * is then called inside the newly delimited continuation.
     * Returns the thunk's result.
     */
    function PUSH_PROMPT(args, env)
    {
        const prompt = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const thunk = vm.assert_type(vm.elt(args, 1), vm.Function);
        const action = () => vm.operate(thunk, vm.nil(), env);
        return vm.push_prompt(prompt, action, env);
    }

    /*
     * (%push-delim-subcont prompt continuation thunk) => result
     *
     * Built-in function that pushes a prompt and reinstates a
     * previously captured continuation inside it.  A user-supplied
     * thunk is then called inside the newly composed continuation.
     * Returns the thunk's result.
     *
     * (Note: this operator is basically `push_delim_subcont' from
     * delimcc, except that the prompt must be manually supplied,
     * since our continuations don't include prompts.)
     */
    function PUSH_DELIM_SUBCONT(args, env)
    {
        const prompt = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const continuation = vm.assert_type(vm.elt(args, 1), vm.Continuation);
        const thunk = vm.assert_type(vm.elt(args, 2), vm.Function);
        /*
         * Resume into the user-supplied continuation with the thunk
         * as resumption handler.
         *
         * This is the first event of continuation composition.
         */
        const action = () => new vm.Resumption(continuation, thunk).resume();
        return vm.push_prompt(prompt, action, env);
    }

    /*
     * Work function for PUSH_PROMPT and PUSH_DELIM_SUBCONT
     * whose difference is factored out into the action parameter.
     */
    vm.push_prompt = (prompt, action, env, resumption = null) =>
    {
        /*
         * Do the action.
         */
        let result;
        if (resumption instanceof vm.Resumption)
            result = resumption.resume();
        else
            result = action();

        if (result instanceof vm.Suspension) {
            /*
             * Action captured a continuation.
             */
            if (vm.equal(prompt, result.prompt)) {
                /*
                 * It's looking for our prompt, i.e. the capture ends here.
                 *
                 * This is the final event of continuation capture.
                 *
                 * The user-supplied suspension handler takes over
                 * control where the prompt was originally pushed.
                 * It receives the captured continuation as argument.
                 */
                return vm.operate(result.handler,
                                  vm.list(result.continuation),
                                  env);
            } else {
                /*
                 * It's looking for an outer prompt, we need to
                 * suspend, ourselves.
                 */
                return result.suspend((resumption) =>
                    vm.push_prompt(prompt, action, env, resumption));
            }
        } else {
            /*
             * Action evaluated normally.
             */
            return result;
        }
    };

    /*
     * (%push-subcont-barrier thunk) => result
     *
     * Built-in function that calls a thunk and prevents it from
     * capturing continuations to the outside.
     */
    function PUSH_SUBCONT_BARRIER(args, env)
    {
        const thunk = vm.assert_type(vm.elt(args, 0), vm.Function);
        return vm.push_subcont_barrier(() => vm.operate(thunk, vm.nil(), env), env);
    }

    vm.push_subcont_barrier = (action, env, resumption = null) =>
    {
        /*
         * How can it be that this work function must handle
         * resumption, you ask?  Isn't the whole idea behind a
         * continuation barrier that continuations cannot escape it,
         * and therefore obviously cannot reenter it either?  The
         * answer can be found in the following comments.
         */
        let result;
        if (resumption instanceof vm.Resumption)
            result = resumption.resume();
        else
            result = action();

        if (result instanceof vm.Suspension) {
            /*
             * Thunk attempted to capture.
             *
             * Add ourselves to the continuation.  Note that this
             * built-in is different from all others.  We do not
             * return the suspension back to the caller -- that is
             * after all exactly what we want to prevent.  But we must
             * still suspend ourselves in this way: if we didn't, the
             * barrier would be missing from the continuation after
             * re-composition.
             */
            result.suspend((resumption) =>
                vm.push_subcont_barrier(action, env, resumption));

            /*
             * Resume back into the continuation and throw an error
             * from the inside.  This means the user will be able to
             * see a useful stack trace that shows where the ill-fated
             * continuation capture occurred.
             */
            const handler = vm.alien_function(() => {
                throw new vm.Prompt_not_found_error(result.prompt); });

            return new vm.Resumption(result.continuation, handler).resume();
        } else {
            return result;
        }
    };

    /*
     * Signalled on continuation barrier breach.
     */
    vm.Prompt_not_found_error = class Lisp_prompt_not_found_error extends vm.Error
    {
        constructor(prompt)
        {
            super("Prompt not found: " + vm.write_to_js_string(prompt));
            this.lisp_slot_prompt = prompt;
        }
    };

    /*** Delimited Dynamic Binding ***/

    /*
     * A dynamic variable is a cell holding a value.
     */
    vm.Dynamic = class Lisp_dynamic extends vm.Standard_object
    {
        constructor(value = vm.void())
        {
            super();
            this.lisp_slot_value = value;
        }

        get_value() { return this.lisp_slot_value; }
        set_value(value) { this.lisp_slot_value = value; }
    };

    /*
     * Create a new dynamic variable with the given default value.
     */
    vm.make_dynamic = (value = vm.void()) =>
    {
        return new vm.Dynamic(value);
    };

    /*
     * (%progv dynamics values thunk) => result
     *
     * Built-in function that evaluates a thunk with a list of dynamic
     * variables temporarily bound to new values taken from a second
     * list.
     *
     * Cf. Common Lisp's PROGV.
     */
    function PROGV(args, env)
    {
        const dynamics = vm.list_to_array(vm.elt(args, 0));
        const values = vm.list_to_array(vm.elt(args, 1));
        const thunk = vm.assert_type(vm.elt(args, 2), vm.Function);
        return do_progv(dynamics, values, thunk, env);
    }

    function do_progv(dynamics, values, thunk, env, resumption = null)
    {
        return vm.progv(dynamics, values, () => {
            let result;
            if (resumption instanceof vm.Resumption) {
                result = resumption.resume();
            } else {
                result = vm.operate(thunk, vm.nil(), env);
            }
            if (result instanceof vm.Suspension) {
                return result.suspend((resumption) =>
                    do_progv(dynamics, values, thunk, env, resumption));
            } else {
                return result;
            }
        });
    }

    /*
     * Utility to bind dynamic variables during a JS thunk.
     *
     * This can also be used outside of the %PROGV primitive.
     */
    vm.progv = (dynamics, values, thunk) =>
    {
        vm.assert(dynamics.length === values.length);
        /*
         * Save old values and apply new ones.
         */
        const old_values = [];
        for (let i = 0; i < dynamics.length; i++) {
            const dynamic = vm.assert_type(dynamics[i], vm.Dynamic);
            old_values[i] = dynamic.get_value();
            dynamic.set_value(values[i]);
        }
        try {
            /*
             * Call the thunk.
             */
            return thunk();
        } finally {
            /*
             * Restore old values.
             */
            for (let i = 0; i < dynamics.length; i++) {
                dynamics[i].set_value(old_values[i]);
            }
        }
    };

    /*** Simple Control ***/

    /*
     * (%loop expr) => |
     *
     * Built-in operator that evaluates an expression in a never-ending cycle.
     *
     * Cf. Common Lisp's "simple" LOOP, not the Loop Facility.
     */
    function LOOP(operands, env)
    {
        const expr = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        return do_loop(expr, env);
    }

    function do_loop(expr, env, resumption = null)
    {
        let first = true; // Only resume once.
        while (true) {
            let result;
            if (first && (resumption instanceof vm.Resumption)) {
                first = false;
                result = resumption.resume();
            } else {
                result = vm.eval(expr, env);
            }
            if (result instanceof vm.Suspension) {
                return result.suspend((resumption) =>
                    do_loop(expr, env, resumption));
            } else {
                continue;
            }
        }
    }

    /*
     * (%catch tag thunk) => result
     *
     * Built-in function that calls a thunk in a dynamic context where
     * a catch tag is bound.  Dynamically nested forms may nonlocally
     * exit to the catch tag with %THROW.
     *
     * Cf. Common Lisp's CATCH.
     */
    function CATCH(operands, env)
    {
        const tag = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        const thunk = vm.assert_type(vm.elt(operands, 1), vm.Function);
        return do_catch(tag, thunk, env);
    }

    function do_catch(tag, thunk, env, resumption = null)
    {
        try {
            let result;
            if (resumption instanceof vm.Resumption) {
                result = resumption.resume();
            } else {
                result = vm.operate(thunk, vm.nil(), env);
            }
            if (result instanceof vm.Suspension) {
                return result.suspend((resumption) =>
                    do_catch(tag, thunk, env, resumption));
            } else {
                return result;
            }
        } catch (e) {
            /*
             * Check if the exception we caught is a nonlocal exit
             * with our catch tag.
             *
             * If so, return its value.  Otherwise rethrow it.
             */
            if ((e instanceof vm.Nonlocal_exit) && (e.tag === tag)) {
                return e.value;
            } else {
                throw e;
            }
        }
    }

    /*
     * (%throw tag value) => |
     *
     * Built-in function that nonlocally exits to the dynamically
     * nesting catch tag and passes the value to it.
     *
     * Cf. Common Lisp's THROW.
     */
    function THROW(args, env)
    {
        const tag = vm.assert_type(vm.elt(args, 0), vm.TYPE_ANY);
        const value = vm.assert_type(vm.elt(args, 1), vm.TYPE_ANY);
        throw new vm.Nonlocal_exit(tag, value);
    }

    /*
     * Instances of this class are thrown by %THROW.
     */
    vm.Nonlocal_exit = class Nonlocal_exit
    {
        constructor(tag, value)
        {
            this.tag = tag;
            this.value = value;
        }
    };

    /*
     * (%unwind-protect protected-expr cleanup-expr) => result
     *
     * Built-in operator that evaluates the protected expression and
     * returns its result.
     *
     * Regardless of whether the protected expression returns normally
     * or via an exception (including nonlocal exits and panics), the
     * cleanup expression is evaluated (and its result discarded)
     * after the protected expression.
     *
     * The cleanup expression is not evaluated when the protected
     * expression exits via a continuation capture.
     *
     * Cf. Common Lisp's UNWIND-PROTECT.
     */
    function UNWIND_PROTECT(operands, env)
    {
        const protected_expr = vm.assert_type(vm.elt(operands, 0), vm.TYPE_ANY);
        const cleanup_expr = vm.assert_type(vm.elt(operands, 1), vm.TYPE_ANY);
        return do_unwind_protect_1(protected_expr, cleanup_expr, env);
    }

    /*
     * This must be implemented in two steps, with two work functions.
     *
     * The first one evaluates the protected expression, which may (a)
     * return normally, or (b) exit nonlocally with an exception, or (c)
     * capture a continuation.
     *
     * If it does capture, we'll have to restart at step 1 later.  If
     * it does not capture, we can go to step 2, but have to remember
     * whether step 1 returned successfully or threw an exception.
     *
     * The second work function, step 2, evaluates the cleanup
     * expression and afterwards either returns the result produced by
     * the protected expression, or (re)throws the exception
     * thrown by it.
     */
    function do_unwind_protect_1(protected_expr, cleanup_expr, env, resumption = null)
    {
        try {
            let result;
            if (resumption instanceof vm.Resumption)
                result = resumption.resume();
            else
                result = vm.eval(protected_expr, env);
            if (result instanceof vm.Suspension)
                /*
                 * (c) Protected expression captured - stay at step 1.
                 */
                return result.suspend((resumption) =>
                    do_unwind_protect_1(protected_expr, cleanup_expr, env, resumption));
            else
                /*
                 * (a) Protected expression returned normally - go to step 2,
                 * remembering that step 1 was successful.
                 */
                return do_unwind_protect_2(cleanup_expr, result, true, env);
        } catch (exception) {
            /*
             * (b) Protected expression threw - go to step 2,
             * remembering that step 1 failed.
             */
            return do_unwind_protect_2(cleanup_expr, exception, false, env);
        }
    }

    /*
     * Second step of UNWIND-PROTECT.  We evaluate the cleanup
     * expression, which of course may suspend, itself.
     *
     * Afterwards we either return the result of the protected
     * expression, or rethrow the exception thrown by it.
     */
    function do_unwind_protect_2(cleanup_expr, value, success, env, resumption = null)
    {
        let result;
        if (resumption instanceof vm.Resumption)
            result = resumption.resume();
        else
            result = vm.eval(cleanup_expr, env);
        if (result instanceof vm.Suspension) {
            return result.suspend((resumption) =>
                do_unwind_protect_2(cleanup_expr, value, success, env, resumption));
        } else {
            /*
             * After the cleanup expression has been evaluated:
             *
             * If the protected expression returned normally (a),
             * return its result now.
             *
             * If it threw an exception (b), rethrow the exception
             * now.
             */
            if (success)
                return value;
            else
                throw value;
        }
    }

    /*** Root Prompt and Evaluation Entry Point ***/

    /*
     * This prompt delimits all forms evaluated by the VM.
     * It also delimits the bodies of JS lambdas (see js.lispx).
     *
     * Its purpose is to serve as a delimiter for stack traces.
     */
    const ROOT_PROMPT = vm.sym("root-prompt");

    /*
     * Evaluate a form in an environment.  This is the main entry
     * point for calling Lisp from JS.
     *
     * The environment defaults to the VM's root environment.
     *
     * Pushes a continuation barrier (to prevent continuations from
     * escaping to JS) and the root prompt (to enable taking of stack
     * traces).
     */
    vm.eval_form = (form, env = vm.get_environment()) =>
        vm.push_subcont_barrier(() =>
            vm.push_prompt(ROOT_PROMPT,
                           () => vm.eval(form, env),
                           env),
            env);

    /*** Lisp API ***/

    vm.define_class("continuation", vm.Continuation);

    vm.define_class("dynamic", vm.Dynamic, vm.Standard_object, vm.Standard_class);

    vm.define_condition("prompt-not-found-error", vm.Prompt_not_found_error, vm.Error);

    vm.define_built_in_function("%take-subcont", TAKE_SUBCONT);

    vm.define_built_in_function("%push-prompt", PUSH_PROMPT);

    vm.define_built_in_function("%push-delim-subcont", PUSH_DELIM_SUBCONT);

    vm.define_built_in_function("%push-subcont-barrier", PUSH_SUBCONT_BARRIER);

    vm.define_built_in_function("%progv", PROGV);

    vm.define_built_in_operator("%loop", LOOP);

    vm.define_built_in_function("%catch", CATCH);

    vm.define_built_in_function("%throw", THROW);

    vm.define_built_in_operator("%unwind-protect", UNWIND_PROTECT);

    vm.define_constant("+root-prompt+", ROOT_PROMPT);

    /*
     * This really should go somewhere else and probably use streams
     * but here we are.
     */
    function print_stacktrace(k)
    {
        vm.assert_type(k, vm.Continuation);
        const lines = [];
        do {
            if (k.trace)
                lines.push(vm.write_to_string(k.trace.expr).to_js_string());
        } while((k = k.inner));
        lines.reverse();
        /*
         * Drop_frames is the deterministically determined amount of
         * stack frames to hide from a stack trace - i.e. the code of
         * signal handling and debugger etc.  If the boot.lispx and
         * related code changes, this needs to be updated, too.
         */
        const drop_frames = 28;
        lines.slice(drop_frames).forEach((line) => console.log(line));
    }

    vm.define_alien_function("%print-stacktrace", print_stacktrace);

};
