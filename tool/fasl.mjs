export function init_fasl(vm)
{
    vm.Visitor = class Visitor
    {
        should_visit() { return true; }
        visit_constant(c) {};
        visit_number(num) {};
        visit_string(str) {};
        visit_symbol(sym) {};
        visit_cons(cons) {};
        visit_function(fexpr) {};
        visit_fexpr(fexpr) {};
        visit_standard_object(obj) {};
        visit_standard_class(cls) {};
        visit_environment(cls) {};
    };
    vm.visit = (v, obj) =>
    {
        if (v.should_visit(obj)) obj.accept_visitor(v);
    };
    vm.Nil.prototype.accept_visitor = function(v) { v.visit_constant(this); };
    vm.Void.prototype.accept_visitor = function(v) { v.visit_constant(this); };
    vm.Ignore.prototype.accept_visitor = function(v) { v.visit_constant(this); };
    vm.Boolean.prototype.accept_visitor = function(v) { v.visit_constant(this); };
    vm.Number.prototype.accept_visitor = function(v) { v.visit_number(this); };
    vm.String.prototype.accept_visitor = function(v) { v.visit_string(this); };
    vm.Symbol.prototype.accept_visitor = function(v)
    {
        v.visit_symbol(this);
        vm.visit(v, this.get_string());
    };
    vm.Cons.prototype.accept_visitor = function(v)
    {
        v.visit_cons(this);
        vm.visit(v, this.car());
        vm.visit(v, this.cdr());
    };
    vm.Function.prototype.accept_visitor = function(v)
    {
        v.visit_function(this);
        vm.visit(v, this.wrapped_operator);
    };
    vm.Fexpr.prototype.accept_visitor = function(v)
    {
        v.visit_fexpr(this);
        vm.visit(v, this.param_tree);
        vm.visit(v, this.env_param);
        vm.visit(v, this.body_form);
        vm.visit(v, this.def_env);
    };
    vm.Standard_object.prototype.accept_visitor = function(v)
    {
        v.visit_standard_object(this);
        for (const slot_name of this.slot_names()) {
            const slot_value = this.slot_value(slot_name);
            vm.visit(v, slot_name);
            vm.visit(v, slot_value);
        }
    };
    vm.Standard_class.prototype.accept_visitor = function(v)
    {
        v.visit_standard_class(this);
        vm.visit(v, this.name);
        vm.visit(v, this.superclass);
        for (const method_name of this.method_names()) {
            const method = this.find_method(method_name);
            vm.visit(v, method_name);
            vm.visit(v, method);
        }
    };
    vm.Environment.prototype.accept_visitor = function(v)
    {
        v.visit_environment(this);
        for (const name of this.binding_names()) {
            const value = this.lookup(name);
            vm.visit(v, name);
            vm.visit(v, value);
        }
    };

    vm.doit = (target, ext_env) =>
    {
        vm.assert(ext_env.parent === null);
        const ext_map = new Map();
        for (const name of ext_env.binding_names()) {
            ext_map.set(ext_env.lookup(name), name);
        }
        ext_map.set(ext_env, "ext-env");

        const [objects, string_map] = vm.collect(target, ext_map);
        const topo_map = vm.topo_sort(objects, ext_map);
        //topo_map.forEach((v, k) => console.log(v + ":" + vm.write_to_js_string(k)));
        const ctx = { topo_map, ext_map, string_map };
        vm.dummy_init(ctx);
        vm.dummy_fini(ctx);
        console.log("return " + topo_map.get(target));
    };

    vm.collect = (target, ext_map) =>
    {
        const seen_once = new Set();
        const seen_twice = new Set();
        const string_map = new Map();
        class Collect_visitor extends vm.Visitor
        {
            should_visit(obj)
            {
                if (ext_map.has(obj)) {
                    return false;
                } else {
                    if (seen_once.has(obj)) {
                        seen_twice.add(obj);
                        return false;
                    } else {
                        return true;
                    }
                }
            }
            visit_string(x)
            {
                const js_string = x.to_js_string();
                if (!string_map.has(js_string))
                    string_map.set(js_string, string_map.size);
            }
            visit_cons(x) { seen_once.add(x); }
            visit_function(x) { seen_once.add(x); }
            visit_fexpr(x) { seen_once.add(x); }
            visit_standard_object(x) { seen_once.add(x); }
            visit_standard_class(x) { seen_once.add(x); }
            visit_environment(x) { seen_once.add(x); }
        }
        vm.visit(new Collect_visitor(), target);
        seen_twice.add(target); // always add target
        return [seen_twice, string_map];
    };

    vm.topo_sort = (objects, ext_map) =>
    {
        const topo_map = new Map(); // object -> position in map
        function topo_add(obj)
        {
            if (!topo_map.has(obj))
                topo_map.set(obj, topo_map.size);
        }
        function topo(obj)
        {
            if (!ext_map.has(obj)) {
                if (obj instanceof vm.Environment) {
                    if (obj.parent !== null) topo(obj.parent);
                } else if (obj instanceof vm.Standard_object) {
                    topo(vm.class_of(obj));
                } else if (obj instanceof vm.Standard_class) {
                    topo(obj.get_superclass());
                }
                topo_add(obj);
            }
        }
        for (const obj of objects) {
            topo(obj);
        }
        return topo_map;
    };

    vm.emit_call = (name, args = []) =>
    {
        return name + "(" + args.join(",") + ")";
    };

    vm.dummy_ref = (obj, ctx) =>
    {
        if (ctx.topo_map.has(obj)) {
            return vm.emit_call("toporef", [ctx.topo_map.get(obj)]);
        } else if (ctx.ext_map.has(obj)) {
            const name = ctx.ext_map.get(obj);
            if (name === "ext-env")
                return "ext_env";
            else
                return vm.emit_call("extref", [vm.emit(name, ctx)]);
        } else {
            return false;
        }
    };

    // INIT
    vm.Cons.prototype.dummy_init = function(ctx)
    {
        return vm.emit_call("dcons");
    };
    vm.Function.prototype.dummy_init = function(ctx)
    {
        return vm.emit_call("dfun");
    };
    vm.Fexpr.prototype.dummy_init = function(ctx)
    {
        return vm.emit_call("dfex");
    };
    vm.Standard_object.prototype.dummy_init = function(ctx)
    {
        return vm.emit_call("dobj", [vm.dummy_ref(vm.class_of(this), ctx)]);
    };
    vm.Standard_class.prototype.dummy_init = function(ctx)
    {
        return vm.emit_call("dcls", [vm.dummy_ref(this.get_superclass(), ctx)]);
    };
    vm.Environment.prototype.dummy_init = function(ctx)
    {
        const parent_ref = (this.parent !== null)
              ? vm.dummy_ref(this.parent, ctx)
              : "null";
        return vm.emit_call("denv", [parent_ref]);
    };
    // FINI
    vm.Cons.prototype.dummy_fini = function(ctx)
    {
        return vm.emit_call("fcons", [vm.dummy_ref(this, ctx), // xxx
                                      vm.emit(this.car(), ctx),
                                      vm.emit(this.cdr(), ctx)]);
    };
    vm.Function.prototype.dummy_fini = function(ctx)
    {
        return vm.emit_call("ffun", [vm.dummy_ref(this, ctx), vm.emit(this.wrapped_operator, ctx)]);
    };
    vm.Fexpr.prototype.dummy_fini = function(ctx)
    {
        return vm.emit_call("ffex", [vm.dummy_ref(this, ctx), "BODY_FORM..."]);
    };
    vm.Standard_object.prototype.dummy_fini = function(ctx)
    {
        return vm.emit_call("fobj", [vm.dummy_ref(this, ctx), "SLOTS"]);
    };
    vm.Standard_class.prototype.dummy_fini = function(ctx)
    {
        return vm.emit_call("fcls", [vm.dummy_ref(this, ctx), "METHODS"]);
    };
    vm.Environment.prototype.dummy_fini = function(ctx)
    {
        return vm.emit_call("fenv", [vm.dummy_ref(this, ctx), "BINDINGS"]);
    };

    vm.emit = (obj, ctx) =>
    {
        const ref = vm.dummy_ref(obj, ctx);
        if (ref !== false)
            return ref;
        else
            return obj.emit(ctx);
    };

    vm.Object.prototype.emit = function(ctx) { throw vm.write_to_js_string(this); }
    vm.Nil.prototype.emit = function(ctx) { return "N"; }
    vm.Void.prototype.emit = function(ctx) { return "V"; }
    vm.Ignore.prototype.emit = function(ctx) { return "I"; }
    vm.Boolean.prototype.emit = function(ctx) { return (this === vm.t()) ? "T" : "F" }
    vm.Symbol.prototype.emit = function(ctx)
    {
        return vm.emit_call(this.namespace, [JSON.stringify(this.get_string().to_js_string())]);
    };
    vm.Cons.prototype.emit = function(ctx)
    {
        return vm.emit_call("C", [vm.emit(this.car(), ctx), vm.emit(this.cdr(), ctx)]);
    };
    vm.Fexpr.prototype.emit = function(ctx)
    {
        return vm.emit_call("FEX");
    };

    vm.dummy_init = (ctx) =>
    {
        ctx.topo_map.forEach((pos, obj) => {
            console.log(obj.dummy_init(ctx));
        });
    };

    vm.dummy_fini = (ctx) =>
    {
        ctx.topo_map.forEach((pos, obj) => {
            console.log(obj.dummy_fini(ctx));
        });
    };

    const PREAMBLE = `
function(vm, ext_env)
{
    const N = vm.nil();
    const objects = [];
    function push(expr) { objects[objects.length] = expr; }
    const dcons = () => push(vm.cons(N,N));
`;

    const POSTAMBLE = `

`;
}
