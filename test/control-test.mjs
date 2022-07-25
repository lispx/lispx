import { assert } from "chai";

import { VM } from "lispx-vm";

const vm = new VM();

function lookup_operator(name)
{
    return vm.get_environment().lookup(vm.fsym(name));
}

describe("Continuations", () => {

    it("Test %TAKE-SUBCONT suspension and resumption.", () => {

        const take_subcont = lookup_operator("%take-subcont");
        const prompt = vm.str("p");
        const handler = vm.alien_function(() => {}); // Unused here.

        const susp = vm.operate(take_subcont, vm.list(prompt, handler));

        assert.instanceOf(susp, vm.Suspension);
        assert.equal(susp.prompt, prompt);
        assert.equal(susp.handler, handler);

        const k = susp.continuation;
        assert.instanceOf(k, vm.Continuation);
        assert.isNull(k.inner);

        const resum = new vm.Resumption(k, vm.alien_function(() => vm.num(100)));
        assert(vm.equal(resum.resume(), vm.num(100)));

    });

});

describe("Dynamic Variables", () => {

    it("Can create a dynamic variable with initial value.", () => {

        const dyn = vm.make_dynamic(12);
        assert.equal(12, dyn.get_value());
        dyn.set_value(11);
        assert.equal(11, dyn.get_value());

    });

    it("Can dynamically bind variables.", () => {

        const dyn1 = vm.make_dynamic(1);
        const dyn2 = vm.make_dynamic(2);
        assert.equal(1, dyn1.get_value());
        assert.equal(2, dyn2.get_value());
        assert.equal(30, vm.progv([dyn1, dyn2], [10, 20], () => {
            return dyn1.get_value() + dyn2.get_value();
        }));
        assert.equal(1, dyn1.get_value());
        assert.equal(2, dyn2.get_value());
        assert.throws(() =>
            vm.progv([dyn1], [10],
                     () => { throw new Error("value: " + dyn1.get_value()) }),
            "value: 10");
        assert.equal(1, dyn1.get_value());
        assert.equal(2, dyn2.get_value());

    });

});
