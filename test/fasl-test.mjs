import { assert } from "chai";

import { make_vm } from "lispx-vm";
import { init_fasl } from "../tool/fasl.mjs";

const vm = make_vm();
init_fasl(vm);

describe("FASL", () => {

    it("Sets.", () => {

        vm.doit(vm.get_user_environment(), vm.get_system_environment());

    });

});
