import { VM } from "../dist/lispx-vm.mjs";
const vm = new VM();

while (true) {
    const input = prompt("*");
    if (input) console.log(vm.write_to_js_string(vm.eval_js_string(input)));
}
