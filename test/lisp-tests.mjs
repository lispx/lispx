import { make_vm } from "lispx-vm";

import test_util_code from "./test-util.lispx";
import test_util_test_code from "./test-util-test.lispx";
import boot_test_code from "./boot-test.lispx";
import cond_sys_test_code from "./cond-sys-test.lispx";
import control_test_code from "./control-test.lispx";
import delimcc_test_code from "./delimcc-test.lispx";
import stream_test_code from "./stream-test.lispx";
import read_test_code from "./read-test.lispx";
import print_test_code from "./print-test.lispx";
import js_test_code from "./js-test.lispx";
import hierarchy_test_code from "./hierarchy-test.lispx";

describe("Lisp Tests", () => {

    const vm = make_vm();
    vm.eval_js_string(test_util_code);
    vm.eval_js_string(test_util_test_code);
    vm.eval_js_string(boot_test_code);
    vm.eval_js_string(cond_sys_test_code);
    vm.eval_js_string(control_test_code);
    vm.eval_js_string(delimcc_test_code);
    vm.eval_js_string(stream_test_code);
    vm.eval_js_string(read_test_code);
    vm.eval_js_string(print_test_code);
    vm.eval_js_string(js_test_code);
    vm.eval_js_string(hierarchy_test_code);

}).timeout(100000);
