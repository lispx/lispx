/*
 * LispX Virtual Machine (development mode)
 * Copyright (c) 2024 Manuel J. Simoni
 */

/*
 * Development-mode VM.
 *
 * This is designed for building quickly, and simply evaluates
 * the bootstrap Lisp code directly.
 */

/*
 * Import VM core.
 */
import { VM } from "./vm.mjs";

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
 * Main entrypoint to create a development-mode VM.
 */
export function make_vm()
{
    const vm = new VM();
    /*
     * Evaluate the bootstrap code.
     */
    vm.eval_js_string(boot_code);
    vm.eval_js_string(cond_sys_code);
    vm.eval_js_string(stream_code);
    vm.eval_js_string(read_code);
    vm.eval_js_string(print_code);
    vm.eval_js_string(js_code);
    return vm;
};
