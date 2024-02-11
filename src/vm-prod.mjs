/*
 * LispX Virtual Machine (production mode)
 * Copyright (c) 2024 Manuel J. Simoni
 */

/*
 * Production-mode VM.
 *
 * This is designed for booting quickly, and loads the bootstrap code
 * from a saved fasl image (../tool/fasl.mjs).
 */

/*
 * Import VM core.
 */
import { VM } from "./vm.mjs";
import image from "../out.image.mjs";

/*
 * Main entrypoint to create a production-mode VM.
 */
export function make_vm()
{
    const vm = new VM();
    vm.user_environment = image(vm);
    return vm;
};
