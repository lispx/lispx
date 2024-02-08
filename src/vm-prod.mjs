/*
 * LispX Virtual Machine (production mode)
 * Copyright (c) 2024 Manuel J. Simoni
 */

/*
 * Production-mode VM.
 *
 * This is designed for booting quickly, and loads the bootstrap code
 * from a saved image.
 */

/*
 * Import VM core.
 */
import { make_vm as make_vm_dev } from "./vm-dev.mjs";

/*
 * Main entrypoint to create a production-mode VM.
 */
export function make_vm()
{
    return make_vm_dev();
};
