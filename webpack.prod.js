/*
 * Configuration for building JS files in dist/ with Webpack.
 *
 * We build the main interpreter as a UMD as well as an ESM output.
 *
 * For testing we produce two UMD outputs, one for Node and one for
 * the browser.  These depend on and test the main interpreter UMD.
 *
 * The ESM interpreter is currently untested.
 */

const { make_entry_esm,
        make_entry_umd,
        make_test_entry_browser,
        make_test_entry_node,
        make_repl_entry_browser,
        make_repl_entry_node }
      = require("./webpack.shared.js");

module.exports = [
    /*
     * Build minimized outputs.
     */
    make_entry_esm("./src/vm-prod.mjs", "lispx-vm", true),
    make_entry_umd("./src/vm-prod.mjs", "lispx-vm", true),
    /*
     * Build test outputs.
     */
    make_test_entry_browser(),
    make_test_entry_node(),
    /*
     * Build REPLs.
     */
    make_repl_entry_browser(),
    make_repl_entry_node()
];
