/*
 * Development-mode configuration for building JS files in dist/ with
 * Webpack.
 *
 * This builds the VM and the browser test as UMDs.
 */

const { make_entry_umd, make_test_entry_browser }
      = require("./webpack.shared.js");

module.exports = [
    /*
     * Build unminimized VM.
     */
    make_entry_umd("lispx-vm-dev", false),
    /*
     * Build browser tests.
     */
    make_test_entry_browser()
];
