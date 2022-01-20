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

const TerserPlugin = require("terser-webpack-plugin");

module.exports = [
    /*
     * Build unminimized main outputs.
     */
    make_entry_umd(false),
    make_entry_esm(false),
    /*
     * Build test outputs.
     */
    make_test_entry_browser(),
    make_test_entry_node()
];

/*
 * Utility to build main entry with configurable target type and
 * minimization setting.
 *
 * The entry is not complete, the functions make_entry_umd() and
 * make_entry_esm() fill in some missing fields for each target type.
 */
function make_entry(filename, libraryTarget, minimize)
{
    return {
        entry: "./src/vm.mjs",
        output: {
            libraryTarget: libraryTarget,
            filename: filename,
            globalObject: "this"
        },
        module: {
            rules: [
                {
                    test: /\.lispx$/,
                    type: "asset/source"
                }
            ]
        },
        devtool: false,
        optimization: {
            minimize: minimize,
            minimizer: [
                new TerserPlugin({
                    terserOptions: {
                        keep_classnames: true,
                        keep_fnames: true
                    }
                })
            ]
        }
    }
}

function make_entry_umd(minimize)
{
    const entry = make_entry("lispx-vm.umd.js", "umd", minimize);
    entry.output.library = "lispx-vm";
    return entry;
}

function make_entry_esm(minimize)
{
    const entry = make_entry("lispx-vm.mjs", "module", minimize);
    entry.experiments = {
        outputModule: true
    };
    return entry;
}

/*
 * Utility to build test entry with configurable library name and file
 * name.
 *
 * The entry is not complete, the functions make_test_entry_node() and
 * make_test_entry_browser() fill in some missing fields for each
 * architecture.
 */
function make_test_entry(library, filename)
{
    const entry = {
        entry: "./test/all-tests.mjs",
        output: {
            library: library,
            libraryTarget: "umd",
            filename: filename,
            globalObject: "this"
        },
        module: {
            rules: [
                {
                    test: /\.lispx$/,
                    type: "asset/source"
                }
            ]
        },
        externalsType: "umd",
        optimization: {
            minimize: false
        }
    };
    return entry;
}

function make_test_entry_browser()
{
    const entry = make_test_entry("lispx-test-browser",
                                  "test/lispx-test-browser.umd.js");
    /*
     * For some reason, the externals declaration here has to be
     * different from the one in the Node output.
     */
    entry.externals = ["lispx-vm"];
    return entry;
}

function make_test_entry_node()
{
    const entry = make_test_entry("lispx-test-node",
                                  "test/lispx-test-node.umd.js");
    entry.externals = {
        "lispx-vm": "../lispx-vm.umd.js"
    };
    return entry;
}
