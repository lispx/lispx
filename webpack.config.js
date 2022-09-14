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
     * Build main outputs.
     */
    make_entry_umd(true),
    make_entry_esm(true),
    make_entry_umd(false),
    make_entry_esm(false),
    /*
     * Build test outputs.
     */
    make_test_entry_browser(),
    make_test_entry_node(),
    /*
     * Build REPLs.
     */
    make_web_repl_entry(),
    make_node_repl_entry()
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
    const lisp_loaders = [ { loader: "raw-loader" } ];
    /*
     * Run our custom minifier.
     */
    if (minimize)
        lisp_loaders.push({ loader: "./tool/minifier.js" });
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
                    use: lisp_loaders
                }
            ]
        },
        devtool: false,
        optimization: {
            minimize: false,
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
    const name = minimize ? "lispx-vm.umd.min.js" : "lispx-vm.umd.js";
    const entry = make_entry(name, "umd", minimize);
    entry.output.library = "lispx-vm";
    return entry;
}

function make_entry_esm(minimize)
{
    const name = minimize ? "lispx-vm.min.mjs" : "lispx-vm.mjs";
    const entry = make_entry(name, "module", minimize);
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
                    use: [ { loader: "raw-loader" } ]
                }
            ]
        },
        externalsType: "umd",
        /*
         * https://stackoverflow.com/questions/64402821/module-not-found-error-cant-resolve-util-in-webpack
         */
        resolve: {
            fallback: {
                util: require.resolve("util/")
            }
        },
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
        "lispx-vm": "../lispx-vm.umd.min.js"
    };
    return entry;
}

/*
 * Web REPL.
 */
function make_web_repl_entry()
{
    return {
        entry: "./tool/repl/web/repl.mjs",
        output: {
            library: "lispx-repl",
            libraryTarget: "umd",
            filename: "repl/web/repl.umd.js",
            globalObject: "this"
        },
        module: {
            rules: [
                {
                    test: /\.lispx$/,
                    use: [ { loader: "raw-loader" }, { loader: "./tool/minifier.js" } ]
                }
            ]
        },
        externalsType: "umd",
        resolve: {
            fallback: {
                util: require.resolve("util/")
            }
        },
        optimization: {
            minimize: true,
            minimizer: [ new TerserPlugin() ]
        }
    };
}

/*
 * Node REPL.
 */
function make_node_repl_entry()
{
    return {
        entry: "./tool/repl/node/repl.mjs",
        target: "node",
        output: {
            filename: "repl/node/repl.umd.js",
        },
        module: {
            rules: [
                {
                    test: /\.lispx$/,
                    use: [ { loader: "raw-loader" }, { loader: "./tool/minifier.js" } ]
                }
            ]
        },
        optimization: {
            minimize: true,
            minimizer: [ new TerserPlugin() ]
        }
    };
}
