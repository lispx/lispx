/*
 * Webpack utilities shared between development and production build.
 */

const TerserPlugin = require("terser-webpack-plugin");

module.exports = {
    make_entry,
    make_entry_esm,
    make_entry_umd,
    make_test_entry_browser
};

/*
 * Utility to build VM with configurable target type (ESM or UMD) and
 * minimization setting.
 *
 * The input_file references the VM root file.  For building the
 * initial ESM, we use src/vm.mjs.  For building the UMD, we use the
 * built ESM.
 *
 * The entry is not complete, the functions make_entry_umd() and
 * make_entry_esm() fill in some missing fields for each target type.
 */
function make_entry(input_file, output_filename, target_type, minimize)
{
    const lisp_loaders = [ { loader: "raw-loader" } ];
    /*
     * Run our custom minifier that strips comments and docstrings
     * from Lisp code.
     */
    if (minimize)
        lisp_loaders.push({ loader: "./tool/minifier.js" });
    return {
        entry: input_file,
        output: {
            libraryTarget: target_type,
            filename: output_filename,
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
        /*
         * This turns off some debugging stuff otherwise added by
         * Webpack.
         */
        devtool: false,
        optimization: {
            minimize: minimize,
            minimizer: [
                new TerserPlugin({
                    terserOptions: {
                        /*
                         * Class names are needed for some tests.
                         */
                        keep_classnames: true,
                        /*
                         * We probably could get rid of function names,
                         * but it's not a big benefit, size-wise.
                         */
                        keep_fnames: true
                    }
                })
            ]
        }
    }
}

function make_entry_esm(basename, minimize)
{
    const output_filename = basename + (minimize ? ".min.mjs" : ".vm.mjs")
    const entry = make_entry("./src/vm.mjs", output_filename, "module", minimize);
    entry.experiments = {
        outputModule: true
    };
    return entry;
}

function make_entry_umd(basename, minimize)
{
    const output_filename = basename + (minimize ? ".umd.min.js" : ".umd.js");
    const entry = make_entry("./src/vm.mjs", output_filename, "umd", minimize);
    entry.output.library = "lispx-vm";
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
