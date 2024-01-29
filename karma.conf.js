/*
 * Configuration for running browser tests with Karma.
 */

const playwright = require("playwright");
process.env.WEBKIT_HEADLESS_BIN = playwright.webkit.executablePath();

module.exports = function(config)
{
    config.set({
        frameworks: ["mocha", "chai"],
        files: ["./dist/lispx-vm.umd.min.js", "./dist/test/lispx-test-browser.umd.js"],
        reporters: ["progress"],
        port: 9876,  // karma web server port
        colors: true,
        logLevel: config.LOG_INFO,
        browsers: ["WebkitHeadless"],
        autoWatch: false,
        singleRun: true, // Karma captures browsers, runs the tests and exits
        concurrency: Infinity
    });
};
