/*
 * Configuration for running browser tests with Karma.
 */
module.exports = function(config)
{
    config.set({
        frameworks: ["mocha", "chai"],
        files: ["./dist/lispx-vm.umd.js", "./dist/test/lispx-test-browser.js"],
        reporters: ["progress"],
        port: 9876,  // karma web server port
        colors: true,
        logLevel: config.LOG_INFO,
        browsers: ["ChromeHeadless"],
        autoWatch: false,
        singleRun: true, // Karma captures browsers, runs the tests and exits
        concurrency: Infinity
    });
};
