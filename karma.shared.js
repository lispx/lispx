/*
 * Shared configuration utilities for running browser tests with Karma.
 */

/*
 * We use Playwright here just because it provides a WebKit browser.
 */
const playwright = require("playwright");
process.env.WEBKIT_HEADLESS_BIN = playwright.webkit.executablePath();

/*
 * TEST_ALL_BROWSERS is set in the Github CI workflow, causing tests
 * to be run in all browsers.
 * For local testing I don't set it, and run tests just in Chrome.
 */
const browsers =
      ("TEST_ALL_BROWSERS" in process.env)
      ? ["WebkitHeadless", "FirefoxHeadless", "ChromeHeadless"]
      : ["ChromeHeadless"];

module.exports = function(filename, config)
{
    config.set({
        frameworks: ["mocha", "chai"],
        files: [filename, "./dist/test/lispx-test-browser.umd.js"],
        reporters: ["progress"],
        port: 9876,  // karma web server port
        colors: true,
        logLevel: config.LOG_INFO,
        browsers: browsers,
        autoWatch: false,
        singleRun: true, // Karma captures browsers, runs the tests and exits
        concurrency: Infinity
    });
};
