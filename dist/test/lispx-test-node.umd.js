(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory(require("../lispx-vm.umd.min.js"));
	else if(typeof define === 'function' && define.amd)
		define(["../lispx-vm.umd.min.js"], factory);
	else if(typeof exports === 'object')
		exports["lispx-test-node"] = factory(require("../lispx-vm.umd.min.js"));
	else
		root["lispx-test-node"] = factory(root["../lispx-vm.umd.min.js"]);
})(this, (__WEBPACK_EXTERNAL_MODULE__848__) => {
return /******/ (() => { // webpackBootstrap
/******/ 	"use strict";
/******/ 	var __webpack_modules__ = ({

/***/ 848:
/***/ ((module) => {

module.exports = __WEBPACK_EXTERNAL_MODULE__848__;

/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/make namespace object */
/******/ 	(() => {
/******/ 		// define __esModule on exports
/******/ 		__webpack_require__.r = (exports) => {
/******/ 			if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 				Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 			}
/******/ 			Object.defineProperty(exports, '__esModule', { value: true });
/******/ 		};
/******/ 	})();
/******/ 	
/************************************************************************/
var __webpack_exports__ = {};
// This entry need to be wrapped in an IIFE because it need to be isolated against other modules in the chunk.
(() => {
// ESM COMPAT FLAG
__webpack_require__.r(__webpack_exports__);

// EXTERNAL MODULE: external "../lispx-vm.umd.min.js"
var external_lispx_vm_umd_min_js_ = __webpack_require__(848);
;// CONCATENATED MODULE: ./test/test-util.lispx
/* harmony default export */ const test_util_lispx = (";;; Test Utilities\n\n;; Import Mocha test framework functions from the global scope.\n(def #'mocha:describe (to-lisp-function (js-global \"describe\")))\n(def #'mocha:it (to-lisp-function (js-global \"it\")))\n(def #'mocha:before (to-lisp-function (js-global \"before\")))\n(def #'mocha:before-each (to-lisp-function (js-global \"beforeEach\")))\n(def #'mocha:after (to-lisp-function (js-global \"after\")))\n(def #'mocha:after-each (to-lisp-function (js-global \"afterEach\")))\n\n(defexpr deftest (name expression . expected?) env\n  \"Run the EXPRESSION as a test identified by NAME and compare its\nresult to an EXPECTED value (which defaults to true).  Tests may\ncapture a continuation to the default prompt.\"\n  (mocha:it\n   (to-js-string (symbol-name name))\n   (js-lambda ()\n     ;; Run each test in a coroutine.\n     ;;\n     ;; Note that this is quite subtle: when an asynchronous test\n     ;; captures a continuation, it returns a promise here.  This\n     ;; promise is returned to Mocha, which does the right thing (wait\n     ;; for it to resolve).\n     (coroutine\n       ;; The test and expected expressions are evaluated in fresh\n       ;; child environments of the current environment, so that\n       ;; bindings they create don't affect other tests.\n       (assert (= (eval expression (make-environment env))\n                  (eval (optional expected? #t) (make-environment env))))))))\n\n(defmacro deftest* (name . forms)\n  \"Run the FORMS as a test identified by NAME, but unlike `deftest',\nsimply ignore their result.  The test only fails when the forms signal\nan error.  Use this instead of `deftest' if you just want to run some\nforms, and don't need to compare their result to an expected value.\"\n  (list #'deftest name (list* #'prog1 #t forms)))\n\n(defexpr defsuite (name . forms) env\n  \"Evaluate FORMS as a group of tests identified by NAME.  The forms\nmust not capure any continuations.\"\n  (mocha:describe\n   (to-js-string (symbol-name name))\n   (js-lambda ()\n     ;; We don't run the suite contents in a coroutine.  The reason is\n     ;; that Mocha describe() doesn't support promises.  Any Lisp code\n     ;; that tries to capture a continuation directly inside a suite\n     ;; will get a \"prompt not found\" error (due to the barrier inside\n     ;; `js-lambda').\n     (eval (list* #'progn forms) env))))\n\n(defun lispx::make-mocha-hook (#'mocha-hook-fun)\n  \"Metaprogramming utility to create Mocha hooks.  Hooks may capture.\"\n  (vau forms env\n    (mocha-hook-fun\n     (js-lambda ()\n       (coroutine ;; Run in coroutine. See above for explanation.\n         (eval (list* #'progn forms) env))))))\n\n(def #'before (lispx::make-mocha-hook #'mocha:before))\n(def #'before-each (lispx::make-mocha-hook #'mocha:before-each))\n(def #'after (lispx::make-mocha-hook #'mocha:after))\n(def #'after-each (lispx::make-mocha-hook #'mocha:after-each))\n\n(defexpr signals-error (form condition-class . slot-specs) env\n  \"Evaluate the FORM and assert that it signals a condition of the\ngiven CONDITION-CLASS.  Expected slots of the condition can be\nspecified by SLOT-SPECS (a plist) and will be compared against the\nslots of the signalled condition.\"\n  (let ((class (find-class condition-class env)))\n    (block exit\n      (handler-case ((object (lambda (c)\n                               (if (typep c class)\n                                   (progn\n                                     (loop-let -check-slots- ((slot-specs slot-specs))\n                                       (when (not (null slot-specs))\n                                         (let (((name value . rest-slot-specs) slot-specs))\n                                           (unless (= (slot-value c name) (eval value env))\n                                             (return-from exit #f))\n                                           (-check-slots- rest-slot-specs))))\n                                     (return-from exit #t))\n                                   (return-from exit #f)))))\n        (eval form env))\n      #f)))\n");
;// CONCATENATED MODULE: ./test/test-util-test.lispx
/* harmony default export */ const test_util_test_lispx = (";; Test before and after (mix in some sleeping to test async)\n\n(def #'test-util-test:x (box 1))\n\n(defsuite test-util-test:1\n  (before\n    (sleep 1)\n    (sleep 1)\n    (test-util-test:x 2)\n    (sleep 1))\n  (after\n    (sleep 1)\n    (sleep 1)\n    (assert (= (test-util-test:x) 3))\n    (test-util-test:x 4)\n    (sleep 1))\n  (deftest* check-it\n    (sleep 1)\n    (sleep 1)\n    (assert (= (test-util-test:x) 2))\n    (test-util-test:x 3)\n    (sleep 1))\n  (deftest* check-it-again\n    (sleep 1)\n    (sleep 1)\n    (assert (= (test-util-test:x) 3))\n    (sleep 1)))\n\n(defsuite test-util-test:2\n  (deftest* check-it-again-afterwards\n    (assert (= (test-util-test:x) 4))))\n\n;; Test before-each and after-each\n\n(def #'test-util-test:y (box 1))\n(def #'test-util-test:z (box 10))\n\n(defsuite test-util-test:3\n  (before-each\n    (sleep 1)\n    (sleep 1)\n    (test-util-test:y (+ (test-util-test:y) 1))\n    (sleep 1))\n  (after-each\n    (sleep 1)\n    (sleep 1)\n    (test-util-test:z (- (test-util-test:z) 1))\n    (sleep 1))\n  (deftest* t1\n    (sleep 1)\n    (sleep 1)\n    (assert (= (test-util-test:y) 2))\n    (assert (= (test-util-test:z) 10))\n      (sleep 1))\n  (deftest* t2\n    (sleep 1)\n    (sleep 1)\n    (assert (= (test-util-test:y) 3))\n    (assert (= (test-util-test:z) 9))\n    (sleep 1)))\n\n(defsuite test-util-test:4\n  (deftest* t3\n    (assert (= (test-util-test:y) 3))\n    (assert (= (test-util-test:z) 8))))\n");
;// CONCATENATED MODULE: ./test/boot-test.lispx
/* harmony default export */ const boot_test_lispx = (";;; LispX Main Test Suite\n\n;; The order of the tests is haphazard.  It should be brought in line\n;; with how the operators appear in boot.lispx.\n\n(defsuite signals-error\n\n  (deftest |SIGNALS-ERROR returns true if the expression signals.|\n    (signals-error\n     x\n     unbound-symbol-error))\n\n  (deftest |SIGNALS-ERROR returns true if the expression signals a subclass.|\n    (signals-error\n     x\n     error))\n\n  (deftest |SIGNALS-ERROR returns true if the expression signals and slots match.|\n    (signals-error\n     x\n     unbound-symbol-error :symbol 'x))\n\n  (deftest |SIGNALS-ERROR returns true if the expression signals a subclass and slots match.|\n    (signals-error\n     x\n     error :symbol 'x))\n\n  (deftest |SIGNALS-ERROR returns false if the expression signals but slots don't match.|\n    (signals-error\n     x\n     unbound-symbol-error :symbol 'y)\n    #f)\n\n  (deftest |SIGNALS-ERROR returns false if the expression doesn't signal.|\n    (signals-error\n     1\n     error)\n    #f)\n\n  (deftest |SIGNALS-ERROR returns false if the expression signals another condition type.|\n    (signals-error\n     x\n     match-error)\n    #f))\n\n(defsuite assert\n\n  (deftest assert.1\n    (assert (= 1 1))\n    #void)\n\n  (deftest assert.2\n    (signals-error\n     (assert (= 1 2))\n     assertion-error))\n\n  (deftest assert.3\n    (signals-error\n     (assert)\n     match-error))\n\n  (deftest assert.4\n    (signals-error\n     (assert 1)\n     type-error :datum 1 :expected-type 'boolean))\n\n  (deftest assert.5\n    (signals-error\n     (assert #t #t)\n     match-error)))\n\n(defsuite error\n\n  (deftest error.1\n    (signals-error\n     (error (make-instance #^standard-object :x 1 :y 2))\n     standard-object :x 1 :y 2))\n\n  (deftest error.2\n    (signals-error\n     (error)\n     match-error))\n\n  (deftest error.3\n    (handler-bind ((simple-error (lambda (e)\n                                   (invoke-restart 'continue 12))))\n      (error (make-simple-error \"Foo!\")\n        (abort (lambda (value) (* value 10)))\n        (continue (lambda (value) (* value 2)))))\n    24)\n\n  (deftest error.3b\n    (handler-bind ((simple-error (lambda (e)\n                                   (invoke-restart 'abort 12))))\n      (error (make-simple-error \"Foo!\")\n        (abort (lambda (value) (* value 10)))\n        (continue (lambda (value) (* value 2)))))\n    120)\n\n  (deftest error.4\n    (restart-case ((abort (lambda (value) (* 1000 value))))\n      (handler-bind ((simple-error (lambda (e)\n                                     (invoke-restart 'abort 12))))\n        (error (error (make-simple-error \"Foo!\"))\n          (abort (lambda (value) (* value 10)))\n          (continue (lambda (value) (* value 2))))))\n    12000)\n\n  (deftest error.4b\n    (block b\n      (restart-case ((abort (lambda (value) (* 1000 value))))\n        (handler-bind ((simple-error (lambda (e)\n                                       (invoke-restart 'abort 12))))\n          (error (error (make-simple-error \"Foo!\")\n                   (abort (lambda (value) (return-from b (* 10000 value)))))\n            (abort (lambda (value) (* value 10)))\n            (continue (lambda (value) (* value 2)))))))\n    120000))\n\n(defsuite assert-type\n\n  (deftest assert-type.1\n    (assert-type #t #^boolean))\n\n  (deftest assert-type.2\n    (signals-error\n     (assert-type #t #^string)\n    type-error :datum #t :expected-type 'string))\n\n  (deftest assert-type.3\n    (signals-error\n     (assert-type #t #f)\n     type-error :datum #f :expected-type 'class))\n\n  (deftest assert-type.4\n    (signals-error\n     (assert-type)\n     match-error))\n\n  (deftest assert-type.5\n    (signals-error\n     (assert-type #t)\n     match-error)))\n\n(defsuite the\n\n  (deftest the.1\n    (the boolean (= 1 1))\n    #t)\n\n  (deftest the.2\n    (signals-error\n     (the string #t)\n     type-error :datum #t :expected-type 'string))\n\n  (deftest the.3\n    (signals-error\n     (the #t #f)\n     type-error :datum #t :expected-type 'symbol))\n\n  (deftest the.4\n    (signals-error\n     (the)\n     match-error))\n\n  (deftest the.5\n    (signals-error\n     (the #t)\n     match-error)))\n\n(defsuite make-type-error\n\n  (deftest make-type-error.1\n    (let ((e (make-type-error \"foo\" 'number)))\n      (assert (typep e #^type-error))\n      (assert (typep e #^error))\n      (assert (typep e #^condition))\n      (assert (= \"foo\" (slot-value e 'datum)))\n      (assert (= 'number (slot-value e 'expected-type))))\n    #void))\n\n(defsuite simple-error\n\n  (deftest make-simple-error.1\n    (let ((e (make-simple-error \"foo\")))\n      (assert (typep e #^simple-error))\n      (assert (typep e #^error))\n      (assert (typep e #^condition))\n      (assert (= \"foo\" (slot-value e 'message))))\n    #void)\n\n  (deftest simple-error.1\n    (signals-error\n     (simple-error)\n     match-error))\n\n  (deftest simple-error.2\n    (signals-error\n     (simple-error \"foo\")\n     simple-error :message \"foo\")))\n\n(defsuite make-restart-error\n\n  (deftest make-restart-error.1\n    (let ((e (make-restart-error 'continue)))\n      (assert (typep e #^restart-error))\n      (assert (typep e #^error))\n      (assert (typep e #^condition))\n      (assert (= 'continue (slot-value e 'restart-name))))\n    #void))\n\n(defsuite vau\n\n  (deftest |A fexpr without body expression results in #VOID.|\n    ((vau #ignore #ignore))\n    #void)\n\n  (deftest |A fexpr returns its operands unevaluated.|\n    ((vau ops #ignore ops) x y z)\n    '(x y z))\n\n  (deftest |A fexpr can have multiple body expressions.|\n    (progn\n      (def env (the-environment))\n      (assert (= ((vau ops #ignore (set env var 1) ops) x y z)\n                 '(x y z)))\n      (assert (= (eval 'var env) 1)))\n    #void)\n\n  (deftest |The environment parameter receives the current environment.|\n    (progn\n      (def env (the-environment))\n      (eq ((vau #ignore e e)) env)))\n\n  (deftest |Fexprs are instances of FEXPR, OPERATOR, OBJECT.|\n    (let ((#'fexpr (vau #ignore #ignore 12)))\n      (typep #'fexpr #^fexpr)\n      (typep #'fexpr #^operator)\n      (typep #'fexpr #^object)\n      (fexpr))\n    12)\n\n  (deftest |Arity check 1.|\n    (signals-error\n     (vau)\n     match-error))\n\n  (deftest |Arity check 2.|\n    (signals-error\n     (vau x)\n     match-error)))\n\n(defsuite quote\n\n  (deftest |Quoted symbols are pointer identical.|\n    (and\n     (eq 'foo 'foo)\n     (eq '#'foo '#'foo)\n     (eq '#'foo (function-symbol 'foo))\n     (eq '#^foo '#^foo)\n     (eq '#^foo (class-symbol 'foo))))\n\n  (deftest |Ordinary symbols can be quoted.|\n    (eq 'foo (variable-symbol 'foo)))\n\n  (deftest |Function symbols can be quoted.|\n    (eq '#'foo (function-symbol 'foo)))\n\n  (deftest |Class symbols can be quoted.|\n    (eq '#^foo (class-symbol 'foo)))\n\n  (deftest |Lists can be quoted.|\n    (= '(1 2 3) (list 1 2 3)))\n\n  (deftest |Numbers can be quoted.|\n    (= '1 1))\n\n  (deftest |Strings can be quoted.|\n    (= '\"foo\" \"foo\"))\n\n  (deftest |QUOTE requires at least one argument.|\n    (signals-error\n     (quote)\n     match-error))\n\n  (deftest |QUOTE requires at most one argument.|\n    (signals-error\n     (quote 1 2 3)\n     match-error)))\n\n(defsuite list\n\n  (deftest list.1\n    (eq #nil (list)))\n\n  (deftest list.2\n    (= '(1) (list 1)))\n\n  (deftest list.3\n    (= '((1) (2)) (list (list 1) (list 2)))))\n\n(defsuite list*\n\n  (deftest list*.1\n    (list*)\n    #nil)\n\n  (deftest list*.2\n    (list* 1 2 '(3))\n    '(1 2 3))\n\n  (deftest list*.3\n    (list* 1 2 3)\n    '(1 2 . 3))\n\n  (deftest list*.4\n    (list* 1 2)\n    '(1 . 2))\n\n  (deftest list*.5\n    (list* 2)\n    '2))\n\n(defsuite the-environment\n\n  (deftest the-environment.1\n    (let ((x 1))\n      (eval 'x (the-environment)))\n    1)\n\n  (deftest the-environment.2\n    (let ((x 1)\n          (env (the-environment)))\n      (boundp 'x env))\n    #f)\n\n  (deftest the-environment.3\n    (let* ((x 1)\n           (env (the-environment)))\n      (boundp 'x env))\n    #t)\n\n  (deftest the-environment.4\n    (signals-error\n     (the-environment #t)\n     match-error)))\n\n(defsuite boundp\n\n  (deftest boundp.1\n    (boundp 'nope (the-environment))\n    #f)\n\n  (deftest boundp.2\n    (boundp '#'defun (the-environment)))\n\n  (deftest boundp.3\n    (boundp '#^standard-object (the-environment)))\n\n  (deftest boundp.4\n    (signals-error\n     (boundp)\n     match-error))\n\n  (deftest boundp.5\n    (signals-error\n     (boundp 'x)\n     match-error))\n\n  (deftest boundp.6\n    (signals-error\n     (boundp 12 (the-environment))\n     type-error :datum 12 :expected-type 'symbol))\n\n  (deftest boundp.7\n    (signals-error\n     (boundp 'x 33)\n     type-error :datum 33 :expected-type 'environment))\n\n  (deftest boundp.8\n    (signals-error\n     (boundp 12 (the-environment) #t)\n     match-error)))\n\n(defsuite macro\n\n  (deftest macro.0\n    ((macro ()))\n    #void)\n\n  (deftest macro.1\n    (let ((z 1))\n      ((macro () 'x 'y 'z)))\n    1)\n\n  (deftest macro.2\n    ((macro () (list #'= 1 1)))\n    #t)\n\n  (deftest macro.3\n    (signals-error\n     (macro)\n     match-error)\n    #t))\n\n(defsuite defmacro\n\n  (deftest defmacro.1\n    (progn\n      (defmacro myprogn body\n        (list* #'progn body))\n      (myprogn 1 2 3))\n    3)\n\n  (deftest defmacro.2\n    (signals-error\n     (defmacro)\n     match-error))\n\n  (deftest defmacro.3\n    (signals-error\n     (defmacro foo)\n     match-error))\n\n  (deftest defmacro.4\n    (progn\n      (defmacro foo ())\n      (foo))\n    #void))\n\n(defsuite defexpr\n\n  (deftest defexpr.1\n    (progn\n      (defexpr myprogn body env\n        (eval (list* #'progn body) env))\n      (myprogn 1 2 3))\n    3)\n\n  (deftest defexpr.2\n    (signals-error\n     (defexpr)\n     match-error))\n\n  (deftest defexpr.3\n    (signals-error\n     (defexpr foo)\n     match-error))\n\n  (deftest defexpr.3b\n    (signals-error\n     (defexpr foo ())\n     match-error))\n\n  (deftest defexpr.4\n    (progn\n      (defexpr foo () #ignore)\n      (foo))\n    #void))\n\n(defsuite def\n\n  (deftest def.1\n    (progn\n      (def (x y z) (list 1 2 3))\n      (assert (= x 1))\n      (assert (= y 2))\n      (assert (= z 3)))\n    #void)\n\n  (deftest def.2\n    (def :key :key)\n    :key)\n\n  (deftest def.2b\n    (signals-error\n     (def :key :key2)\n     match-error :definiend :key :value :key2))\n\n  (deftest def.3\n    (def #ignore 2)\n    2)\n\n  (deftest def.4\n    (def x 1)\n    1)\n\n  (deftest def.5\n    (def () ())\n    ())\n\n  (deftest def.6\n    (signals-error\n     (def)\n     match-error))\n\n  (deftest def.7\n    (signals-error\n     (def x)\n     match-error))\n\n  (deftest def.8\n    (signals-error\n     (def #t 12)\n     type-error :datum #t :expected-type '(or symbol ignore list)))\n\n  (deftest def.9\n    (def x 1 \"docstring\")\n    1))\n\n(defsuite defconstant\n\n  (deftest defconstant.1\n    (progn\n      (defconstant x 1)\n      x)\n    1)\n\n  (deftest defconstant.2\n    (signals-error\n     (defconstant)\n     match-error))\n\n  (deftest defconstant.3\n    (signals-error\n     (defconstant x)\n     match-error))\n\n  (deftest defconstant.4\n    (progn\n      (defconstant x 1)\n      x)\n    1)\n\n  (deftest defconstant.5\n    (defconstant x 1)\n    1))\n\n(defsuite lambda\n\n  (deftest lambda.1\n    ((lambda ()))\n    #void)\n\n  (deftest lambda.2\n    ((lambda args 'whatever args) 1 2 (intern \"foo\"))\n    '(1 2 foo))\n\n  (deftest lambda.3\n    (signals-error\n     (lambda)\n     match-error))\n\n  (deftest lambda.4\n    (signals-error\n     ((lambda #t))\n     type-error :datum #t :expected-type '(or symbol ignore list))))\n\n(defsuite defun\n\n  (deftest defun.1\n    (progn\n      (defun foo () 'foo)\n      (defun bar (x) (list (foo) x))\n      (bar 'bar))\n    '(foo bar))\n\n  (deftest defun.2\n    (progn\n      (defun nope ())\n      (nope))\n    #void)\n\n  (deftest defun.3\n    (signals-error\n     (defun)\n     match-error))\n\n  (deftest defun.4\n    (signals-error\n     (defun foo)\n     match-error))\n\n  (deftest defun.5\n    (signals-error\n     (defun #t ())\n     type-error :datum #t :expected-type 'symbol))\n\n  (deftest defun.6\n    (typep (defun foo ()) #^function)))\n\n(defsuite progn\n\n  (deftest progn.1\n    (progn)\n    #void)\n\n  (deftest progn.2\n    (progn (list))\n    (list))\n\n  (deftest progn.3\n    (progn\n      (def env (the-environment))\n      (progn (set env x 1) (set env x 2) x))\n    2))\n\n(defsuite prog1\n\n  (deftest prog1.1\n    (prog1 1)\n    1)\n\n  (deftest prog1.2\n    (prog1 1 2 3)\n    1)\n\n  (deftest prog1.3\n    (signals-error\n     (prog1)\n     match-error))\n\n  (deftest prog1.4\n    (progn\n      (def env (the-environment))\n      (assert (= 4 (prog1 4 (set env x 2))))\n      x)\n    2))\n\n(defsuite eval\n\n  (deftest eval.1\n    (eval #nil (the-environment))\n    #nil)\n\n  (deftest eval.2\n    (eval '(= 1 2) (the-environment))\n    #f)\n\n  (deftest eval.3\n    (signals-error\n     (eval)\n     match-error))\n\n  (deftest eval.4\n    (signals-error\n     (eval '1)\n     match-error))\n\n  (deftest eval.5\n    (signals-error\n     (eval 'x (the-environment))\n     unbound-symbol-error\n     :symbol 'x))\n\n  (deftest eval.6\n    (signals-error\n     (eval '1 (the-environment) #t)\n     match-error)))\n\n(defsuite cons\n\n  (deftest cons.1\n    (cons (list 1) (list 2))\n    '((1) . (2)))\n\n  (deftest cons.2\n    (cons (list 1) #nil)\n    '((1)))\n\n  (deftest cons.3\n    (signals-error\n     (cons)\n     match-error))\n\n  (deftest cons.4\n    (signals-error\n     (cons 1)\n     match-error))\n\n  (deftest cons.5\n    (signals-error\n     (cons 1 2 3)\n     match-error)))\n\n(defsuite car\n\n  (deftest car.1\n    (car (list 1 2))\n    1)\n\n  (deftest car.2\n    (signals-error\n     (car)\n     match-error))\n\n  (deftest car.3\n    (signals-error\n     (car 1)\n     type-error :datum 1 :expected-type 'cons))\n\n  (deftest car.4\n    (signals-error\n     (car 1 2)\n     match-error)))\n\n(defsuite cdr\n\n  (deftest cdr.1\n    (cdr (list 1 2))\n    '(2))\n\n  (deftest cdr.2\n    (signals-error\n     (cdr)\n     match-error))\n\n  (deftest cdr.3\n    (signals-error\n     (cdr 1)\n     type-error :datum 1 :expected-type 'cons))\n\n  (deftest car.4\n    (signals-error\n     (cdr 1 2)\n     match-error)))\n\n(defsuite symbol-name\n\n  (deftest symbol-name.1\n    (= \"foo\" (symbol-name 'foo)))\n\n  (deftest symbol-name.2\n    (= \"foo\" (symbol-name '#'foo)))\n\n  (deftest symbol-name.3\n    (= \"foo\" (symbol-name (class-symbol 'foo))))\n\n  (deftest symbol-name.4\n    (= \"foo\" (symbol-name :foo)))\n\n  (deftest symbol-name.5\n    (signals-error\n     (symbol-name)\n     match-error))\n\n  (deftest symbol-name.6\n    (signals-error\n     (symbol-name 12)\n     type-error :datum 12 :expected-type 'symbol)))\n\n(defsuite variable-symbol\n\n  (deftest variable-symbol.1\n    (variable-symbol '#'foo)\n    'foo)\n\n  (deftest variable-symbol.2\n    (variable-symbol :foo)\n    'foo)\n\n  (deftest variable-symbol.4\n    (variable-symbol (class-symbol 'foo))\n    'foo)\n\n  (deftest variable-symbol.5\n    (signals-error\n     (variable-symbol)\n     match-error))\n\n  (deftest variable-symbol.6\n    (signals-error\n     (variable-symbol 5)\n     type-error :datum 5 :expected-type 'symbol)))\n\n(defsuite function-symbol\n\n  (deftest function-symbol.1\n    (function-symbol 'foo)\n    '#'foo)\n\n  (deftest function-symbol.2\n    (function-symbol :foo)\n    '#'foo)\n\n  (deftest function-symbol.4\n    (function-symbol (class-symbol 'foo))\n    '#'foo)\n\n  (deftest function-symbol.5\n    (signals-error\n     (function-symbol)\n     match-error))\n\n  (deftest function-symbol.6\n    (signals-error\n     (function-symbol 5)\n     type-error :datum 5 :expected-type 'symbol)))\n\n(defsuite class-symbol\n\n  (deftest class-symbol.1\n    (class-symbol 'foo)\n    (class-symbol 'foo))\n\n  (deftest class-symbol.2\n    (class-symbol :foo)\n    (class-symbol 'foo))\n\n  (deftest class-symbol.3\n    (class-symbol '#'foo)\n    (class-symbol 'foo))\n\n  (deftest class-symbol.5\n    (signals-error\n     (class-symbol)\n     match-error))\n\n  (deftest class-symbol.6\n    (signals-error\n     (class-symbol 5)\n     type-error :datum 5 :expected-type 'symbol)))\n\n(defsuite keyword-symbol\n\n  (deftest keyword-symbol.1\n    (keyword-symbol 'foo)\n    :foo)\n\n  (deftest keyword-symbol.2\n    (keyword-symbol '#'foo)\n    :foo)\n\n  (deftest keyword-symbol.3\n    (keyword-symbol (class-symbol 'foo))\n    :foo)\n\n  (deftest keyword-symbol.5\n    (signals-error\n     (keyword-symbol)\n     match-error))\n\n  (deftest keyword-symbol.6\n    (signals-error\n     (keyword-symbol 5)\n     type-error :datum 5 :expected-type 'symbol)))\n\n(defsuite wrap\n\n  (deftest wrap.1\n    ((wrap (vau (x) #ignore x)) (list 1 2 3))\n    '(1 2 3))\n\n  (deftest wrap.2\n    (signals-error\n     (wrap)\n     match-error))\n\n  (deftest wrap.3\n    (signals-error\n     (wrap 33)\n     type-error :datum 33 :expected-type 'operator)))\n\n(defsuite unwrap\n\n  (deftest unwrap.1\n    ((unwrap (lambda (x) x)) (list 1 2 3))\n    '(list 1 2 3))\n\n  (deftest unwrap.2\n    (signals-error\n     (unwrap)\n     match-error))\n\n  (deftest unwrap.3\n    (signals-error\n     (unwrap 33)\n     type-error :datum 33 :expected-type 'function)))\n\n(defsuite if\n\n  (deftest if.1\n    (if #t (= 1 1) (= 1 2))\n    #t)\n\n  (deftest if.2\n    (if #f (= 1 1) (= 1 2))\n    #f)\n\n  (deftest if.3\n    (signals-error\n     (if)\n     match-error))\n\n  (deftest if.4\n    (signals-error\n     (if #t)\n     match-error))\n\n  (deftest if.5\n    (signals-error\n     (if #t #f)\n     match-error))\n\n  (deftest if.6\n    (signals-error\n     (if 1 #f #t)\n     type-error :datum 1 :expected-type 'boolean)))\n\n(defsuite cond\n\n  (deftest cond.1\n    (cond)\n    #void)\n\n  (deftest cond.2\n    (cond (#f 1 2 3))\n    #void)\n\n  (deftest cond.3\n    (cond (#f 1 2 3)\n          ((= 1 1) 4 5 (= 1 1)))\n    #t)\n\n  (deftest cond.4\n    (signals-error\n     (cond (1 #t))\n     type-error :datum 1 :expected-type 'boolean)))\n\n(defsuite typecase\n\n  (deftest typecase.1\n    (signals-error\n     (typecase)\n     match-error))\n\n  (deftest typecase.2\n    (typecase (+ 2 2))\n    #void)\n\n  (deftest typecase.3\n    (typecase (+ 2 2)\n      (object 1)\n      (number 2))\n    1)\n\n  (deftest typecase.4\n    (typecase (+ 2 2)\n      (number 2)\n      (object 1))\n    2))\n\n(defsuite etypecase\n\n  (deftest etypecase.1\n    (signals-error\n     (etypecase)\n     match-error))\n\n  (deftest etypecase.2\n    (signals-error\n     (etypecase (+ 2 2))\n     type-error :datum 4))\n\n  (deftest etypecase.3\n    (etypecase (+ 2 2)\n      (object 1)\n      (number 2))\n    1)\n\n  (deftest etypecase.4\n    (etypecase (+ 2 2)\n      (number 2)\n      (object 1))\n    2)\n\n  (deftest etypecase.5\n    (signals-error\n     (etypecase (+ 2 2)\n       (string 2)\n       (boolean 1))\n     type-error :datum 4)))\n\n(defsuite not\n\n  (deftest not.1\n    (not #t)\n    #f)\n\n  (deftest not.2\n    (not #f)\n    #t)\n\n  (deftest not.3\n    (signals-error\n     (not)\n     match-error))\n\n  (deftest not.4\n    (signals-error\n     (not 1)\n     type-error :datum 1 :expected-type 'boolean)))\n\n(defsuite eq\n\n  (deftest eq.1\n    (eq 'foo 'foo)\n    #t)\n\n  (deftest eq.2\n    (eq 'foo 'bar)\n    #f)\n\n  (deftest eq.3\n    (eq #t #t)\n    #t)\n\n  (deftest eq.4\n    (eq #f #f)\n    #t)\n\n  (deftest eq.5\n    (eq #void #void)\n    #t)\n\n  (deftest eq.6\n    (eq #ignore #ignore)\n    #t)\n\n  (deftest eq.7\n    (eq #nil #nil)\n    #t)\n\n  (deftest eq.8\n    (eq #nil ())\n    #t)\n\n  (deftest eq.9\n    (eq #nil '())\n    #t)\n\n  (deftest eq.10\n    (signals-error\n     (eq)\n     match-error))\n\n  (deftest eq.11\n    (signals-error\n     (eq 11)\n     match-error)))\n\n(defsuite =\n\n  (deftest =.1\n    (= 1 1 1)\n    #t)\n\n  (deftest =.2\n    (= 1 1 2)\n    #f)\n\n  (deftest =.3\n    (= #t #t #t)\n    #t)\n\n  (deftest =.4\n    (= #t #t #f)\n    #f)\n\n  (deftest =.5\n    (= \"foo\" \"foo\" \"foo\")\n    #t)\n\n  (deftest =.6\n    (= (list (list 1)) (list (list 1)))\n    #t)\n\n  (deftest =.7\n    (signals-error\n     (=)\n     match-error))\n\n  (deftest =.8\n    (signals-error\n     (= 1)\n     match-error)))\n\n(defsuite /=\n\n  (deftest /=.1\n    (/= 1 1 1)\n    #f)\n\n  (deftest /=.2\n    (/= 1 1 2)\n    #f)\n\n  (deftest /=.3\n    (/= 1 2 3)\n    #t)\n\n  (deftest /=.4\n    (/= #t #t #f)\n    #f)\n\n  (deftest /=.4b\n    (/= #t #f)\n    #t)\n\n  (deftest /=.5\n    (/= \"foo\" \"foo\" \"foo\")\n    #f)\n\n  (deftest /=.5b\n    (/= \"foo\" \"bar\" \"quux\")\n    #t)\n\n  (deftest /=.6\n    (/= (list (list 1)) (list (list 2)))\n    #t)\n\n  (deftest /=.7\n    (signals-error\n     (/=)\n     match-error))\n\n  (deftest /=.8\n    (/= 1)\n    #t)\n\n  (deftest /=.9\n    (/= 1 2 3 3)\n    #f)\n\n  (deftest /=.9b\n    (/= 3 1 2 3)\n    #f)\n\n  (deftest /=.9c\n    (/= 3 1 2 3 4)\n    #f)\n\n  (deftest /=.9d\n    (/= 1 2 3 3 4)\n    #f)\n\n  (deftest /=.9e\n    (/= 3 3 4)\n    #f)\n\n  (deftest /=.10\n    (/= #t \"a\")\n    #t))\n\n(defsuite <\n\n  (deftest <.1\n    (< 1 1 1)\n    #f)\n\n  (deftest <.2\n    (< 1 1 2)\n    #f)\n\n  (deftest <.3\n    (< -3 -2 -1 0 1 2 3)\n    #t)\n\n  (deftest <.4\n    (signals-error\n     (< #t #t #f)\n    error))\n\n  (deftest <.5\n    (signals-error\n     (< \"foo\" \"foo\" \"foo\")\n     error))\n\n  (deftest <.6\n    (signals-error\n     (< (list (list 1)) (list (list 2)))\n     error))\n\n  (deftest <.7\n    (signals-error\n     (<)\n     match-error))\n\n  (deftest <.8\n    (signals-error\n     (< 1)\n     match-error))\n\n  (deftest <.9\n    (< 1 2 3 3)\n    #f)\n\n  (deftest <.different-types\n    (signals-error\n     (< 1 2 3 \"foo\")\n     type-error :datum \"foo\" :expected-type 'number)))\n\n(defsuite >\n\n  (deftest >.1\n    (> 1 1 1)\n    #f)\n\n  (deftest >.2\n    (> 2 1 1)\n    #f)\n\n  (deftest >.3\n    (> 3 2 1 0 -1 -2 -3)\n    #t)\n\n  (deftest >.4\n    (signals-error\n     (> #t #t #f)\n    error))\n\n  (deftest >.5\n    (signals-error\n     (> \"foo\" \"foo\" \"foo\")\n     error))\n\n  (deftest >.6\n    (signals-error\n     (> (list (list 1)) (list (list 2)))\n     error))\n\n  (deftest >.7\n    (signals-error\n     (>)\n     match-error))\n\n  (deftest >.8\n    (signals-error\n     (> 1)\n     match-error))\n\n  (deftest >.9\n    (> 3 2 1 1)\n    #f)\n\n  (deftest >.different-types\n    (signals-error\n     (> 3 2 1 \"foo\")\n     type-error :datum \"foo\" :expected-type 'number)))\n\n(defsuite <=\n\n  (deftest <=.1\n    (<= 1 1 1)\n    #t)\n\n  (deftest <=.2\n    (<= 1 1 2)\n    #t)\n\n  (deftest <=.3\n    (<= -3 -2 -1 0 1 2 3)\n    #t)\n\n  (deftest <=.4\n    (signals-error\n     (<= #t #t #f)\n    error))\n\n  (deftest <=.5\n    (signals-error\n     (<= \"foo\" \"foo\" \"foo\")\n     error))\n\n  (deftest <=.6\n    (signals-error\n     (<= (list (list 1)) (list (list 2)))\n     error))\n\n  (deftest <=.7\n    (signals-error\n     (<=)\n     match-error))\n\n  (deftest <=.8\n    (signals-error\n     (<= 1)\n     match-error))\n\n  (deftest <=.9\n    (<= 1 2 3 3)\n    #t)\n\n  (deftest <=.10\n    (<= 1 2 3 3 -1)\n    #f)\n\n  (deftest <=.different-types\n    (signals-error\n     (<= 1 1 3 \"foo\")\n     type-error :datum \"foo\" :expected-type 'number)))\n\n(defsuite >=\n\n  (deftest >=.1\n    (>= 1 1 1)\n    #t)\n\n  (deftest >=.2\n    (>= 2 1 1)\n    #t)\n\n  (deftest >=.3\n    (>= 3 2 1 1 0 0 -1 -2 -3 -3)\n    #t)\n\n  (deftest >=.4\n    (signals-error\n     (>= #t #t #f)\n    error))\n\n  (deftest >=.5\n    (signals-error\n     (>= \"foo\" \"foo\" \"foo\")\n     error))\n\n  (deftest >=.6\n    (signals-error\n     (>= (list (list 1)) (list (list 2)))\n     error))\n\n  (deftest >=.7\n    (signals-error\n     (>=)\n     match-error))\n\n  (deftest >=.8\n    (signals-error\n     (>= 1)\n     match-error))\n\n  (deftest >=.9\n    (>= 3 3 2 1)\n    #t)\n\n  (deftest >=.10\n    (>= 3 3 2 100)\n    #f)\n\n  (deftest >=.different-types\n    (signals-error\n     (>= 3 3 2 \"foo\")\n     type-error :datum \"foo\" :expected-type 'number)))\n\n(defsuite +\n\n  (deftest +.1\n    (+)\n    0)\n\n  (deftest +.2\n    (+ 10)\n    10)\n\n  (deftest +.3\n    (+ 10 10 10)\n    30))\n\n(defsuite -\n\n  (deftest -.1\n    (signals-error\n     (-)\n     match-error))\n\n  (deftest -.2\n    (- 1)\n    -1)\n\n  (deftest -.3\n    (- -1)\n    1)\n\n  (deftest -.4\n    (- 100 10 1)\n    89))\n\n(defsuite *\n\n  (deftest *.1\n    (*)\n    1)\n\n  (deftest *.2\n    (* 10)\n    10)\n\n  (deftest *.3\n    (* 10 -10 10)\n    -1000))\n\n(defsuite /\n\n  (deftest /.1\n    (signals-error\n     (/)\n     match-error))\n\n  (deftest /.2\n    (/ 10)\n    0.1)\n\n  (deftest /.3\n    (/ 100 10 2)\n    5)\n\n  (deftest /.4\n    (/ -100 10 2)\n    -5))\n\n(defsuite typep\n\n  (deftest typep.1\n    (and\n     (typep #t #^boolean)\n     (typep #f #^boolean)\n     (typep 12 #^number)\n     (typep \"foo\" #^string)\n     (typep #nil #^nil)\n     (typep #^object #^class)))\n\n  (deftest typep.2\n    (and\n     (typep #^condition #^standard-class)\n     (typep #^condition #^class)\n     (typep #^condition #^object)))\n\n  (deftest typep.3\n    (signals-error\n     (typep)\n     match-error))\n\n  (deftest typep.3\n    (signals-error\n     (typep \"foo\")\n     match-error)))\n\n(defsuite intern\n\n  (deftest intern.1\n    (eq 'foo (intern \"foo\")))\n\n  (deftest intern.2\n    (signals-error\n     (intern 12)\n     type-error :datum 12 :expected-type 'string))\n\n  (deftest intern.3\n    (signals-error\n     (intern)\n     match-error)))\n\n(defsuite apply\n\n  (deftest apply.1\n    (apply #'list ())\n    '())\n\n  (deftest apply.2\n    (apply #'list '(1))\n    '(1))\n\n  (deftest apply.3\n    (signals-error\n     (apply)\n     match-error))\n\n  (deftest apply.4\n    (signals-error\n     (apply #'list)\n     match-error))\n\n  (deftest apply.5\n    (signals-error\n     (apply 12 '())\n     type-error :datum 12 :expected-type 'function)))\n\n(defsuite find-class\n\n  (deftest find-class.1\n    (eq (find-class 'string (the-environment))\n        (class-of \"foo\")))\n\n  (deftest find-class.2a\n    (signals-error\n     (find-class)\n     match-error))\n\n  (deftest find-class.2b\n    (signals-error\n     (find-class 'foo)\n     match-error))\n\n  (deftest find-class.3\n    (signals-error\n     (find-class \"string\" (the-environment))\n     type-error :datum \"string\" :expected-type 'symbol))\n\n  (deftest find-class.4\n    (signals-error\n     (find-class 'does-not-exist (the-environment))\n     unbound-symbol-error :symbol (class-symbol 'does-not-exist))))\n\n(defsuite make-environment\n\n  (deftest make-environment.1\n    (let ((env (make-environment)))\n      (boundp '#'def env))\n    #f)\n\n  (deftest make-environment.2\n    (let* ((env (make-environment (the-environment)))\n           (child (make-environment env)))\n      (assert (not (boundp 'x env)))\n      (assert (not (boundp 'x child)))\n      (eval '(def x 1) env)\n      (assert (boundp 'x env))\n      (assert (boundp 'x child))\n      (assert (= 1 (eval 'x env)))\n      (assert (= 1 (eval 'x child))))\n    #void)\n\n  (deftest make-environment.3\n    (signals-error\n     (make-environment 12)\n     type-error :datum 12 :expected-type 'environment)))\n\n(defsuite cxxr\n\n  (deftest caar.1\n    (caar '((1 2) (3 4) (5 6)))\n    1)\n\n  (deftest caar.2\n    (signals-error\n     (caar)\n     match-error))\n\n  (deftest caar.3\n    (signals-error\n     (caar 1)\n     type-error :datum 1 :expected-type 'cons))\n\n  (deftest cadr.1\n    (cadr '((1 2) (3 4) (5 6)))\n    '(3 4))\n\n  (deftest cadr.2\n    (signals-error\n     (cadr)\n     match-error))\n\n  (deftest cadr.3\n    (signals-error\n     (cadr 1)\n     type-error :datum 1 :expected-type 'cons))\n\n  (deftest cddr.1\n    (cddr '((1 2) (3 4) (5 6)))\n    '((5 6)))\n\n  (deftest cddr.2\n    (signals-error\n     (cddr)\n     match-error))\n\n  (deftest cddr.3\n    (signals-error\n     (cddr 1)\n     type-error :datum 1 :expected-type 'cons))\n\n  (deftest cdar.1\n    (cdar '((1 2) (3 4) (5 6)))\n    '(2))\n\n  (deftest cdar.2\n    (signals-error\n     (cdar)\n     match-error))\n\n  (deftest cdar.3\n    (signals-error\n     (cdar 1)\n     type-error :datum 1 :expected-type 'cons))\n\n(defsuite when\n\n  (deftest when.1\n    (when #t)\n    #void)\n\n  (deftest when.2\n    (when #f)\n    #void)\n\n  (deftest when.3\n    (when #t 1 2 (= 1 1))\n    #t)\n\n  (deftest when.4\n    (when #f 1 2 3)\n    #void)\n\n  (deftest when.5\n    (signals-error\n     (when)\n     match-error))\n\n  (deftest when.6\n    (signals-error\n     (when 1)\n     type-error :datum 1 :expected-type 'boolean))))\n\n(defsuite unless\n\n  (deftest unless.1\n    (unless #t)\n    #void)\n\n  (deftest unless.2\n    (unless #f)\n    #void)\n\n  (deftest unless.3\n    (unless #f 1 2 (= 1 1))\n    #t)\n\n  (deftest unless.4\n    (unless #t 1 2 3)\n    #void)\n\n  (deftest unless.5\n    (signals-error\n     (unless)\n     match-error))\n\n  (deftest unless.6\n    (signals-error\n     (unless 1)\n     type-error :datum 1 :expected-type 'boolean)))\n\n(defsuite null\n\n  (deftest null.1\n    (null #nil)\n    #t)\n\n  (deftest null.2\n    (null '(1))\n    #f)\n\n  (deftest null.3\n    (signals-error\n     (null)\n     match-error))\n\n  (deftest null.4\n    (null 1)\n    #f))\n\n(defsuite consp\n\n  (deftest consp.1\n    (consp #nil)\n    #f)\n\n  (deftest consp.2\n    (consp '(1))\n    #t)\n\n  (deftest consp.3\n    (signals-error\n     (consp)\n     match-error))\n\n  (deftest consp.4\n    (consp 1)\n    #f))\n\n(defsuite identity\n\n  (deftest identity.1\n    (identity #nil)\n    #nil)\n\n  (deftest identity.2\n    (identity '(1))\n    '(1))\n\n  (deftest identity.3\n    (signals-error\n     (identity)\n     match-error))\n\n  (deftest identity.4\n    (identity (+ 1 1))\n    2))\n\n(defsuite compose\n\n  (deftest compose.1\n    (signals-error\n     (compose)\n     match-error))\n\n  (deftest compose.2\n    (signals-error\n     (compose (lambda ()))\n     match-error))\n\n  (deftest compose.3\n    (typep (compose (lambda ()) (lambda ())) #^function))\n\n  (deftest compose.4\n    ((compose #'identity #'identity) 12)\n    12)\n\n  (deftest compose.5\n    ((compose #'list #'length) 'a 'b 'c)\n    3)\n\n  (deftest compose.6\n    ((compose #'list #'car) 'a 'b 'c)\n    'a))\n\n(defsuite mapcar\n\n  (deftest mapcar.1\n    (mapcar #'intern ())\n    ())\n\n  (deftest mapcar.2\n    (mapcar #'intern '(\"foo\" \"bar\"))\n    '(foo bar))\n\n  (deftest mapcar.3\n    (signals-error\n     (mapcar)\n     match-error))\n\n  (deftest mapcar.4\n    (signals-error\n     (mapcar #'intern)\n     match-error))\n\n  (deftest mapcar.5\n    (signals-error\n     (mapcar #'intern 12)\n     type-error :datum 12 :expected-type 'cons))\n\n  (deftest mapcar.6\n    (signals-error\n     (mapcar 12 '(1))\n     type-error :datum 12 :expected-type 'operator)))\n\n(defsuite reduce\n\n  (deftest reduce.1\n    (reduce #'%* (list 1 2 3) :initial-value 10)\n    60)\n\n  (deftest reduce.2\n    (reduce #'%* (list) :initial-value 10)\n    10)\n\n  (deftest reduce.3\n    (signals-error\n     (reduce)\n     match-error))\n\n  (deftest reduce.4\n    (signals-error\n     (reduce #'%*)\n     match-error))\n\n  (deftest reduce.5\n    (signals-error\n     (reduce #'%* (list))\n     match-error))\n\n  (deftest reduce.ansi.3\n    (reduce #'cons '(a b c d e f) :initial-value 'z)\n    '((((((z . a) . b) . c) . d) . e) . f))\n\n  (deftest reduce.clhs.1\n    (reduce #'list '(1 2 3 4) :initial-value 'foo)\n    '((((foo 1) 2) 3) 4)))\n\n(defsuite mapc\n\n  (deftest mapc.1\n    (signals-error\n     (mapc)\n     match-error))\n\n  (deftest mapc.2\n    (signals-error\n     (mapc (lambda (x) x))\n     match-error))\n\n  (deftest mapc.3\n    (signals-error\n     (mapc \"foo\" '(1 2 3))\n     type-error :datum \"foo\" :expected-type 'operator))\n\n  (deftest mapc.4\n    (signals-error\n     (mapc (lambda (x) x) \"foo\")\n     type-error :datum \"foo\" :expected-type 'cons))\n\n  (deftest mapc.5\n    (let ((ct 0))\n      (def env (the-environment))\n      (mapc (lambda (x) (set env ct (+ ct x)))\n            '(1 2 3 4))\n      ct)\n    10)\n\n  (deftest mapc.6\n    (let ((list (list 1 2 3)))\n      (mapc (lambda (x)) list))\n    (list 1 2 3)))\n\n(defsuite mapcan\n\n  (deftest mapcan.1\n    (mapcar (lambda ()) ())\n    ())\n\n  (deftest mapcan.2\n    (mapcan #'list '(\"foo\" \"bar\"))\n    '(\"foo\" \"bar\"))\n\n  (deftest mapcan.3\n    (signals-error\n     (mapcan)\n     match-error))\n\n  (deftest mapcan.4\n    (signals-error\n     (mapcan #'list)\n     match-error))\n\n  (deftest mapcan.5\n    (signals-error\n     (mapcan #'list 12)\n     type-error :datum 12 :expected-type 'cons))\n\n  (deftest mapcan.6\n    (signals-error\n     (mapcan 12 '(1))\n     type-error :datum 12 :expected-type 'operator))\n\n  (deftest mapcan.7\n    (mapcan (lambda (x) (if (typep x #^number) (list x) #nil))\n            '(a 1 b c 3 4 d 5))\n    '(1 3 4 5))\n\n  (deftest mapcan.8\n    (mapcan (lambda (x) (list (+ x 10) 'x)) '(1 2 3 4))\n    '(11 x 12 x 13 x 14 x)))\n\n(defsuite dolist\n\n  (deftest dolist.1\n    (signals-error\n     (dolist)\n     match-error))\n\n  (deftest dolist.2\n    (signals-error\n     (dolist ())\n     match-error))\n\n  (deftest dolist.3\n    (signals-error\n     (dolist (x))\n     match-error))\n\n  (deftest dolist.4\n    (dolist (x '(1 2 3)))\n    #void)\n\n  (deftest dolist.5\n    (signals-error\n     (dolist (x \"not-a-list\"))\n     type-error :datum \"not-a-list\" :expected-type 'cons))\n\n  (deftest dolist.6\n    (signals-error\n     (dolist (\"not-a-symbol\" '(1 2 3)))\n     type-error\n     :datum \"not-a-symbol\"\n     :expected-type '(or symbol ignore list)))\n\n  (deftest dolist.7\n    (let ((ct 0))\n      (def env (the-environment))\n      (assert (= #void (dolist (x '(1 2 3 4))\n                         'bla\n                         (set env ct (+ ct x)))))\n      ct)\n    10)\n\n  (deftest dolist.8\n    (let ((ct 0))\n      (def env (the-environment))\n      (dolist (x '(1 2 3 4) (+ ct 100))\n        'bla\n        (set env ct (+ ct x))))\n    110)\n\n  (deftest dolist.9\n    (dolist (x '(1 2 3) x))\n    '()))\n\n(defsuite nth\n\n  (deftest nth.1\n    (signals-error\n     (nth)\n     match-error))\n\n  (deftest nth.2\n    (signals-error\n     (nth '())\n     match-error))\n\n  (deftest nth.3\n    (signals-error\n     (nth #t '(1 2))\n     type-error :datum #t :expected-type 'number))\n\n  (deftest nth.3a\n    (signals-error\n     (nth 1 '())\n     type-error :datum '() :expected-type 'cons))\n\n  (deftest nth.4\n    (signals-error\n     (nth 0 \"foo\")\n     type-error :datum \"foo\" :expected-type 'cons))\n\n  (deftest nth.5\n    (nth 0 '(1 2))\n    1)\n\n  (deftest nth.6\n    (nth 1 '(1 2))\n    2)\n\n  (deftest nth.7\n    (signals-error\n     (nth 2 '(1 2))\n     type-error :datum '() :expected-type 'cons)))\n\n(defsuite reverse\n\n  (deftest reverse.1\n    (signals-error\n     (reverse)\n     match-error))\n\n  (deftest reverse.2\n    (signals-error\n     (reverse \"foo\")\n     type-error :datum \"foo\" :expected-type 'list))\n\n  (deftest reverse.3\n    (signals-error\n     (reverse \"foo\" \"bar\")\n     match-error))\n\n  (deftest reverse.4\n    (reverse ())\n    ())\n\n  (deftest reverse.5\n    (reverse '(1))\n    '(1))\n\n  (deftest reverse.6\n    (reverse '(1 2 3))\n    '(3 2 1)))\n\n(defsuite append\n\n  (deftest append.1\n    (signals-error\n     (append)\n     match-error))\n\n  (deftest append.2\n    (signals-error\n     (append '())\n     match-error))\n\n  (deftest append.3\n    (signals-error\n     (append 1 '())\n     type-error :datum 1 :expected-type 'list))\n\n  (deftest append.4\n    (append '() '())\n    '())\n\n  (deftest append.5\n    (append '() 12)\n    12)\n\n  (deftest append.6\n    (append '(1) 12)\n    '(1 . 12))\n\n  (deftest append.7\n    (append '(1 2) 12)\n    '(1 2 . 12))\n\n  (deftest append.8\n    (append '(1 2) '(3 4))\n    '(1 2 3 4)))\n\n(defsuite length\n\n  (deftest length.1\n    (signals-error\n     (length)\n     error))\n\n  (deftest length.2\n    (signals-error\n     (length #f)\n     error)))\n\n(defsuite list-length\n\n  (deftest list-length.1\n    (length '())\n    0)\n\n  (deftest list-length.2\n    (length '(1 2 3))\n    3))\n\n(defsuite nthcdr\n\n  (deftest nthcdr.1\n    (signals-error\n     (nthcdr)\n     match-error))\n\n  (deftest nthcdr.2\n    (signals-error\n     (nthcdr 1)\n     match-error))\n\n  (deftest nthcdr.3\n    (signals-error\n     (nthcdr \"foo\" '())\n     type-error :datum \"foo\" :expected-type 'number))\n\n  (deftest nthcdr.4\n    (signals-error\n     (nthcdr 12 \"foo\")\n     type-error :datum \"foo\" :expected-type 'list))\n\n  (deftest nthcdr.5\n    (nthcdr 0 '())\n    '())\n\n  (deftest nthcdr.6\n    (signals-error\n     (nthcdr 1 '())\n     out-of-bounds-error))\n\n  (deftest nthcdr.7\n    (signals-error\n     (nthcdr 2 '(1))\n     out-of-bounds-error))\n\n  (deftest nthcdr.8\n    (signals-error\n     (nthcdr 3 '(1))\n     out-of-bounds-error))\n\n  (deftest nthcdr.9\n    (nthcdr 0 '(1 2 3))\n    '(1 2 3))\n\n  (deftest nthcdr.10\n    (nthcdr 1 '(1 2 3))\n    '(2 3))\n\n  (deftest nthcdr.11\n    (nthcdr 2 '(1 2 3))\n    '(3))\n\n  (deftest nthcdr.12\n    (nthcdr 3 '(1 2 3))\n    '())\n\n  (deftest nthcdr.13\n    (signals-error\n     (nthcdr 4 '(1 2 3))\n     out-of-bounds-error)))\n\n(defsuite subseq\n\n  (deftest subseq.1\n    (signals-error\n     (subseq)\n     error))\n\n  (deftest subseq.2\n    (signals-error\n     (subseq '())\n     match-error))\n\n  (deftest subseq.3\n    (signals-error\n     (subseq 12 1 2)\n     unbound-method-error :class #^number :method-name 'subseq)))\n\n(defsuite list-subseq\n\n  (deftest list-subseq.4\n    (signals-error\n     (subseq '() \"a\" 2)\n     type-error :datum \"a\" :expected-type 'number))\n\n  (deftest list-subseq.5\n    (signals-error\n     (subseq '() 1 \"b\")\n     type-error :datum \"b\" :expected-type 'number))\n\n  (deftest list-subseq.6\n    (signals-error\n     (subseq '() 1)\n     out-of-bounds-error))\n\n  (deftest list-subseq.7\n    (signals-error\n     (subseq '() 0 1)\n     out-of-bounds-error))\n\n  (deftest list-subseq.8\n    (subseq '() 0)\n    '())\n\n  (deftest list-subseq.9\n    (subseq '() 0 0)\n    '())\n\n  (deftest list-subseq.10\n    (subseq '() 0 #void)\n    '())\n\n  (deftest list-subseq.11\n    (subseq '(1 2 3) 0)\n    '(1 2 3))\n\n  (deftest list-subseq.12\n    (subseq '(1 2 3) 0 #void)\n    '(1 2 3))\n\n  (deftest list-subseq.13\n    (subseq '(0 1 2 3 4 5) 2)\n    '(2 3 4 5))\n\n  (deftest list-subseq.14\n    (subseq '(0 1 2 3 4 5) 3 5)\n    '(3 4))\n\n  (deftest list-subseq.15\n    (subseq '(0 1 2 3 4 5) 0 6)\n    '(0 1 2 3 4 5))\n\n  (deftest list-subseq.16\n    (signals-error\n     (subseq '(0 1 2 3 4 5) 0 7)\n     out-of-bounds-error))\n\n  (deftest list-subseq.17\n    (signals-error\n     (subseq '(0 1 2 3 4 5) 7 10)\n     out-of-bounds-error))\n\n  (deftest list-subseq.18\n    (subseq '(0 1 2 3 4 5) 6)\n    '()))\n\n(defsuite string-subseq\n\n  (deftest string-subseq.2\n    (signals-error\n     (subseq \"\")\n     match-error))\n\n  (deftest string-subseq.4\n    (signals-error\n     (subseq \"\" \"a\" 2)\n     type-error :datum \"a\" :expected-type 'number))\n\n  (deftest string-subseq.5\n    (signals-error\n     (subseq \"foo\" 1 \"b\")\n     type-error :datum \"b\" :expected-type 'number))\n\n  (deftest string-subseq.6\n    (signals-error\n     (subseq \"\" 1)\n     out-of-bounds-error))\n\n  (deftest string-subseq.7\n    (signals-error\n     (subseq \"\" 0 1)\n     out-of-bounds-error))\n\n  (deftest string-subseq.8\n    (subseq \"\" 0)\n    \"\")\n\n  (deftest string-subseq.9\n    (subseq \"\" 0 0)\n    \"\")\n\n  (deftest string-subseq.10\n    (subseq \"\" 0 #void)\n    \"\")\n\n  (deftest string-subseq.10.1\n    (signals-error\n     (subseq \"\" 1 #void)\n     out-of-bounds-error))\n\n  (deftest string-subseq.11\n    (subseq \"123\" 0)\n    \"123\")\n\n  (deftest string-subseq.12\n    (subseq \"123\" 0 #void)\n    \"123\")\n\n  (deftest string-subseq.13\n    (subseq \"012345\" 2)\n    \"2345\")\n\n  (deftest string-subseq.14\n    (subseq \"012345\" 3 5)\n    \"34\")\n\n  (deftest string-subseq.15\n    (subseq \"012345\" 0 6)\n    \"012345\")\n\n  (deftest string-subseq.16\n    (signals-error\n     (subseq \"012345\" 0 7)\n     out-of-bounds-error))\n\n  (deftest string-subseq.17\n    (signals-error\n     (subseq \"012345\" 7 10)\n     out-of-bounds-error))\n\n  (deftest string-subseq.18\n    (subseq \"012345\" 6)\n     \"\"))\n\n(defsuite elt\n\n  (deftest elt.1\n    (signals-error\n     (elt)\n     error))\n\n  (deftest elt.2\n    (signals-error\n     (elt #t 0)\n     error)))\n\n(defsuite list-elt\n\n  (deftest list-elt.2\n    (signals-error\n     (elt '())\n     match-error))\n\n  (deftest list-elt.3a\n    (signals-error\n     (elt '() 1)\n     type-error :datum '() :expected-type 'cons))\n\n  (deftest list-elt.5\n    (elt '(1 2) 0)\n    1)\n\n  (deftest list-elt.6\n    (elt '(1 2) 1)\n    2)\n\n  (deftest list-elt.7\n    (signals-error\n     (elt '(1 2) 2)\n     type-error :datum '() :expected-type 'cons)))\n\n(defsuite member\n\n  (deftest member.1\n    (signals-error\n     (member)\n     match-error))\n\n  (deftest member.2\n    (signals-error\n     (member 'a)\n     match-error))\n\n  (deftest member.3\n    (member 'a '() :test #'eq)\n    #nil)\n\n  (deftest member.3a\n    (member 'a '())\n    #nil)\n\n  (deftest member.4\n    (member 'b '(a) :test #'eq)\n    #nil)\n\n  (deftest member.5\n    (member 'b '(a b) :test #'eq)\n    '(b))\n\n  (deftest member.5b\n    (member  'x '(a b) :test #'eq)\n    #nil)\n\n  (deftest member.6\n    (member 'b '(a b c) :test #'eq)\n    '(b c))\n\n  (deftest member.6a\n    (member 'b '(a b c))\n    '(b c))\n\n  (deftest member.7\n    (member \"b\" '(\"a\" \"b\" \"c\") :test #'eq)\n    #nil)\n\n  (deftest member.7a\n    (member \"b\" '(\"a\" \"b\" \"c\"))\n    #nil)\n\n  (deftest member.8\n    (member \"b\" '(\"a\" \"b\" \"c\") :test #'=)\n    '(\"b\" \"c\"))\n\n  (deftest member.9\n    (member 'b '((2 a) (1 b) (3 c)) :key #'cadr)\n    '((1 b) (3 c)))\n\n  (deftest member.9a\n    (member \"b\" '((2 \"a\") (1 \"b\") (3 \"c\")) :key #'cadr)\n    #nil)\n\n  (deftest member.9b\n    (member \"b\" '((2 \"a\") (1 \"b\") (3 \"c\")) :key #'cadr :test #'=)\n    '((1 \"b\") (3 \"c\"))))\n\n(defsuite remove-if\n\n  (deftest remove-if.1\n    (signals-error\n     (remove-if)\n     match-error))\n\n  (deftest remove-if.2\n    (signals-error\n     (remove-if (lambda (item) #t))\n     match-error))\n\n  (deftest remove-if.3\n    (signals-error\n     (remove-if (lambda (item) #t) 12)\n     type-error :datum 12 :expected-type 'cons))\n\n  (deftest remove-if.4\n    (signals-error\n     (remove-if 1 '(1 2))\n     type-error :datum 1 :expected-type 'operator))\n\n  (deftest remove-if.5\n    (remove-if (lambda (item) (= 1 item)) '(1 2 3 1 2 3))\n    '(2 3 2 3))\n\n  (deftest remove-if.6\n    (remove-if (lambda (item) (= 1 item)) '())\n    '()))\n\n(defsuite get?\n\n  (deftest get?.1\n    (get? '(:foo 1 :bar 2) :foo)\n    (some 1))\n\n  (deftest get?.2\n    (get? '(:foo 1 :bar 2) :bar)\n    (some 2))\n\n  (deftest get?.3\n    (get? '() :bar)\n    #nil)\n\n  (deftest get?.4\n    (get? '(:foo 1 :bar 2) :quux)\n    #nil)\n\n  (deftest get?.5\n    (signals-error\n     (get?)\n     match-error)\n    #t)\n\n  (deftest get?.6\n    (signals-error\n     (get? '())\n     match-error)\n    #t)\n\n  (deftest get?.7\n    (signals-error\n     (get? 12 :quux)\n     match-error)\n    #t))\n\n(defsuite and\n\n  (deftest and.1\n    (and)\n    #t)\n\n  (deftest and.2\n    (and #t #f)\n    #f)\n\n  (deftest and.3\n    (and #t (= 1 1) #t)\n    #t)\n\n  (deftest and.4\n    (signals-error\n     (and #t #t 1)\n     type-error :datum 1 :expected-type 'boolean))\n\n  (deftest and.5\n    (progn\n      (def x 1)\n      (def env (the-environment))\n      (and #f (set env x 2))\n      x)\n    1))\n\n(defsuite or\n\n  (deftest or.1\n    (or)\n    #f)\n\n  (deftest or.2\n    (or #t #f)\n    #t)\n\n  (deftest or.3\n    (or #t #t #t)\n    #t)\n\n  (deftest or.4\n    (or #f #f #f)\n    #f)\n\n  (deftest or.5\n    (signals-error\n     (or #f #f 1)\n     type-error :datum 1 :expected-type 'boolean))\n\n  (deftest or.6\n    (progn\n      (def x 1)\n      (def env (the-environment))\n      (or #t (set env x 2))\n      x)\n    1))\n\n(defsuite make-instance\n\n  (deftest make-instance.1\n    (progn\n      (def obj (make-instance #^type-error :datum 1 :expected-type 'string))\n      (assert (= (slot-value obj 'datum) 1))\n      (assert (= (slot-value obj :datum) 1))\n      (assert (= (slot-value obj 'expected-type) 'string))\n      (assert (= (slot-value obj :expected-type) 'string)))\n    #void)\n\n  (deftest make-instance.2\n    (signals-error\n     (make-instance)\n     match-error))\n\n  (deftest make-instance.3\n    (signals-error\n     (make-instance 3)\n     type-error :datum 3 :expected-type 'standard-class))\n\n  (deftest make-instance.4\n    (signals-error\n     (make-instance #^object)\n     type-error :datum #^object :expected-type 'standard-class))\n\n  (deftest make-instance.5\n    (signals-error\n     (make-instance #^standard-object :x)\n     assertion-error)))\n\n(defsuite slots\n\n  (deftest slots.1\n    (progn\n      (def obj (make-instance #^standard-object))\n      (assert (not (slot-bound-p obj 'x)))\n      (assert (not (slot-bound-p obj :x)))\n      (assert (signals-error (slot-value obj 'x)\n                             unbound-slot-error :object obj :slot-name 'x))\n      (assert (signals-error (slot-value obj :x)\n                             unbound-slot-error :object obj :slot-name :x))\n      (set-slot-value obj 'x 1)\n      (assert (slot-bound-p obj 'x))\n      (assert (slot-bound-p obj :x))\n      (assert (= (slot-value obj 'x) 1))\n      (assert (= (slot-value obj :x) 1))\n      (set-slot-value obj 'x 2)\n      (assert (= (slot-value obj 'x) 2))\n      (assert (= (slot-value obj :x) 2)))\n    #void)\n\n  (deftest slot-value.1\n    (signals-error\n     (slot-value \"foo\" 'x)\n     type-error :datum \"foo\" :expected-type 'standard-object))\n\n  (deftest slot-value.1a\n    (signals-error\n     (slot-value (make-instance #^standard-object) \"x\")\n     type-error :datum \"x\" :expected-type 'symbol))\n\n  (deftest slot-value.2\n    (signals-error\n     (slot-value (make-instance #^standard-object))\n     match-error))\n\n  (deftest slot-value.3\n    (signals-error\n     (slot-value)\n     match-error))\n\n  (deftest set-slot-value.1\n    (signals-error\n     (set-slot-value \"foo\" 'x 1)\n     type-error :datum \"foo\" :expected-type 'standard-object))\n\n  (deftest set-slot-value.1a\n    (signals-error\n     (set-slot-value (make-instance #^standard-object) \"x\" 1)\n     type-error :datum \"x\" :expected-type 'symbol))\n\n  (deftest set-slot-value.2\n    (signals-error\n     (set-slot-value (make-instance #^standard-object))\n     match-error))\n\n  (deftest set-slot-value.3\n    (signals-error\n     (set-slot-value (make-instance #^standard-object) 'x)\n     match-error))\n\n  (deftest slot-bound-p.1\n    (signals-error\n     (slot-bound-p \"foo\" 'x)\n     type-error :datum \"foo\" :expected-type 'standard-object))\n\n  (deftest slot-bound-p.1a\n    (signals-error\n     (slot-bound-p (make-instance #^standard-object) \"x\")\n     type-error :datum \"x\" :expected-type 'symbol))\n\n  (deftest slot-bound-p.2\n    (signals-error\n     (slot-bound-p)\n     match-error))\n\n  (deftest slot-bound-p.3\n    (signals-error\n     (slot-bound-p (make-instance #^standard-object))\n     match-error)))\n\n(defsuite let\n\n  (deftest let.1\n    (let ((x 1))\n      x)\n    1)\n\n  (deftest let.2\n    (let ((x 1))\n      (let ((x 2))\n        x))\n    2)\n\n  (deftest let.3\n    (let ((x 1))\n      (let ((x 2)))\n      x)\n    1)\n\n  (deftest let.4\n    (let ((x 1))\n      (let ((x 2)\n            (it x))\n        it))\n    1)\n\n  (deftest let.5\n    (let ())\n    #void)\n\n  (deftest let.6\n    (signals-error\n     (let)\n     match-error))\n\n  (deftest let.7\n    (signals-error\n     (let #t)\n     type-error :datum #t :expected-type 'cons))\n\n  (deftest let.8\n    (signals-error\n     (let ((#t 1)))\n     type-error :datum #t :expected-type '(or symbol ignore list))))\n\n(defsuite let*\n\n  (deftest let*.1\n    (let* ((x 1)\n           (y x)\n           (z y))\n      (list x y z))\n    '(1 1 1))\n\n  (deftest let*.2\n    (let* ())\n    #void)\n\n  (deftest let*.3\n    (signals-error\n     (let*)\n     match-error))\n\n  (deftest let*.4\n    (signals-error\n     (let* #t)\n     type-error :datum #t :expected-type 'cons))\n\n  (deftest let*.5\n    (signals-error\n     (let* ((#t 1)))\n     type-error :datum #t :expected-type '(or symbol ignore list))))\n\n(defsuite flet\n\n  (deftest flet.1\n    (flet ((f () 1))\n          (flet ((f () 2)\n                 (g () (f)))\n                (g)))\n    1)\n\n  (deftest flet.2\n    (flet ((foo ()))\n          (foo))\n    #void)\n\n  (deftest flet.2\n    (flet ((foo (x) x))\n          (foo 1))\n    1)\n\n  (deftest flet.3\n    (signals-error\n     (flet)\n     match-error))\n\n  (deftest flet.4\n    (signals-error\n     (flet #t)\n     type-error :datum #t :expected-type 'cons))\n\n  (deftest flet.5\n    (flet ())\n    #void))\n\n(defsuite labels\n\n  (deftest labels.1\n    (labels ((f () 1))\n            (labels ((f () 2)\n                     (g () (f)))\n                    (g)))\n    2)\n\n  (deftest labels.2\n    (labels ((foo ()))\n            (foo))\n    #void)\n\n  (deftest labels.3\n    (labels ((foo (x) x))\n            (foo 1))\n    1)\n\n  (deftest labels.4\n    (signals-error\n     (labels)\n     match-error))\n\n  (deftest labels.5\n    (signals-error\n     (labels #t)\n     type-error :datum #t :expected-type 'cons))\n\n  (deftest labels.6\n    (labels ())\n    #void))\n\n(defsuite loop-let\n\n  (deftest loop-let.1\n    (signals-error\n     (loop-let)\n     match-error))\n\n  (deftest loop-let.2\n    (signals-error\n     (loop-let -name-)\n     match-error))\n\n  (deftest loop-let.3\n    (loop-let -name- ())\n    #void)\n\n  (deftest loop-let.4\n    (loop-let -sum- ((as (list 1 2 3))\n                     (bs (list 4 5 6)))\n      (if (null as)\n          #nil\n          (cons (+ (car as) (car bs))\n                (-sum- (cdr as) (cdr bs)))))\n    '(5 7 9)))\n\n(defsuite while\n\n  (deftest while.1\n    (signals-error\n     (while)\n     match-error))\n\n  (deftest while.2\n    (while (= 1 2))\n    #void)\n\n  (deftest while.3\n    (signals-error\n     (while 12)\n     type-error :datum 12 :expected-type 'boolean))\n\n  (deftest while.4\n    (progn\n      (def x 0)\n      (def y 0)\n      (def env (the-environment))\n      (while (< x 5)\n        (set env x (+ x 1))\n        (set env y (+ y 1)))\n      (list x y))\n    '(5 5)))\n\n(defsuite until\n\n  (deftest until.1\n    (signals-error\n     (until)\n     match-error))\n\n  (deftest until.2\n    (until (/= 1 2))\n    #void)\n\n  (deftest until.3\n    (signals-error\n     (until 12)\n     type-error :datum 12 :expected-type 'boolean))\n\n  (deftest until.4\n    (progn\n      (def x 0)\n      (def y 0)\n      (def env (the-environment))\n      (until (> x 4)\n        (set env x (+ x 1))\n        (set env y (+ y 1)))\n      (list x y))\n    '(5 5)))\n\n(defsuite dotimes\n\n  (deftest dotimes.1\n    (dotimes (temp-one 10 temp-one))\n    10)\n\n  (deftest dotimes.2\n    (progn\n      (def temp-two 0)\n      (def env (the-environment))\n      (dotimes (temp-one 10 #t) (set env temp-two (+ temp-two 1)))\n      temp-two)\n    10)\n\n  (deftest dotimes.3\n    (signals-error\n     (dotimes)\n     match-error))\n\n  (deftest dotimes.4\n    (signals-error\n     (dotimes (x))\n     match-error))\n\n  (deftest dotimes.5\n    (dotimes (x 0))\n    #void)\n\n  (deftest dotimes.6\n    (dotimes (x 1))\n    #void)\n\n  (deftest dotimes.7\n    (dotimes (x 1) 1 2 3)\n    #void)\n\n  (deftest dotimes.8\n    (signals-error\n     (dotimes (x \"foo\") 1 2 3)\n     type-error :datum \"foo\" :expected-type 'number))\n\n  (deftest dotimes.9\n    (progn\n      (def x 0)\n      (def y 0)\n      (def env (the-environment))\n      (dotimes (i 5 (list x y))\n        (set env x (+ x 1))\n        (set env y (+ y 1))))\n    '(5 5)))\n\n(defsuite optional\n\n  (deftest optional.1\n    (optional '())\n    #void)\n\n  (deftest optional.2\n    (optional '() \"default\")\n    \"default\")\n\n  (deftest optional.3\n    (optional (some 1))\n    1)\n\n  (deftest optional.4\n    (optional (some 1) \"default\")\n    1)\n\n  (deftest optional.5\n    (signals-error\n     (optional)\n     match-error))\n\n  (deftest optional.6\n    (signals-error\n     (optional 1)\n     type-error :datum 1 :expected-type 'cons))\n\n  (deftest optional.lazy.1\n    (optional (some 1) (error (make-simple-error \"foo\")))\n    1)\n\n  (deftest optional.lazy.2\n    (signals-error\n     (optional #nil (error (make-simple-error \"foo\")))\n     simple-error :message \"foo\")))\n\n(defsuite optionals\n\n  (deftest optionals.1\n    (optionals '())\n    '())\n\n  (deftest optionals.2\n    (optionals '() 1)\n    '(1))\n\n  (deftest optionals.3\n    (optionals '() 1 2)\n    '(1 2))\n\n  (deftest optionals.4\n    (optionals '(4) 1 2)\n    '(4 2))\n\n  (deftest optionals.5\n    (optionals '(4 5) 1 2)\n    '(4 5))\n\n  (deftest optionals.6\n    (optionals '(4 5 6) 1 2)\n    '(4 5 6))\n\n  (deftest optionals.7\n    (optionals '(4 5 6))\n    '(4 5 6)))\n\n(defsuite get-option\n\n  (deftest get-option.1\n    (signals-error\n     (get-option)\n     match-error))\n\n  (deftest get-option.2\n    (get-option (some 2))\n    2)\n\n  (deftest get-option.3\n    (signals-error\n     (get-option #nil)\n     simple-error :message \"Option is nil\")))\n\n(defsuite some\n\n  (deftest some.1\n    (some 1)\n    (list 1))\n\n  (deftest some.2\n    (signals-error\n     (some)\n     match-error))\n\n  (deftest some.3\n    (signals-error\n     (some 1 2)\n     match-error)))\n\n(defsuite if-option\n\n  (deftest if-option.1\n    (if-option (x (some 12))\n               x\n               14)\n    12)\n\n  (deftest if-option.2\n    (if-option (x #nil)\n               x\n               14)\n    14)\n\n  (deftest if-option.3\n    (signals-error\n     (if-option)\n     match-error))\n\n  (deftest if-option.4\n    (signals-error\n     (if-option (name))\n     match-error))\n\n  (deftest if-option.4.2\n    (signals-error\n     (if-option (name) then)\n     match-error))\n\n  (deftest if-option.5\n    (signals-error\n     (if-option (name option))\n     match-error))\n\n  (deftest if-option.6\n    (signals-error\n     (if-option (name option) then)\n     match-error)))\n\n(defsuite when-option\n\n  (deftest when-option.0\n    (when-option (x (some 12)))\n    #void)\n\n  (deftest when-option.1\n    (when-option (x (some 12))\n                 x)\n    12)\n\n  (deftest when-option.2\n    (when-option (x #nil)\n                 x)\n    #nil)\n\n  (deftest when-option.3\n    (signals-error\n     (when-option)\n     match-error))\n\n  (deftest when-option.4\n    (signals-error\n     (when-option (name))\n     match-error)))\n\n(defsuite unless-option\n\n  (deftest unless-option.0\n    (unless-option (some 12) 2 3 (+ 2 2))\n    #nil)\n\n  (deftest unless-option.1\n    (unless-option #nil 2 3 (+ 2 2))\n    4)\n\n  (deftest unless-option.2\n    (unless-option #nil)\n    #void)\n\n  (deftest unless-option.3\n    (signals-error\n     (unless-option)\n     match-error)))\n\n(defsuite class-of\n\n  (deftest class-of.1\n    (progn\n      (assert (eq (class-of \"foo\") #^string))\n      (assert (eq (class-of 12) #^number))\n      (assert (eq (class-of #^string) #^built-in-class)))\n    #void)\n\n  (deftest class-of.2\n    (signals-error\n     (class-of)\n     match-error)))\n\n(defsuite set\n\n  (deftest set.1\n    (progn\n      (set (the-environment) x 1)\n      x)\n    1)\n\n  (deftest set.2\n    (progn\n      (def x 0)\n      (set (the-environment) x 1)\n      x)\n    1)\n\n  (deftest set.3\n    (progn\n      (set (the-environment) (x y) (list 1 2))\n      (list x y))\n    '(1 2))\n\n  (deftest set.4\n    (signals-error\n     (set)\n     match-error))\n\n  (deftest set.5\n    (signals-error\n     (set (the-environment))\n     match-error))\n\n  (deftest set.6\n    (signals-error\n     (set (the-environment) x)\n     match-error))\n\n  (deftest set.7\n    (signals-error\n     (set 33 x y)\n     type-error :datum 33 :expected-type 'environment)))\n\n(defsuite class-name\n\n  (deftest class-name.1\n    (signals-error\n     (class-name)\n     match-error))\n\n  (deftest class-name.2\n    (signals-error\n     (class-name \"foo\")\n     type-error :datum \"foo\" :expected-type 'class))\n\n  (deftest class-name.3\n    (class-name #^object)\n    'object))\n\n(defsuite subclassp\n\n  (deftest subclassp.1\n    (signals-error\n     (subclassp)\n     match-error))\n\n  (deftest subclassp.2\n    (signals-error\n     (subclassp #^object)\n     match-error))\n\n  (deftest subclassp.3\n    (signals-error\n     (subclassp #^object \"foo\")\n     type-error :datum \"foo\" :expected-type 'class))\n\n  (deftest subclassp.4\n    (signals-error\n     (subclassp \"foo\" #^object)\n     type-error :datum \"foo\" :expected-type 'class))\n\n  (deftest subclassp.5\n    (and (subclassp #^object #^object)\n         (subclassp #^class #^object)\n         (subclassp #^standard-class #^class)\n         (subclassp #^standard-class #^object))\n    #t))\n\n;; Utility used by `%make-standard-class' and `defclass' tests.\n\n(defun classes-test (c d p q)\n  (assert (typep c #^standard-class))\n  (assert (typep c #^class))\n  (assert (typep c #^object))\n  (assert (= (class-name c) 'point-2d))\n  (assert (subclassp c #^standard-object))\n  (assert (subclassp c #^object))\n\n  (assert (typep p c))\n  (assert (typep p #^standard-object))\n  (assert (typep p #^object))\n  (assert (= (slot-value p 'x) 1))\n  (assert (= (slot-value p 'y) 2))\n\n  (assert (typep d #^standard-class))\n  (assert (typep d #^class))\n  (assert (typep d #^object))\n  (assert (= (class-name d) 'point-3d))\n  (assert (subclassp d c))\n  (assert (subclassp d #^standard-object))\n  (assert (subclassp d #^object))\n\n  (assert (typep q d))\n  (assert (typep q c))\n  (assert (typep q #^standard-object))\n  (assert (typep q #^object))\n  (assert (= (slot-value q 'x) 1))\n  (assert (= (slot-value q 'y) 2))\n  (assert (= (slot-value q 'z) 3)))\n\n(defsuite %make-standard-class\n\n  (deftest %make-standard-class.1\n    (let* ((c (%make-standard-class 'point-2d #^standard-object))\n           (d (%make-standard-class 'point-3d c))\n           (p (make-instance c :x 1 :y 2))\n           (q (make-instance d :x 1 :y 2 :z 3)))\n      (classes-test c d p q))\n    #void)\n\n  (deftest %make-standard-class.2\n    (signals-error\n     (%make-standard-class 'foo #^object)\n     type-error :datum #^object :expected-type 'standard-class))\n\n  #|\n  (deftest %make-standard-class.3\n    (signals-error\n     (%make-standard-class 'foo)\n     match-error))\n\n  (deftest %make-standard-class.4\n    (signals-error\n     (%make-standard-class)\n     match-error))\n  |#\n\n  (deftest %make-standard-class.5\n    (signals-error\n     (%make-standard-class \"foo\" #^standard-object)\n     type-error :datum \"foo\" :expected-type 'symbol)))\n\n(defsuite %reinitialize-standard-class\n\n  (deftest* %reinitialize-standard-class.1\n    (let* ((a (%make-standard-class 'a #^standard-object))\n           (b (%make-standard-class 'b a))\n           (c (%make-standard-class 'c b))\n           (d (%make-standard-class 'd #^standard-object))\n           (e (%make-standard-class 'e d)))\n      (assert (subclassp b a))\n      (assert (subclassp c a))\n      (assert (subclassp c b))\n      (%reinitialize-standard-class b e)\n      (assert (subclassp b e))\n      (assert (subclassp b d))\n      (assert (subclassp c e))\n      (assert (subclassp c d))\n      (assert (subclassp c b)))))\n\n(defsuite defclass\n\n  (deftest defclass.1\n    (signals-error\n     (defclass foo () (\"a\"))\n     type-error :datum \"a\" :expected-type 'symbol))\n\n  (deftest defclass.2\n    (signals-error\n     (defclass)\n     match-error))\n\n  (deftest defclass.3\n    (signals-error\n     (defclass foo)\n     match-error))\n\n  (deftest defclass.4\n    (signals-error\n     (defclass foo ())\n     match-error))\n\n  (deftest defclass.5\n    (typep (defclass c () ()) #^standard-class))\n\n  (deftest defclass.6\n    (progn\n      (defclass point-2d () (x y))\n      (defclass point-3d (point-2d) (z))\n      (def p (make-instance #^point-2d :x 1 :y 2))\n      (def q (make-instance #^point-3d :x 1 :y 2 :z 3))\n      (classes-test #^point-2d #^point-3d p q))\n    #void)\n\n  (deftest* defclass.redefine.1\n    (defclass a () ())\n    (defclass b (a) ())\n    (defgeneric foo (x))\n    (defmethod foo ((a a)) \"foo\")\n    (def b (make-instance #^b))\n    (assert (subclassp #^b #^a))\n    (assert (typep b #^b))\n    (assert (typep b #^a))\n    (assert (= \"foo\" (foo b)))\n    (defclass c () ())\n    (defmethod foo ((c c)) \"bar\")\n    (defclass b (c) ())\n    (assert (subclassp #^b #^c))\n    (assert (eq (class-of b) #^b))\n    (assert (typep b #^b))\n    (assert (typep b #^c))\n    (assert (= \"bar\" (foo b)))))\n\n(defsuite generics\n\n  (deftest generics.1\n    (progn\n      (defclass foo () ())\n      (defclass bar (foo) ())\n      (def foo (make-instance #^foo))\n      (def bar (make-instance #^bar))\n\n      (defgeneric g1 (obj param))\n      (assert (typep #'g1 #^function))\n\n      (assert (signals-error\n               (g1 12 (+ 1 1))\n               unbound-method-error :class #^number :method-name 'g1))\n      (assert (signals-error\n               (g1 foo (+ 1 1))\n               unbound-method-error :class #^foo :method-name 'g1))\n      (assert (signals-error\n               (g1 bar (+ 1 1))\n               unbound-method-error :class #^bar :method-name 'g1))\n\n      (defmethod g1 ((obj foo) param)\n        (+ param 100))\n\n      (assert (signals-error\n               (g1 12 (+ 1 1))\n               unbound-method-error :class #^number :method-name 'g1))\n      (assert (= 102 (g1 foo (+ 1 1))))\n      (assert (= 102 (g1 bar (+ 1 1))))\n\n      (defmethod g1 ((obj bar) param)\n        (+ param 1000))\n\n      (assert (signals-error\n               (g1 12 (+ 1 1))\n               unbound-method-error :class #^number :method-name 'g1))\n      (assert (= 102 (g1 foo (+ 1 1))))\n      (assert (= 1002 (g1 bar (+ 1 1))))\n\n      (defmethod g1 ((obj number) param)\n        (+ param 10000))\n\n      (assert (= 10002 (g1 12 (+ 1 1))))\n      (assert (= 102 (g1 foo (+ 1 1))))\n      (assert (= 1002 (g1 bar (+ 1 1))))\n\n      (defmethod g1 ((obj number) param)\n        (+ param 100000))\n\n      (assert (= 100002 (g1 12 (+ 1 1))))\n      (assert (= 102 (g1 foo (+ 1 1))))\n      (assert (= 1002 (g1 bar (+ 1 1))))\n\n      (defmethod g1 ((obj object) param)\n        (- param))\n\n      (assert (= 100002 (g1 12 (+ 1 1))))\n      (assert (= 102 (g1 foo (+ 1 1))))\n      (assert (= 1002 (g1 bar (+ 1 1))))\n      (assert (= -2 (g1 #t (+ 1 1))))\n      (assert (= -2 (g1 #void (+ 1 1))))\n      (assert (= -2 (g1 \"hello\" (+ 1 1)))))\n    #void))\n\n(defsuite box\n\n  (deftest box.1\n    (let ((#'c (box)))\n      (c))\n    #void)\n\n  (deftest box.2\n    (let ((#'c (box (+ 1 1))))\n      (c))\n    2)\n\n  (deftest box.3\n    (let ((#'c (box (+ 1 1))))\n      (c (+ 2 2))\n      (assert (= (c) 4))\n      (c (+ 4 4))\n      (c))\n    8))\n");
;// CONCATENATED MODULE: ./test/cond-sys-test.lispx
/* harmony default export */ const cond_sys_test_lispx = ("(defsuite handler-bind\n\n  (defclass test-error (error) ())\n\n  (deftest handler-bind.0\n    (signals-error\n     (handler-bind)\n     match-error))\n\n  (deftest handler-bind.0b\n    (handler-bind ())\n    #void)\n\n  (deftest handler-bind.1\n    (handler-bind ()\n      1 2 (+ 1 2))\n    3)\n\n  (deftest handler-bind.2\n    (handler-bind ((simple-error (lambda (e) 'whatever)))\n      1 2 (+ 1 2))\n    3)\n\n  (deftest handler-bind.3\n    (handler-bind ((simple-error (lambda (e) 'whatever))\n                   (test-error (lambda (e) 'whatever)))\n      1 2 (+ 1 2))\n    3)\n\n  (deftest handler-bind.no-handler\n    (handler-bind ((test-error (lambda (e) 'whatever)))\n      (signal (make-simple-error \"foo\")))\n    #void)\n\n  (deftest handler-bind.unhandled\n    (handler-bind ((simple-error (lambda (e) 'whatever)))\n      (signal (make-simple-error \"foo\")))\n    #void)\n\n  (deftest handler-bind.decline\n    (block b\n      (handler-bind ((simple-error (lambda (e) (return-from b 1))))\n        (handler-bind ((simple-error (lambda (e) 'decline)))\n          (signal (make-simple-error \"foo\")))))\n    1)\n\n  (deftest handler-bind.handled\n    (block b\n      (handler-bind ((simple-error (lambda (e)\n                                     (assert (= \"foo\" (slot-value e 'message)))\n                                     (return-from b 1))))\n        (signal (make-simple-error \"foo\"))))\n    1)\n\n  (deftest handler-bind.nested\n    (block b\n      (handler-bind ((simple-error (lambda (e) (return-from b 'outer))))\n        (handler-bind ((simple-error (lambda (e) (return-from b 'inner))))\n          (signal (make-simple-error \"foo\")))))\n    'inner)\n\n  (deftest handler-bind.resignal\n    (block b\n      (handler-bind ((simple-error (lambda (e) (return-from b 'outer))))\n        (handler-bind ((simple-error (lambda (e) (signal e))))\n          (signal (make-simple-error \"foo\")))))\n    'outer)\n\n  (deftest handler-bind.firewall\n    (block b\n      (handler-bind ((test-error (lambda (e) (return-from b 'outer))))\n        (handler-bind ((simple-error (lambda (e)\n                                       (signal (make-instance #^test-error)))))\n          (handler-bind ((test-error (lambda (e) (return-from b 'inner))))\n            (signal (make-simple-error \"foo\"))))))\n    'outer)\n\n  (deftest handler-bind.multiple.1\n    (block b\n      (handler-bind ((test-error (lambda (e) (return-from b 'test)))\n                     (simple-error (lambda (e) (return-from b 'simple))))\n        (signal (make-simple-error \"foo\"))))\n    'simple)\n\n  (deftest handler-bind.multiple.2\n    (block b\n      (handler-bind ((test-error (lambda (e) (return-from b 'test)))\n                     (simple-error (lambda (e) (return-from b 'simple))))\n        (signal (make-instance #^test-error))))\n    'test)\n\n  (deftest handler-bind.anything\n    (block b\n      (handler-bind ((object (lambda (e) (return-from b e))))\n        (signal \"foo\")))\n    \"foo\"))\n\n(defsuite handler-case\n\n  (defclass test-error (error) ())\n\n  (deftest handler-case.0\n    (signals-error\n     (handler-case)\n     match-error))\n\n  (deftest handler-case.0b\n    (handler-case ())\n    #void)\n\n  (deftest handler-case.1\n    (handler-case ()\n      1 2 (+ 1 2))\n    3)\n\n  (deftest handler-case.2\n    (handler-case ((simple-error (lambda (e) 'whatever)))\n      1 2 (+ 1 2))\n    3)\n\n  (deftest handler-case.3\n    (handler-case ((simple-error (lambda (e) 'whatever))\n                   (test-error (lambda (e) 'whatever)))\n      1 2 (+ 1 2))\n    3)\n\n  (deftest handler-case.no-handler\n    (handler-case ((test-error (lambda (e) 'whatever)))\n      (signal (make-simple-error \"foo\")))\n    #void)\n\n  (deftest handler-case.handled\n    (handler-case ((simple-error (lambda (e)\n                                   (assert (= \"foo\" (slot-value e 'message)))\n                                   'whatever)))\n      (signal (make-simple-error \"foo\")))\n    'whatever)\n\n  (deftest handler-case.nested\n    (handler-case ((simple-error (lambda (e) 'outer)))\n      (handler-case ((simple-error (lambda (e) 'inner)))\n        (signal (make-simple-error \"foo\"))))\n    'inner)\n\n  (deftest handler-case.resignal\n    (handler-case ((simple-error (lambda (e) 'outer)))\n      (handler-case ((simple-error (lambda (e) (signal e))))\n        (signal (make-simple-error \"foo\"))))\n    'outer)\n\n  (deftest handler-case.multiple.1\n    (handler-case ((test-error (lambda (e) 'test))\n                   (simple-error (lambda (e) 'simple)))\n      (signal (make-simple-error \"foo\")))\n    'simple)\n\n  (deftest handler-case.multiple.2\n    (handler-case ((test-error (lambda (e) 'test))\n                   (simple-error (lambda (e) 'simple)))\n      (signal (make-instance #^test-error)))\n    'test)\n\n  (deftest handler-case.anything\n    (handler-case ((object (lambda (e) e)))\n      (signal \"foo\"))\n    \"foo\"))\n\n(defsuite restart-bind\n\n  (deftest restart-bind.0\n    (signals-error\n     (restart-bind)\n     match-error))\n\n  (deftest restart-bind.0b\n    (restart-bind ())\n    #void)\n\n  (deftest restart-bind.1\n    (restart-bind ()\n      1 2 (+ 1 2))\n    3)\n\n  (deftest restart-bind.2\n    (restart-bind ((continue (lambda () 'whatever)))\n      1 2 (+ 1 2))\n    3)\n\n  (deftest restart-bind.3\n    (restart-bind ((continue (lambda () 'whatever))\n                   (abort (lambda () 'whatever)))\n      1 2 (+ 1 2))\n    3)\n\n  (deftest restart-bind.no-handler.1\n    (signals-error\n     (invoke-restart 'continue)\n     restart-error :restart-name 'continue))\n\n  (deftest restart-bind.no-handler.2\n    (signals-error\n     (restart-bind ((continue (lambda () 'whatever)))\n       (invoke-restart 'abort))\n     restart-error :restart-name 'abort))\n\n  (deftest restart-bind.no-handler.1a\n    (signals-error\n     (invoke-restart-interactively 'continue)\n     restart-error :restart-name 'continue))\n\n  (deftest restart-bind.no-handler.2a\n    (signals-error\n     (restart-bind ((continue (lambda () 'whatever)))\n       (invoke-restart-interactively 'abort))\n     restart-error :restart-name 'abort))\n\n  (deftest restart-bind.no-exit.1\n    (restart-bind ((continue (lambda () 'whatever)))\n      (invoke-restart 'continue))\n    'whatever)\n\n  (deftest restart-bind.no-exit.2\n    (restart-bind ((continue (lambda () 'whatever)))\n      (if-option (restart-handler (find-restart? 'continue))\n        (invoke-restart restart-handler)\n        (assert #f)))\n    'whatever)\n\n  (deftest restart-bind.handled\n    (block b\n      (restart-bind ((abort (lambda (arg1 arg2)\n                              (assert (= 1 arg1))\n                              (assert (= 2 arg2))\n                              (return-from b 3))))\n        (invoke-restart 'abort (+ 0 1) (+ 1 1))))\n    3)\n\n  (deftest restart-bind.nested\n    (block b\n      (restart-bind ((continue (lambda () (return-from b 'outer))))\n        (restart-bind ((continue (lambda () (return-from b 'inner))))\n          (invoke-restart 'continue))))\n    'inner)\n\n  (deftest restart-bind.reinvoke\n    (block b\n      (restart-bind ((y (lambda (val) (return-from b (+ val 1)))))\n        (restart-bind ((x (lambda (val) (invoke-restart 'y (+ val 1)))))\n          (invoke-restart 'x 1))))\n    3)\n\n  (deftest restart-bind.no-firewall\n    (block b\n      (restart-bind ((r1 (lambda () (return-from b 'outer))))\n        (restart-bind ((r2 (lambda () (invoke-restart 'r1))))\n          (restart-bind ((r1 (lambda () (return-from b 'inner))))\n            (invoke-restart 'r2)))))\n    'inner)\n\n  (deftest restart-bind.multiple.1\n    (block b\n      (restart-bind ((r1 (lambda (arg) (return-from b (- 100 arg))))\n                     (r2 (lambda (arg) (return-from b (+ 100 arg)))))\n        (invoke-restart 'r2 1)))\n    101)\n\n  (deftest restart-bind.multiple.2\n    (block b\n      (restart-bind ((r1 (lambda (arg) (return-from b (- 100 arg))))\n                     (r2 (lambda (arg) (return-from b (+ 100 arg)))))\n        (invoke-restart 'r1 1)))\n    99))\n\n(defsuite restart-case\n\n  (deftest restart-case.0\n    (signals-error\n     (restart-case)\n     match-error))\n\n  (deftest restart-case.0b\n    (restart-case ())\n    #void)\n\n  (deftest restart-case.1\n    (restart-case ()\n      1 2 (+ 1 2))\n    3)\n\n  (deftest restart-case.2\n    (restart-case ((continue (lambda () 'whatever)))\n      1 2 (+ 1 2))\n    3)\n\n  (deftest restart-case.3\n    (restart-case ((continue (lambda () 'whatever))\n                   (abort (lambda () 'whatever)))\n      1 2 (+ 1 2))\n    3)\n\n  (deftest restart-case.no-handler.1\n    (signals-error\n     (invoke-restart 'continue)\n     restart-error :restart-name 'continue))\n\n  (deftest restart-case.no-handler.2\n    (signals-error\n     (restart-case ((continue (lambda () 'whatever)))\n       (invoke-restart 'abort))\n     restart-error :restart-name 'abort))\n\n  (deftest restart-case.no-exit.1\n    (restart-case ((continue (lambda () 'whatever)))\n      (invoke-restart 'continue))\n    'whatever)\n\n  (deftest restart-case.no-exit.2\n    (restart-case ((continue (lambda () 'whatever)))\n      (if-option (restart-handler (find-restart? 'continue))\n        (invoke-restart restart-handler)\n        (assert #f)))\n    'whatever)\n\n  (deftest restart-case.handled\n    (restart-case ((abort (lambda (arg1 arg2)\n                            (assert (= 1 arg1))\n                            (assert (= 2 arg2))\n                            3)))\n      (invoke-restart 'abort (+ 0 1) (+ 1 1)))\n    3)\n\n  (deftest restart-case.nested\n    (restart-case ((continue (lambda () 'outer)))\n      (restart-case ((continue (lambda () 'inner)))\n        (invoke-restart 'continue)))\n    'inner)\n\n  (deftest restart-case.nested2\n    (restart-case ((r1 (lambda () 'outer)))\n      (restart-case ((r1 (lambda ()  (invoke-restart 'r1)))\n                     (r2 (lambda ())))\n        (invoke-restart 'r1)))\n    'outer)\n\n  (deftest restart-case.reinvoke\n    (restart-case ((y (lambda (val) (+ val 1))))\n      (restart-case ((x (lambda (val) (invoke-restart 'y (+ val 1)))))\n        (invoke-restart 'x 1)))\n    3)\n\n  (deftest restart-case.no-firewall\n    (restart-case ((r1 (lambda () 'outer)))\n      (restart-case ((r2 (lambda () (invoke-restart 'r1))))\n        (restart-case ((r1 (lambda () 'inner)))\n          (invoke-restart 'r2))))\n    'outer)\n\n  (deftest restart-case.parallel\n    (restart-case ((r1 (lambda () 'outer)))\n      (restart-case ((r1 (lambda () 'inner))\n                     (r2 (lambda () (invoke-restart 'r1))))\n        (invoke-restart 'r2)))\n    'outer)\n\n  (deftest restart-case.multiple.1\n    (restart-case ((r1 (lambda (arg) (- 100 arg)))\n                   (r2 (lambda (arg) (+ 100 arg))))\n      (invoke-restart 'r2 1))\n    101)\n\n  (deftest restart-case.multiple.2\n    (restart-case ((r1 (lambda (arg) (- 100 arg)))\n                   (r2 (lambda (arg) (+ 100 arg))))\n      (invoke-restart 'r1 1))\n    99))\n\n(defsuite find-restart?\n\n  (deftest find-restart?.0\n    (signals-error\n     (find-restart?)\n     match-error))\n\n  (deftest find-restart?.1\n    (find-restart? 'foo)\n    #nil)\n\n  (deftest find-restart?.1b\n    (restart-bind ((bar (lambda ())))\n      (find-restart? 'foo))\n    #nil)\n\n  (deftest find-restart?.2\n    (find-restart? 'foo (make-simple-error \"foo\"))\n    #nil)\n\n  (deftest find-restart?.3\n    (block exit\n      (restart-bind ((x (lambda (arg) (return-from exit (+ 1000 arg)))))\n        (if-option (r (find-restart? 'x))\n          (progn\n            (assert (= (slot-value r 'restart-name) 'x))\n            (assert (= (slot-value r 'associated-conditions) '()))\n            (invoke-restart r 1))\n          (assert #f))))\n    1001)\n\n  (deftest find-restart?.3b\n    (restart-case ((x (lambda (arg) (+ 1000 arg))))\n      (if-option (r (find-restart? 'x))\n        (progn\n          (assert (= (slot-value r 'restart-name) 'x))\n          (assert (= (slot-value r 'associated-conditions) '()))\n          (invoke-restart r 1))\n        (assert #f)))\n    1001)\n\n  (deftest find-restart?.4\n    (block exit\n      (def c (make-simple-error \"foo\"))\n      (restart-bind ((x (lambda (arg) (return-from exit (+ 1000 arg)))\n                        :associated-conditions (list c)))\n        (if-option (r (find-restart? 'x c))\n          (progn\n            (assert (= (slot-value r 'restart-name) 'x))\n            (assert (= (slot-value r 'associated-conditions) (list c)))\n            (invoke-restart r 1))\n          (assert #f))))\n    1001)\n\n  (deftest find-restart?.4b\n    (progn\n      (def c (make-simple-error \"foo\"))\n      (restart-bind ((x (lambda (arg) (+ 1000 arg))\n                        :associated-conditions (list c)))\n        (if-option (r (find-restart? 'x c))\n          (progn\n            (assert (= (slot-value r 'restart-name) 'x))\n            (assert (= (slot-value r 'associated-conditions) (list c)))\n            (invoke-restart r 1))\n          (assert #f))))\n    1001)\n\n  (deftest find-restart?.5\n    (block exit\n      (def c (make-simple-error \"foo\"))\n      (restart-bind ((x (lambda (arg) (return-from exit (+ 1000 arg)))\n                        :associated-conditions (list c)))\n        (restart-bind ((x (lambda (arg) (return-from exit (- 1000 arg)))))\n          (if-option (r (find-restart? 'x c))\n            (invoke-restart r 1)\n            (assert #f)))))\n    999)\n\n  (deftest find-restart?.5b\n    (progn\n      (def c (make-simple-error \"foo\"))\n      (restart-case ((x (lambda (arg) (+ 1000 arg))\n                        :associated-conditions (list c)))\n        (restart-case ((x (lambda (arg) (- 1000 arg))))\n          (if-option (r (find-restart? 'x c))\n            (invoke-restart r 1)\n            (assert #f)))))\n    999)\n\n  (deftest find-restart?.6\n    (block exit\n      (def c (make-simple-error \"foo\"))\n      (def d (make-simple-error \"bar\"))\n      (restart-bind ((x (lambda (arg) (return-from exit (+ 1000 arg)))\n                        :associated-conditions (list c)))\n        (restart-bind ((x (lambda (arg) (return-from exit (- 1000 arg)))\n                          :associated-conditions (list d)))\n          (if-option (r (find-restart? 'x c))\n            (invoke-restart r 1)\n            (assert #f)))))\n    1001)\n\n  (deftest find-restart?.6b\n    (progn\n      (def c (make-simple-error \"foo\"))\n      (def d (make-simple-error \"bar\"))\n      (restart-case ((x (lambda (arg) (+ 1000 arg))\n                        :associated-conditions (list c)))\n        (restart-case ((x (lambda (arg) (- 1000 arg))\n                          :associated-conditions (list d)))\n          (if-option (r (find-restart? 'x c))\n            (invoke-restart r 1)\n            (assert #f)))))\n    1001))\n\n(defsuite compute-restarts\n\n  (deftest compute-restarts.1\n    (compute-restarts)\n    '())\n\n  (defun restart-names condition?\n    (let ((restarts (apply #'compute-restarts condition?)))\n      (mapcar (lambda (restart)\n                (slot-value restart 'restart-name))\n              restarts)))\n\n  (deftest compute-restarts.1\n    (restart-case ((outer1 (lambda ()))\n                   (outer2 (lambda ())))\n      (restart-case ((middle1 (lambda ()))\n                     (middle2 (lambda ())))\n        (restart-case ((inner1 (lambda ()))\n                       (inner2 (lambda ())))\n          (assert (= (restart-names)\n                     '(inner1\n                       inner2\n                       middle1\n                       middle2\n                       outer1\n                       outer2)))\n          #t))))\n\n  (deftest compute-restarts.2\n    (progn\n      (def c (make-simple-error \"foo\"))\n      (def d (make-simple-error \"bar\"))\n      (restart-case ((outer1 (lambda ()))\n                     (outer2 (lambda ()) :associated-conditions (list c)))\n        (restart-case ((middle1 (lambda ()) :associated-conditions (list c))\n                       (middle2 (lambda ())))\n          (restart-case ((inner1 (lambda ()) :associated-conditions (list c d))\n                         (inner2 (lambda ())))\n            (assert (= (restart-names)\n                       '(inner1\n                         inner2\n                         middle1\n                         middle2\n                         outer1\n                         outer2)))\n            (assert (= (restart-names c)\n                       '(inner1\n                         inner2\n                         middle1\n                         middle2\n                         outer1\n                         outer2)))\n            (assert (= (restart-names d)\n                       '(inner1\n                         inner2\n                         middle2\n                         outer1)))\n            #t))))))\n\n(defsuite signal\n\n  (deftest signal.0\n    (signals-error\n     (signal)\n     match-error))\n\n  (deftest signal.1\n    (signal (make-simple-error \"foo\"))\n    #void)\n\n  (deftest signal.2\n    (signals-error\n     (signal (make-instance #^standard-object :x 1 :y 2))\n     standard-object :x 1 :y 2))\n\n  (deftest signal.3\n    (handler-bind ((simple-error (lambda (e)\n                                   (invoke-restart 'continue 12))))\n      (signal (make-simple-error \"Foo!\")\n        (abort (lambda (value) (* value 10)))\n        (continue (lambda (value) (* value 2)))))\n    24)\n\n  (deftest signal.3b\n    (handler-bind ((simple-error (lambda (e)\n                                   (invoke-restart 'abort 12))))\n      (signal (make-simple-error \"Foo!\")\n        (abort (lambda (value) (* value 10)))\n        (continue (lambda (value) (* value 2)))))\n    120)\n\n  (deftest signal.4\n    (restart-case ((abort (lambda (value) (* 1000 value))))\n      (handler-bind ((simple-error (lambda (e)\n                                     (invoke-restart 'abort 12))))\n        ;; Silly?  Yeah, probably.\n        (signal (signal (make-simple-error \"Foo!\"))\n          (abort (lambda (value) (* value 10)))\n          (continue (lambda (value) (* value 2))))))\n    12000)\n\n  (deftest signal.4b\n    (block b\n      (restart-case ((abort (lambda (value) (* 1000 value))))\n        (handler-bind ((simple-error (lambda (e)\n                                       (invoke-restart 'abort 12))))\n          (signal (signal (make-simple-error \"Foo!\")\n                    (abort (lambda (value) (return-from b (* 10000 value)))))\n            (abort (lambda (value) (* value 10)))\n            (continue (lambda (value) (* value 2)))))))\n    120000)\n\n  ;;; Test that `signal' (and `error' likewise) associates the\n  ;;; restarts it binds with the signalled error.\n  (deftest signal.associations\n    (dolist (#'signal-operator (list #'signal #'error) #t)\n      (block ok ; so that the test doesn't fail when testing `error'.\n        (handler-bind ((simple-error\n                        (lambda (e)\n                          (let* ((handler (optional (find-restart? 'r e)))\n                                 (a-cs (slot-value handler 'associated-conditions)))\n                            (assert (consp (member e a-cs)))\n                            (return-from ok)))))\n          (signal-operator (make-simple-error \"error!\")\n            (r (lambda () 1))))))))\n\n(defsuite invoke-restart\n\n  (deftest invoke-restart.0\n    (signals-error\n     (invoke-restart)\n     match-error))\n\n  (deftest invoke-restart.1\n    (signals-error\n     (invoke-restart \"foo\")\n     type-error :datum \"foo\")))\n\n(defsuite invoke-restart-interactively\n\n  (deftest invoke-restart-interactively.1\n    (restart-bind ((r1 (lambda (arg1 arg2) (+ arg1 arg2))\n                       :interactive-function (lambda () (list 1 2))))\n      (invoke-restart-interactively 'r1))\n    3)\n\n  (deftest invoke-restart-interactively.1a\n    (restart-bind ((r1 (lambda (arg1 arg2) (+ arg1 arg2))\n                       :interactive-function (lambda () (list 1 2))))\n      (invoke-restart-interactively (optional (find-restart? 'r1))))\n    3)\n\n  (deftest invoke-restart-interactively.1b\n    (restart-bind ((r1 (lambda () (+ 1 2))))\n      (invoke-restart-interactively 'r1))\n    3)\n\n  (deftest invoke-restart-interactively.1c\n    (restart-bind ((r1 (lambda () (+ 1 2))))\n      (invoke-restart-interactively (optional (find-restart?'r1))))\n    3)\n\n  (deftest invoke-restart-interactively.2\n    (signals-error\n     (restart-bind ((r1 (lambda (arg1 arg2) (+ arg1 arg2))\n                        :interactive-function (lambda () '())))\n       (invoke-restart-interactively 'r1))\n     match-error))\n\n  (deftest invoke-restart-interactively.3\n    (signals-error\n     (restart-bind ((r1 (lambda (arg1) arg1)\n                        :interactive-function (lambda () '(1 2 3))))\n       (invoke-restart-interactively 'r1))\n     match-error)))\n\n(defsuite |Condition Handling in the Lisp Language Family|\n  ;; Adapted from https://www.nhplace.com/kent/Papers/Condition-Handling-2001.html\n\n  (defclass unbound-variable (error) (name))\n\n  (deftest* firewall.1\n    (let ((result\n           (handler-case ((unbound-variable (lambda (c) (list 'outer c))))\n             (handler-bind ((error (lambda (#ignore)\n                                     (error (make-instance #^unbound-variable :name 'fred)))))\n               (handler-case ((unbound-variable (lambda (c) (list 'inner c))))\n                 ;; Signal an arbitrary error:\n                 (simple-error \"Not an UNBOUND-VARIABLE error.\"))))))\n      (assert (and (eq (car result) 'outer)\n                   (typep (cadr result) #^unbound-variable)\n                   (eq (slot-value (cadr result) 'name) 'fred)))))\n\n  (deftest* firewall.2\n    (let ((result\n           (handler-case ((unbound-variable (lambda (c) (list 'outer c))))\n             (handler-bind ((error (lambda (#ignore)\n                                     (error (make-instance #^unbound-variable :name 'fred)))))\n               (handler-case ((unbound-variable (lambda (c) (list 'inner c))))\n                 (error (make-instance #^unbound-variable :name 'marvin)))))))\n      (assert (and (eq (car result) 'inner)\n                   (typep (cadr result) #^unbound-variable)\n                   (eq (slot-value (cadr result) 'name) 'marvin))))))\n");
;// CONCATENATED MODULE: ./test/control-test.lispx
/* harmony default export */ const control_test_lispx = (";;; Control-related Tests\n\n;;; Fibers\n\n;; The following implementation of fibers follows the one at URL\n;; `http://okmij.org/ftp/continuations/implementations.html#dynamic-wind'\n;;\n;; We're calling them fibers instead of coroutines so as to not\n;; conflict with the built-in coroutine operators.\n;;\n;; We use it for testing that built-in operators properly suspend and\n;; resume.\n\n(defconstant +fiber-prompt+ 'fiber-prompt\n  \"The prompt used for delimiting fibers.\")\n\n(defclass yield-record ()\n  (value continuation)\n  (:documentation \"Instances of this class are yielded.\"))\n\n(defun make-yield-record (v k)\n  \"Create a new yield record with the given yielded value and resume continuation.\"\n  (make-instance #^yield-record :value v :continuation k))\n\n(defun fiber-yield v?\n  \"Yield a value (which defaults to void).\"\n  (take-subcont +fiber-prompt+ k\n    (make-yield-record (optional v?) k)))\n\n(defun fiber-resume (yield-record . v?)\n  \"Resume a suspended fiber with a value (which defaults to void).\"\n  (push-delim-subcont +fiber-prompt+ (slot-value yield-record 'continuation)\n    (optional v?)))\n\n(defmacro fiber body\n  \"Evaluate the body expressions as a fiber.\"\n  (list* #'push-prompt '+fiber-prompt+ body))\n\n(defun run-fiber (#'thunk)\n  \"Get all values yielded by a fiber, and its final result, and\ncollect them in a list.\"\n  (loop-let -run- ((result (fiber (thunk))))\n    (if (typep result #^yield-record)\n        (cons (slot-value result 'value) (-run- (fiber-resume result)))\n        (list result))))\n\n(defun run-fiber-with-values (#'thunk values)\n  \"Like `run-fiber' but uses a list of values that are sent to the\nfiber with `fiber-resume'.\"\n  (loop-let -run- ((result (fiber (thunk))) (values values))\n    (if (typep result #^yield-record)\n        (cons (slot-value result 'value)\n              (-run- (fiber-resume result (car values)) (cdr values)))\n        (list result))))\n\n(defsuite fibers\n\n  (deftest fiber.1\n    (fiber 1 2 (= 1 1))\n    #t)\n\n  (deftest fiber.2\n    (progn\n      (def yield-record (fiber 1 2 (+ (fiber-yield (= 1 1)) 3)))\n      (assert (typep yield-record #^yield-record))\n      (assert (= #t (slot-value yield-record 'value)))\n      (assert (= 33 (fiber-resume yield-record 30))))\n    #void)\n\n  (deftest fiber.progn.1\n    (run-fiber\n     (lambda ()\n       (progn\n         (fiber-yield 1)\n         (fiber-yield 2)\n         3)))\n    '(1 2 3))\n\n  (deftest fiber.progn.2\n    (run-fiber-with-values\n     (lambda ()\n       (progn\n         (fiber-yield 1)\n         (fiber-yield 2)))\n     '(#void 3))\n    '(1 2 3))\n\n  (deftest fiber.vau.1\n    (run-fiber\n     (lambda ()\n       ((vau () #ignore\n          (fiber-yield 1)\n          (fiber-yield 2)\n          3))))\n    '(1 2 3))\n\n  (deftest fiber.vau.2\n    (run-fiber-with-values\n     (lambda ()\n       ((vau () #ignore\n          (fiber-yield 1)\n          (fiber-yield 2))))\n     '(#void 3))\n    '(1 2 3))\n\n  (deftest fiber.lambda.1\n    (run-fiber\n     (lambda ()\n       ((lambda ()\n          (fiber-yield 1)\n          (fiber-yield 2)\n          3))))\n    '(1 2 3))\n\n  (deftest fiber.lambda.2\n    (run-fiber-with-values\n     (lambda ()\n       ((lambda ()\n          (fiber-yield 1)\n          (fiber-yield 2))))\n     '(#void 3))\n    '(1 2 3))\n\n  (deftest fiber.def.1\n    (run-fiber\n     (lambda ()\n       (def (x y) (list (fiber-yield 1) (fiber-yield 2)))\n       (list x y)))\n    '(1 2 (#void #void)))\n\n  (deftest fiber.def.2\n    (run-fiber-with-values\n     (lambda ()\n       (def (x y) (list (fiber-yield 1) (fiber-yield 2)))\n       (list x y))\n     '(3 4))\n    '(1 2 (3 4)))\n\n  (deftest fiber.if.1\n    (run-fiber\n     (lambda ()\n       (if #t (fiber-yield 1) (fiber-yield 2))))\n    '(1 #void))\n\n  (deftest fiber.if.2\n    (run-fiber-with-values\n     (lambda ()\n       (if #t (fiber-yield 1) (fiber-yield 2)))\n     '(3))\n    '(1 3))\n\n  (deftest fiber.if.3\n    (run-fiber\n     (lambda ()\n       (if #f (fiber-yield 1) (fiber-yield 2))))\n    '(2 #void))\n\n  (deftest fiber.if.4\n    (run-fiber-with-values\n     (lambda ()\n       (if #f (fiber-yield 1) (fiber-yield 2)))\n     '(3))\n    '(2 3))\n\n  (deftest fiber.if.5\n    (run-fiber-with-values\n     (lambda ()\n       (if (fiber-yield 1) (fiber-yield 2) (fiber-yield 3)))\n     '(#t 4))\n    '(1 2 4))\n\n  (deftest fiber.if.6\n    (run-fiber-with-values\n     (lambda ()\n       (if (fiber-yield 1) (fiber-yield 2) (fiber-yield 3)))\n     '(#f 4))\n    '(1 3 4))\n\n  (deftest fiber.if.7\n    (run-fiber-with-values\n     (lambda ()\n       (if (progn (fiber-yield 1) (fiber-yield 2))\n           (progn (fiber-yield 3) (fiber-yield 3.5))\n           (progn (fiber-yield 4) (fiber-yield 4.5))))\n     '(#void #t #void 5))\n    '(1 2 3 3.5 5))\n\n  (deftest fiber.if.8\n    (run-fiber-with-values\n     (lambda ()\n       (if (progn (fiber-yield 1) (fiber-yield 2))\n           (progn (fiber-yield 3) (fiber-yield 3.5))\n           (progn (fiber-yield 4) (fiber-yield 4.5))))\n     '(#void #f #void 5))\n    '(1 2 4 4.5 5))\n\n  (deftest fiber.loop.1\n    (run-fiber\n     (lambda ()\n       (def ct 1)\n       (def env (the-environment))\n       (block exit\n         (loop\n          (if (= ct 5)\n              (return-from exit ct)\n              (progn\n                (fiber-yield ct)\n                (fiber-yield (- ct))\n                (set env ct (+ ct 1))))))))\n    '(1 -1 2 -2 3 -3 4 -4 5))\n\n  (deftest fiber.block.1\n    (run-fiber\n     (lambda ()\n       (block b\n         (fiber-yield 1)\n         (fiber-yield 2)\n         3)))\n    '(1 2 3))\n\n  (deftest fiber.block.2\n    (run-fiber-with-values\n     (lambda ()\n       (block b\n         (fiber-yield 1)\n         (fiber-yield 2)))\n     '(#void 3))\n    '(1 2 3))\n\n  (deftest fiber.block.3\n    (run-fiber\n     (lambda ()\n       (block b\n         (fiber-yield 1)\n         (fiber-yield 2)\n         (return-from b 3))))\n    '(1 2 3))\n\n  (deftest fiber.block.4\n    (run-fiber-with-values\n     (lambda ()\n       (block b\n         (fiber-yield 1)\n         (return-from b (fiber-yield 2))))\n     '(#void 3))\n    '(1 2 3))\n\n  (deftest fiber.catch.1\n    (run-fiber\n     (lambda ()\n       (catch 'b\n         (fiber-yield 1)\n         (fiber-yield 2)\n         3)))\n    '(1 2 3))\n\n  (deftest fiber.catch.2\n    (run-fiber-with-values\n     (lambda ()\n       (catch 'b\n         (fiber-yield 1)\n         (fiber-yield 2)))\n     '(#void 3))\n    '(1 2 3))\n\n  (deftest fiber.catch.3\n    (run-fiber\n     (lambda ()\n       (catch 'b\n         (fiber-yield 1)\n         (fiber-yield 2)\n         (throw 'b 3))))\n    '(1 2 3))\n\n  (deftest fiber.catch.4\n    (run-fiber-with-values\n     (lambda ()\n       (catch 'b\n         (fiber-yield 1)\n         (throw 'b (fiber-yield 2))))\n     '(#void 3))\n    '(1 2 3))\n\n  (deftest fiber.unwind-protect.1\n    (run-fiber-with-values\n     (lambda ()\n       (unwind-protect (fiber-yield 1)\n         (fiber-yield 2)\n         (fiber-yield 2.5)\n         3))\n     '(4 #void #void))\n    '(1 2 2.5 4))\n\n  (deftest fiber.unwind-protect.2\n    (run-fiber-with-values\n     (lambda ()\n       (block exit\n         (unwind-protect (return-from exit (fiber-yield 1))\n           (fiber-yield 2)\n           (fiber-yield 2.5)\n           3)))\n     '(4 #void #void))\n    '(1 2 2.5 4))\n\n  (deftest fiber.unwind-protect.3\n    (run-fiber-with-values\n     (lambda ()\n       (block exit\n         (unwind-protect (return-from exit 4)\n           (fiber-yield 2)\n           (fiber-yield 2.5)\n           3)))\n     '(#void #void))\n    '(2 2.5 4))\n\n  (deftest fiber.unwind-protect.4\n    (run-fiber-with-values\n     (lambda ()\n       (block exit\n         (unwind-protect (progn (fiber-yield 1) (fiber-yield 1.5))\n           (fiber-yield 2)\n           (fiber-yield 2.5)\n           3)))\n     '(#void 4 #void #void))\n    '(1 1.5 2 2.5 4))\n\n  (deftest fiber.progv.1\n    (run-fiber\n     (lambda ()\n       (defdynamic *x*)\n       (progv (list *x*) (list 1)\n         (fiber-yield (dynamic *x*))\n         (fiber-yield (dynamic *x*))\n         (progv (list *x*) (list 2)\n           (fiber-yield (dynamic *x*))\n           (fiber-yield (dynamic *x*))\n           3))))\n    '(1 1 2 2 3))\n\n  (deftest fiber.progv.2\n    (run-fiber-with-values\n     (lambda ()\n       (defdynamic *x*)\n       (progv (list *x*) (list 1)\n         (fiber-yield (dynamic *x*))\n         (fiber-yield (dynamic *x*))\n         (progv (list *x*) (list 2)\n           (fiber-yield (dynamic *x*))\n           (fiber-yield (dynamic *x*)))))\n     '(#void #void #void 3))\n    '(1 1 2 2 3))\n\n  (deftest fiber.fun.1\n    (run-fiber\n     (lambda ()\n       (list (fiber-yield 1) (fiber-yield 2))))\n    '(1 2 (#void #void)))\n\n  (deftest fiber.fun.2\n    (run-fiber-with-values\n     (lambda () (list (fiber-yield 1) (fiber-yield 2)))\n     '(3 4))\n    '(1 2 (3 4))))\n\n;;; Basic Operator Tests\n\n(defsuite push-prompt\n\n  (deftest push-prompt.1\n    (signals-error\n     (push-prompt)\n     match-error))\n\n  (deftest push-prompt.2\n    (push-prompt 'p)\n    #void)\n\n  (deftest push-prompt.3\n    (push-prompt 'p 1 2 (= 1 1))\n    #t)\n\n  (deftest push-prompt.4\n    (progn\n      (block ret\n        (push-prompt 'p\n          (assert (prompt-set-p 'p))\n          (return-from ret)))\n      (assert (not (prompt-set-p 'p))))\n    #void)\n\n  (deftest push-prompt.5\n    (progn\n      (assert (not (prompt-set-p 'p)))\n      (assert (not (prompt-set-p 'q)))\n      (push-prompt 'p\n        (assert (prompt-set-p 'p))\n        (assert (not (prompt-set-p 'q)))\n        (push-prompt 'q\n          (assert (prompt-set-p 'p))\n          (assert (prompt-set-p 'q)))\n        (assert (prompt-set-p 'p))\n        (assert (not (prompt-set-p 'q))))\n      (assert (not (prompt-set-p 'p)))\n      (assert (not (prompt-set-p 'q))))\n    #void))\n\n(defsuite take-subcont\n\n  (deftest take-subcont.1\n    (signals-error\n     (take-subcont)\n     match-error))\n\n  (deftest take-subcont.2\n    (signals-error\n     (take-subcont 'p)\n     match-error))\n\n  (deftest take-subcont.3\n    (signals-error\n     (take-subcont 'p #ignore)\n     prompt-not-found-error :prompt 'p))\n\n  (deftest take-subcont.4\n    (signals-error\n     (push-prompt 'p (take-subcont 'q #ignore))\n     prompt-not-found-error :prompt 'q))\n\n  (deftest take-subcont.5\n    (push-prompt 'p (take-subcont 'p #ignore))\n    #void)\n\n  (deftest take-subcont.6\n    (push-prompt 'p (take-subcont 'p #ignore (= 1 1)))\n    #t)\n\n  (deftest take-subcont.7\n    (push-prompt 'p (take-subcont 'p k (push-delim-subcont 'p k (= 1 1))))\n    #t))\n\n(defsuite push-delim-subcont\n\n  (deftest push-delim-subcont.1\n    (signals-error\n     (push-delim-subcont)\n     match-error))\n\n  (deftest push-delim-subcont.2\n    (signals-error\n     (push-delim-subcont 'p)\n     match-error))\n\n  (deftest push-delim-subcont.3\n    (signals-error\n     (push-delim-subcont 'p 12)\n     type-error :datum 12))\n\n  (deftest push-delim-subcont.4\n    (let ((k (push-prompt 'p (+ 100 (take-subcont 'p k k)))))\n      (assert (= 102 (push-delim-subcont 'p k (+ 1 1))))\n      (assert (= 120 (push-delim-subcont 'p k (+ 10 10)))))\n    #void)\n\n  (deftest push-delim-subcont.5\n    (let ((k (push-prompt 'p (push-prompt 'q (take-subcont 'p k k)))))\n      (push-delim-subcont 'p k\n        (assert (prompt-set-p 'q))\n        (assert (prompt-set-p 'p))))\n    #void))\n\n(defsuite prompt-set-p\n\n  (deftest prompt-set-p.1\n    (signals-error\n     (prompt-set-p)\n     match-error))\n\n  (deftest prompt-set-p.2\n    (prompt-set-p 'p)\n    #f)\n\n  (deftest prompt-set-p.3\n    (push-prompt 'p (progn (prompt-set-p 'p)))\n    #t))\n\n(defsuite push-subcont-barrier\n\n  (deftest push-subcont-barrier.1\n    (signals-error\n     (push-subcont-barrier\n      (take-subcont 'p1 sk))\n     prompt-not-found-error :prompt 'p1))\n\n  (deftest push-subcont-barrier.2\n    (signals-error\n     (push-prompt 'p1\n       (push-subcont-barrier\n         (take-subcont 'p1 sk)))\n     prompt-not-found-error :prompt 'p1)))\n\n(defsuite dynamics\n\n  (deftest defdynamic.1\n    (progn\n      (defdynamic *x* 1)\n      (defdynamic *y* (+ 1 1))\n      (assert (= (dynamic *x*) 1))\n      (assert (= (dynamic *y*) 2))\n      (dynamic-let ((*x* 3))\n        (assert (= (dynamic *x*) 3))\n        (assert (= (dynamic *y*) 2))\n        (dynamic-let ((*y* 4))\n          (assert (= (dynamic *x*) 3))\n          (assert (= (dynamic *y*) 4)))\n        (assert (= (dynamic *x*) 3))\n        (assert (= (dynamic *y*) 2)))\n      (assert (= (dynamic *x*) 1))\n      (assert (= (dynamic *y*) 2)))\n    #void)\n\n  (deftest* defdynamic.redefine\n    (defdynamic *a* (+ 1 1))\n    (def old-a *a*)\n    (assert (= (dynamic *a*) 2))\n    (assert (= (dynamic old-a) 2))\n    (defdynamic *a* (+ 2 2))\n    (assert (= (dynamic *a*) 4))\n    (assert (= (dynamic old-a) 4))\n    (assert (eq old-a *a*))\n    (defdynamic *a*)\n    (assert (= (dynamic *a*) #void))\n    (assert (= (dynamic old-a) #void))\n    (assert (eq old-a *a*)))\n\n  (deftest progv.1\n    (progn\n      (defdynamic *x* 1)\n      (defdynamic *y* 2)\n      (assert (= (dynamic *x*) 1))\n      (assert (= (dynamic *y*) 2))\n      (progv (list *x*) (list 3)\n        (assert (= (dynamic *x*) 3))\n        (assert (= (dynamic *y*) 2))\n        (progv (list *y*) (list 4)\n          (assert (= (dynamic *x*) 3))\n          (assert (= (dynamic *y*) 4)))\n        (assert (= (dynamic *x*) 3))\n        (assert (= (dynamic *y*) 2)))\n      (assert (= (dynamic *x*) 1))\n      (assert (= (dynamic *y*) 2)))\n    #void)\n\n  (deftest dynamic.1\n    (progn\n      (defdynamic *foo*)\n      (assert (= (dynamic *foo*) #void))\n      (assert (= (slot-value *foo* 'value) #void))\n      (assert (typep *foo* #^dynamic))\n      (assert (typep *foo* #^standard-object))\n      (assert (typep *foo* #^object))\n      (assert (subclassp #^dynamic #^standard-object))\n      (assert (subclassp #^dynamic #^object)))\n    #void)\n\n  (deftest set-dynamic.1\n    (progn\n      (defdynamic *bar*)\n      (dynamic-let ((*bar* 1))\n        (set-dynamic *bar* 2)\n        (assert (= 2 (dynamic *bar*)))\n        (dynamic-let ((*bar* 3))\n          (assert (= 3 (dynamic *bar*))))\n        (assert (= 2 (dynamic *bar*)))\n        (set-dynamic *bar* 4)\n        (assert (= 4 (dynamic *bar*))))\n      (assert (= #void (dynamic *bar*)))\n      #t))\n\n  (deftest dynamic-let*.1\n    (dynamic-let* () (+ 1 1))\n    2)\n\n  (deftest dynamic-let*.2\n    (progn\n      (defdynamic *x* 1)\n      (dynamic-let* ((*x* 2)) (+ 1 (dynamic *x*))))\n    3)\n\n  (deftest dynamic-let*.2\n    (progn\n      (defdynamic *x* 1)\n      (defdynamic *y* 0)\n      (dynamic-let* ((*x* 2) (*y* (+ (dynamic *x*) 1)))\n        (list (dynamic *x*) (dynamic *y*))))\n    '(2 3))\n\n  (deftest dynamic-let-sanity-check\n    (progn\n      (defdynamic *x* 1)\n      (defdynamic *y* 0)\n      (dynamic-let ((*x* 2) (*y* (+ (dynamic *x*) 1)))\n        (list (dynamic *x*) (dynamic *y*))))\n    '(2 2)))\n\n(defsuite built-in-prompts\n\n  (deftest |Default prompt exists|\n    (boundp '+default-prompt+ (the-environment)))\n\n  (deftest |Root prompt exists|\n    (boundp '+root-prompt+ (the-environment)))\n\n  (deftest |Root prompt is set|\n    ;; I have no idea why this works.  I mean it's the right thing,\n    ;; but it's unclear how it interacts with test-util.lispx's\n    ;; Mocha stuff that runs this test.\n    (take-subcont +root-prompt+ #ignore)\n    #void))\n\n;;; Simple Control Operators\n\n(defsuite loop\n\n  (deftest loop.1\n    (let ((ct 0))\n      (def env (the-environment))\n      (block exit\n        (loop 'just-a-symbol-to-test-implicit-progn\n              (if (= ct 10)\n                  (return-from exit ct)\n                  (set env ct (+ ct 1))))))\n    10))\n\n(defsuite catch/throw\n\n  (deftest catch.1\n    (signals-error\n     (catch)\n     match-error))\n\n  (deftest catch.2\n    (catch 'x)\n    #void)\n\n  (deftest catch.3\n    (catch 'x 1 2 3 (= 1 1))\n    #t)\n\n  (deftest catch.4\n    (catch 'x 1 (throw 'x (= 1 2)) 3 (= 1 1))\n    #f)\n\n  (deftest catch.5\n    (catch 'x 1 (throw 'x) 3 (= 1 1))\n    #void)\n\n  (deftest catch.6\n    (catch 'x 1 (catch 'y (throw 'x 44)) 3 (= 1 1))\n    44)\n\n  (deftest catch.7\n    (catch 'x 1 (catch 'y (throw 'y 2)) 3 55)\n    55)\n\n  (deftest catch.8\n    (catch 'x 1 (catch 'y (throw 'y 2)))\n    2)\n\n  (deftest catch.9\n    (catch 'x 1 (catch 'y (throw 'y)))\n    #void))\n\n(defsuite block/return-from\n\n  (deftest block.1\n    (signals-error\n     (block)\n     match-error))\n\n  (deftest block.2\n    (block x)\n    #void)\n\n  (deftest block.3\n    (block x 1 2 3 (= 1 1))\n    #t)\n\n  (deftest block.4\n    (block x 1 (return-from x (= 1 2)) 3 (= 1 1))\n    #f)\n\n  (deftest block.5\n    (block x 1 (return-from x) 3 (= 1 1))\n    #void)\n\n  (deftest block.6\n    (block x 1 (block y (return-from x (= 1 2))) 3 (= 1 1))\n    #f)\n\n  (deftest block.7\n    (block x 1 (block y (return-from y 2)) 3 (= 1 1))\n    #t)\n\n  (deftest block.8\n    (block x 1 (block y (return-from y 2)))\n    2)\n\n  (deftest block.9\n    (block x 1 (block y (return-from y)))\n    #void))\n\n(defsuite unwind-protect\n\n  (deftest unwind-protect.1\n    (signals-error\n     (unwind-protect)\n     match-error))\n\n  (deftest unwind-protect.2\n    (unwind-protect (= 1 1))\n    #t)\n\n  (deftest unwind-protect.3\n    (progn\n      (def env (the-environment))\n      (+ (unwind-protect 1 2 3 (set env x 10))\n         x))\n    11)\n\n  (deftest unwind-protect.4\n    (progn\n      (def env (the-environment))\n      (+ (block exit\n           (unwind-protect (return-from exit 1) 2 3 (set env x 10)))\n         x))\n    11)\n\n  (deftest unwind-protect.5\n    (block exit\n      (unwind-protect 1 2 3 (return-from exit 4)))\n    4)\n\n  (deftest unwind-protect.6\n    (block exit\n      (unwind-protect (return-from exit 1) 2 3 (return-from exit 4)))\n    4))\n");
;// CONCATENATED MODULE: ./test/delimcc-test.lispx
/* harmony default export */ const delimcc_test_lispx = (";;; Delimited Control Tests                                -*- Lisp -*-\n\n;; The following tests are adapted from the file `testd0.ml' of the\n;; caml-shift distribution, available at the URL\n;; `http://okmij.org/ftp/continuations/implementations.html'\n\n;; First, some preliminary definitions.\n\n(defun abort-subcont (prompt value)\n  \"Utility that captures the continuation but just ignores it.\"\n  (take-subcont prompt #ignore value))\n\n(defmacro push-subcont (k . forms)\n  \"We don't have `push-subcont' but for these tests we can emulate it\nwith a `push-delim-subcont' that pushes an unused prompt.\"\n  (list* #'push-delim-subcont ''unused-prompt k forms))\n\n(defun shift (p #'f)\n  \"The `shift' operator, adapted from the file `delimcc.ml'.\"\n  (take-subcont p sk\n    (push-prompt p\n      (f (lambda (#'c)\n           (push-delim-subcont p sk (c)))))))\n\n(defun prompt-set-p (prompt)\n  \"Return true if a prompt is set, false otherwise.\n(Note that this leads to stack growth, i.e. leaks.)\"\n  (handler-case ((prompt-not-found-error (lambda #ignore #f)))\n    (take-subcont prompt k (push-delim-subcont prompt k #t))))\n\n;; The actual tests.\n\n(defsuite caml-shift\n\n  (deftest test1\n    (progn\n      (assert (not (prompt-set-p 'p)))\n      (push-prompt 'p\n        (assert (prompt-set-p 'p))\n        1))\n    1)\n\n  (deftest test2\n    (+ (push-prompt 'p (push-prompt 'p 5))\n       4)\n    9)\n\n  (deftest test3\n    (+ (push-prompt 'p (+ 6 (abort-subcont 'p 5)))\n       4)\n    9)\n\n  (deftest |test3'|\n    (+ (push-prompt 'p\n         (push-prompt 'p (+ 6 (abort-subcont 'p 5))))\n       4)\n    9)\n\n  (deftest |test3''|\n    (+ (push-prompt 'p\n         (push-prompt 'p (+ 6 (abort-subcont 'p 5)))\n         (+ (abort-subcont 'p 7)\n            10))\n       20)\n    27)\n\n  (deftest |test3'''|\n    (signals-error\n     (progn\n       (push-prompt 'p\n         (push-prompt 'p (+ 6 (abort-subcont 'p 5)))\n         (+ (abort-subcont 'p 7)\n            10))\n       (abort-subcont 'p 9))\n     prompt-not-found-error :prompt 'p))\n\n  (deftest test4\n    (+ (push-prompt 'p\n         (+ (take-subcont 'p sk\n              (push-prompt 'p\n                (push-subcont sk 5)))\n            10))\n       20)\n    35)\n\n  (deftest test5\n    (+ (push-prompt 'p0\n         (+ (shift 'p0 (lambda (#'sk)\n                         (+ 100 (sk (lambda () (sk (lambda () 3)))))))\n            2))\n       10)\n    117)\n\n  (deftest |test5'|\n    (+ 10 (push-prompt 'p0\n            (+ 2 (shift 'p0 (lambda (#'sk)\n                              (sk (lambda () (+ 3 100))))))))\n    115)\n\n  (deftest |test5''|\n    (+ (push-prompt 'p0\n         (+ (shift 'p0 (lambda (#'sk)\n                         (+ (sk (lambda ()\n                                  (push-prompt 'p1\n                                    (+ 9 (sk (lambda () (abort-subcont 'p1 3)))))))\n                            100)))\n            2))\n       10)\n    115)\n\n  (deftest |test5'''|\n    (+ (push-prompt 'p0\n         (let ((v (shift 'p0 (lambda (#'sk)\n                               (+ (sk (lambda ()\n                                        (push-prompt 'p1\n                                          (+ 9 (sk (lambda ()\n                                                     (abort-subcont 'p1 3)))))))\n                                  100)))))\n           (+ v 2)))\n       10)\n    115)\n\n  (deftest test54\n    (+ (push-prompt 'p0\n         (let ((v (shift 'p0 (lambda (#'sk)\n                               (+ (sk (lambda ()\n                                        (push-prompt 'p1\n                                          (+ 9 (sk (lambda ()\n                                                     (abort-subcont 'p0 3)))))))\n                                  100)))))\n           (+ v 2)))\n       10)\n    124)\n\n  (deftest test6\n    (+ (flet ((push-twice (sk)\n                (push-subcont sk (push-subcont sk 3))))\n         (push-prompt 'p1\n           (push-prompt 'p2\n             (+ (take-subcont 'p1 sk\n                  (push-twice sk))\n                1))))\n       10)\n    15)\n\n  (deftest test7\n    (+ (flet ((push-twice (sk)\n                (push-subcont sk\n                  (push-subcont sk\n                    (take-subcont 'p2 sk2\n                      (push-subcont sk2\n                        (push-subcont sk2 3)))))))\n         (push-prompt 'p1\n           (+ (push-prompt 'p2\n                (+ 10 (push-prompt 'p3\n                        (take-subcont 'p1 sk (push-twice sk)))))\n              1)))\n       100)\n    135)\n\n  (deftest |test7'|\n    (+ (flet ((push-twice (#'sk)\n                (sk (lambda ()\n                      (sk (lambda ()\n                            (shift 'p2 (lambda (#'sk2)\n                                         (sk2 (lambda ()\n                                                (sk2 (lambda () 3))))))))))))\n         (push-prompt 'p1\n           (+ (push-prompt 'p2\n                (+ 10 (push-prompt 'p3\n                        (shift 'p1 (lambda (#'sk) (push-twice #'sk))))))\n              1)))\n       100)\n    135))\n\n;;; Delimited Dynamic Binding\n\n;; The following tests are adapted from the files `caml-dynvar.ml' and\n;; `dynvar-scheme48-problem.scm' of the DBplusDC distribution, available\n;; at the URL `http://okmij.org/ftp/Computation/dynamic-binding.html'\n\n(defun dset (dynvar value)\n  \"Set the value of a dynamic variable and return the old value.\"\n  (prog1 (dynamic dynvar)\n    (set-dynamic dynvar value)))\n\n(defsuite dbplusdc\n\n  (deftest testc\n    (progn\n      (defdynamic *p*)\n      (dynamic-let ((*p* 0))\n        (flet ((f () (dynamic *p*)))\n          (let ((x (f))\n                (y (dynamic-let ((*p* 1)) (f)))\n                (z (f)))\n            (list x y z)))))\n    '(0 1 0))\n\n  (deftest test1\n    (progn\n      (defdynamic *p*)\n      (dynamic-let ((*p* 1))\n        (list (dynamic *p*) (dynamic *p*))))\n    '(1 1))\n\n  (deftest test11\n    (progn\n      (defdynamic *p*)\n      (dynamic-let ((*p* 1))\n        (list (dynamic *p*)\n              (dynamic *p*)\n              (dynamic-let ((*p* 2))\n                (dynamic *p*)))))\n    '(1 1 2))\n\n  ;; It's probably a happy accident that this test works, since\n  ;; our `dset' is pretty different from DBplusDC's.\n  (deftest test12\n    (progn\n      (defdynamic *p*)\n      (dynamic-let ((*p* 1))\n        (let* ((v1 (dynamic *p*))\n               (v2 (dynamic-let ((*p* 2))\n                     (let ((v3 (dset *p* 12))\n                           (v4 (dynamic *p*)))\n                       (list v3 v4)))))\n          (let ((v5 (dynamic *p*)))\n            (list v1 v2 v5)))))\n    '(1 (2 12) 1))\n\n  (deftest test_eq4\n    (progn\n      (defdynamic *p*)\n      (dynamic-let ((*p* 1))\n        (push-prompt 'p0\n          (dynamic *p*))))\n    1)\n\n  (deftest test_eq5\n    (progn\n      (defdynamic *p*)\n      (dynamic-let ((*p* 1))\n        (push-prompt 'p0\n          (dynamic-let ((*p* 2))\n            (take-subcont 'p0 #ignore\n              (dynamic *p*))))))\n    1)\n\n  (deftest test_eq6\n    (progn\n      (defdynamic *p*)\n      (defdynamic *q*)\n      ((lambda (#'f)\n         (dynamic-let ((*p* 2) (*q* 20))\n           (#'f (lambda () 0))))\n       (dynamic-let ((*p* 1))\n         (push-prompt 'p\n           (dynamic-let ((*q* 10))\n             ((lambda (x) (+ (dynamic *p*) (dynamic *q*)))\n              (shift 'p (lambda (f) f))))))))\n    12))\n\n;;; Tests for single prompt coroutines\n\n(defsuite |Coroutines|\n\n  (deftest |Adapted from A Monadic Framework for Delimited Continuations|\n    (+ 2 (coroutine (if (yield k (+ (resume k #f) (resume k #t)))\n                        3\n                        4)))\n    9))\n");
;// CONCATENATED MODULE: ./test/stream-test.lispx
/* harmony default export */ const stream_test_lispx = (";;; Stream Tests\n\n(defsuite stream-tests\n\n  (deftest standard-streams-defined\n    (and (typep *standard-input* #^dynamic)\n         (typep *standard-output* #^dynamic))))\n\n(defsuite string-input-streams\n\n  (deftest make-string-input-stream.1\n    (typep (make-string-input-stream \"foo\") #^string-input-stream))\n\n  (deftest make-string-input-stream.2\n    (signals-error\n     (make-string-input-stream)\n     match-error))\n\n  (deftest make-string-input-stream.3\n    (signals-error\n     (make-string-input-stream 12)\n     type-error :datum 12 :expected-type 'string))\n\n  (deftest with-standard-input-from-string.1\n    (with-standard-input-from-string \"foo\")\n    #void)\n\n  (deftest with-standard-input-from-string.2\n    (with-standard-input-from-string \"12\" (read))\n    12)\n\n  (deftest with-standard-input-from-string.2\n    (with-standard-input-from-string \"12 #t\" (list (read) (read)))\n    (list 12 #t)))\n\n(defsuite string-output-streams\n\n  (deftest make-string-output-stream.1\n    (typep (make-string-output-stream) #^string-output-stream))\n\n  (deftest with-standard-output-to-string.1\n    (with-standard-output-to-string (print \"foo\") (print \"bar\"))\n    \"\\n\\\"foo\\\"\\n\\\"bar\\\"\")\n\n  (deftest with-standard-output-to-string.2\n    (with-standard-output-to-string)\n    \"\")\n\n  (deftest get-output-stream-string.1\n    (let ((s (make-string-output-stream)))\n      (get-output-stream-string s))\n    \"\")\n\n  (deftest get-output-stream-string.2\n    (signals-error\n     (get-output-stream-string)\n     match-error))\n\n  (deftest get-output-stream-string.3\n    (signals-error\n     (get-output-stream-string 1)\n     type-error :datum 1 :expected-type 'string-output-stream)))\n\n(defsuite fresh-line\n\n  (deftest fresh-line.1\n    (with-standard-output-to-string (fresh-line) (uprint1 \"a\") (fresh-line))\n    \"\\na\\n\")\n\n  (deftest fresh-line.2\n    (let ((sos (make-string-output-stream)))\n      (with-standard-output-to-string\n       (fresh-line) (uprint1 \"a\") (fresh-line)\n       (fresh-line sos) (dynamic-let ((*standard-output* sos)) (uprint1 \"b\")) (fresh-line sos)\n       (assert (= \"\\nb\\n\" (get-output-stream-string sos)))))\n    \"\\na\\n\"))\n");
;// CONCATENATED MODULE: ./test/read-test.lispx
/* harmony default export */ const read_test_lispx = (";;; LispX Reader Tests\n\n(defsuite read\n\n  (deftest read.1\n    (with-standard-input-from-string \"foo\" (read))\n    'foo)\n\n  (deftest read.1a\n    (with-standard-input-from-string \"foo\" (read (dynamic *standard-input*)))\n    'foo)\n\n  (deftest read.2\n    (signals-error\n     (with-standard-input-from-string \"(foo\" (read))\n    end-of-file))\n\n  (deftest read.2a\n    (signals-error\n     (with-standard-input-from-string \"(foo\" (read (dynamic *standard-input*)))\n    end-of-file))\n\n  ;; Incomplete objects always cause `end-of-file', even if EOF-ERROR-P is false.\n  (deftest read.2b\n    (signals-error\n     (with-standard-input-from-string \"(foo\" (read (dynamic *standard-input*) #f))\n     end-of-file))\n\n  (deftest read.2c1\n    (signals-error\n     (with-standard-input-from-string \"\" (read))\n     end-of-file))\n\n  (deftest read.2c2\n    (signals-error\n     (with-standard-input-from-string \"\" (read (dynamic *standard-input*)))\n     end-of-file))\n\n  (deftest read.2d\n    (with-standard-input-from-string \"\" (read (dynamic *standard-input*) #f))\n    #void)\n\n  (deftest read.2e\n    (with-standard-input-from-string \"\" (read (dynamic *standard-input*) #f 44))\n    44)\n\n  (deftest read.3a\n    (signals-error\n     (with-standard-input-from-string \"\" (read 'not-a-stream))\n     unbound-method-error :class #^symbol :method-name 'stream-read))\n\n  (deftest read.3b\n    (signals-error\n     (with-standard-input-from-string \"\" (read (dynamic *standard-input*) 12))\n     type-error :datum 12 :expected-type 'boolean))\n\n  (deftest read.4\n    (with-standard-input-from-string \"1 2 3\"\n      (assert (= 1 (read)))\n      (assert (= 2 (read)))\n      (assert (= 3 (read)))\n      (assert (= 4 (read (dynamic *standard-input*) #f 4)))\n      #t)))\n");
;// CONCATENATED MODULE: ./test/print-test.lispx
/* harmony default export */ const print_test_lispx = (";;; LispX Printer Tests\n\n(defsuite *print-escape*\n\n  (deftest *print-escape*.defined\n    (and (typep *print-escape* #^dynamic)\n         (typep (dynamic *print-escape*) #^boolean))))\n\n(defsuite *print-level?*\n\n  (deftest *print-level?*.defined\n    (and (typep *print-level?* #^dynamic)\n         (typep (dynamic *print-level?*) #^list)))\n\n  (defun write-with-level (object level)\n    (dynamic-let ((*print-level?* (some level)))\n      (with-standard-output-to-string (write object))))\n\n  (deftest *print-level?*.cons\n    (progn\n      (def a '(1 (2 (3 (4 (5 (6)))))))\n      (assert (= \"#\" (write-with-level a 0)))\n      (assert (= \"(1 #)\" (write-with-level a 1)))\n      (assert (= \"(1 (2 #))\" (write-with-level a 2)))\n      (assert (= \"(1 (2 (3 #)))\" (write-with-level a 3)))\n      (assert (= \"(1 (2 (3 (4 #))))\" (write-with-level a 4)))\n      (assert (= \"(1 (2 (3 (4 (5 #)))))\" (write-with-level a 5)))\n      (assert (= \"(1 (2 (3 (4 (5 (6))))))\" (write-with-level a 6)))\n      (assert (= \"(1 (2 (3 (4 (5 (6))))))\" (write-with-level a 7)))\n      #t))\n\n  (deftest *print-level?*.standard-object\n    (progn\n      (defclass a () (slot))\n      (defun make-a (slot) (make-instance #^a :slot slot))\n      (def a (make-a (make-a (make-a 12))))\n      (assert (= \"#\" (write-with-level a 0)))\n      (assert (= \"#<a :slot #>\" (write-with-level a 1)))\n      (assert (= \"#<a :slot #<a :slot #>>\" (write-with-level a 2)))\n      (assert (= \"#<a :slot #<a :slot #<a :slot 12>>>\" (write-with-level a 3)))\n      (assert (= \"#<a :slot #<a :slot #<a :slot 12>>>\" (write-with-level a 4)))\n      #t)))\n\n(defsuite write\n\n  (deftest write.1\n    (with-standard-output-to-string (write 1) (write 2))\n    \"12\")\n\n  (deftest write.1a\n    (with-standard-output-to-string\n      (assert (= 1 (write 1))))\n    \"1\")\n\n  (deftest write.2\n    (signals-error\n     (write)\n     match-error))\n\n  (deftest write.3\n    (let ((s1 (make-string-output-stream))\n          (s2 (make-string-output-stream)))\n      (write 1 :stream s1)\n      (write 2 :stream s2)\n      (and (= \"1\" (get-output-stream-string s1))\n           (= \"2\" (get-output-stream-string s2)))))\n\n  (deftest write.4a\n    (signals-error\n     (write \"foo\" :stream)\n     match-error))\n\n  (deftest write.4b\n    (signals-error\n     (write \"foo\" :stream 12)\n     type-error :datum 12 :expected-type 'output-stream)))\n\n(defsuite write-to-string\n\n  (deftest write-to-string.1\n    (write-to-string 12)\n    \"12\")\n\n  (deftest write-to-string.2\n    (write-to-string \"foo\")\n    \"\\\"foo\\\"\")\n\n  (deftest write-to-string.3\n    (signals-error\n     (write-to-string)\n     match-error)))\n\n(defsuite print\n\n  (deftest print.1\n    (with-standard-output-to-string (print \"foo\"))\n    \"\\n\\\"foo\\\"\")\n\n  (deftest print.1a\n    (with-standard-output-to-string\n      (assert (= \"foo\" (print \"foo\"))))\n    \"\\n\\\"foo\\\"\")\n\n  (deftest print.2\n    (with-standard-output-to-string (print \"foo\") (print \"bar\"))\n    \"\\n\\\"foo\\\"\\n\\\"bar\\\"\")\n\n  (deftest print.3\n    (signals-error\n     (print)\n     match-error))\n\n  (deftest print1.1\n    (with-standard-output-to-string (print \"foo\") (print1 \"bar\"))\n    \"\\n\\\"foo\\\"\\\"bar\\\"\")\n\n  (deftest print1.1a\n    (with-standard-output-to-string\n      (assert (= \"foo\" (print1 \"foo\"))))\n    \"\\\"foo\\\"\")\n\n  (deftest print1.2\n    (with-standard-output-to-string (print1 \"foo\") (print1 \"bar\"))\n    \"\\\"foo\\\"\\\"bar\\\"\")\n\n  (deftest print1.3\n    (signals-error\n     (print1)\n     match-error))\n\n  (deftest uprint.1\n    (with-standard-output-to-string (uprint \"foo\"))\n    \"\\nfoo\")\n\n  (deftest uprint.1a\n    (with-standard-output-to-string\n      (assert (= \"foo\" (uprint \"foo\"))))\n    \"\\nfoo\")\n\n  (deftest uprint.2\n    (with-standard-output-to-string (uprint \"foo\") (uprint \"bar\"))\n    \"\\nfoo\\nbar\")\n\n  (deftest uprint.3\n    (signals-error\n     (uprint)\n     match-error))\n\n  (deftest uprint1.1\n    (with-standard-output-to-string (uprint \"foo\") (uprint1 \"bar\"))\n    \"\\nfoobar\")\n\n  (deftest uprint1.1a\n    (with-standard-output-to-string\n      (assert (= \"foo\" (uprint1 \"foo\"))))\n    \"foo\")\n\n  (deftest uprint1.2\n    (signals-error\n     (uprint1)\n     match-error))\n\n  (deftest uprint1.3\n    (with-standard-output-to-string (uprint1 \"foo\") (uprint1 \"bar\"))\n    \"foobar\"))\n");
;// CONCATENATED MODULE: ./test/js-test.lispx
/* harmony default export */ const js_test_lispx = (";;; JavaScript Interface Tests\n\n(defsuite js-eq\n\n  (deftest js-eq.1\n    (and (js-eq +js-true+ +js-true+)\n         (js-eq +js-false+ +js-false+)\n         (not (js-eq +js-true+ +js-false+))\n         (js-eq (to-js-number 1) (to-js-number 1))\n         (not (js-eq (to-js-number 1) (to-js-number 2)))\n         (js-eq (to-js-string \"1\") (to-js-string \"1\"))\n         (not (js-eq (to-js-string \"1\") (to-js-string \"2\")))))\n\n  (deftest js-eq.2\n    (signals-error\n     (js-eq 1)\n     match-error))\n\n  (deftest js-eq.3\n    (signals-error\n     (js-eq)\n     match-error)))\n\n(defsuite null-and-undefined\n\n  (deftest null-and-undefined-exist\n    (and (eq (class-of +js-null+) #^object)\n         (eq (class-of +js-undefined+) #^object)))\n\n  (deftest null-and-undefined-distinguishable\n    (not (js-eq +js-null+ +js-undefined+))))\n\n(defsuite js-booleans\n\n  (deftest js-booleans-exist\n    (and\n     (eq (class-of +js-true+) #^object)\n     (eq (class-of +js-false+) #^object)))\n\n  (deftest js-booleans-identity\n    (and (not (eq +js-true+ +js-false+))\n         (eq +js-true+ +js-true+)\n         (eq +js-false+ +js-false+)\n         (not (eq +js-true+ #t))\n         (not (eq +js-false+ #f))))\n\n  (deftest to-lisp-boolean.1\n    (and (to-lisp-boolean +js-true+)\n         (not (to-lisp-boolean +js-false+))))\n\n  (deftest to-lisp-boolean.2\n    (signals-error\n     (to-lisp-boolean 12)\n     type-error :datum 12 :expected-type \"boolean\"))\n\n  (deftest to-lisp-boolean.3\n    (signals-error\n     (to-lisp-boolean)\n     match-error))\n\n  (deftest to-js-boolean.1\n    (and (to-lisp-boolean (to-js-boolean #t))\n         (not (to-lisp-boolean (to-js-boolean #f)))))\n\n  (deftest to-js-boolean.2\n    (signals-error\n     (to-js-boolean 12)\n     type-error :datum 12 :expected-type 'boolean))\n\n  (deftest to-js-boolean.3\n    (signals-error\n     (to-js-boolean)\n     match-error)))\n\n(defsuite js-numbers\n\n  (deftest to-lisp-number.1\n    (and\n     (= -1 (to-lisp-number (to-js-number -1)))\n     (= 0 (to-lisp-number (to-js-number 0)))\n     (= 1 (to-lisp-number (to-js-number 1)))))\n\n  (deftest to-lisp-number.2\n    (signals-error\n     (to-lisp-number #t)\n     type-error :datum #t :expected-type \"number\"))\n\n  (deftest to-lisp-number.3\n    (signals-error\n     (to-lisp-number)\n     match-error))\n\n  (deftest to-js-number.1\n    (eq (class-of (to-js-number 1))\n        #^object))\n\n  (deftest to-js-number.1a\n    (and (js-eq (to-js-number 1)\n                (to-js-number 1))\n         (not (js-eq (to-js-number 1)\n                     (to-js-number 2)))))\n\n  (deftest to-js-number.2\n    (signals-error\n     (to-js-number #t)\n     type-error :datum #t :expected-type 'number))\n\n  (deftest to-js-number.3\n    (signals-error\n     (to-js-number)\n     match-error)))\n\n(defsuite js-strings\n\n  (deftest to-lisp-string.1\n    (= \"a\" (to-lisp-string (to-js-string \"a\"))))\n\n  (deftest to-lisp-string.2\n    (signals-error\n     (to-lisp-string #t)\n     type-error :datum #t :expected-type \"string\"))\n\n  (deftest to-lisp-string.3\n    (signals-error\n     (to-lisp-string)\n     match-error))\n\n  (deftest to-js-string.1\n    (eq (class-of (to-js-string \"1\"))\n        #^object))\n\n  (deftest to-js-string.1a\n    (and (js-eq (to-js-string \"1\")\n                (to-js-string \"1\"))\n         (not (js-eq (to-js-string \"1\")\n                     (to-js-string \"2\")))))\n\n  (deftest to-js-string.2\n    (signals-error\n     (to-js-string #t)\n     type-error :datum #t :expected-type 'string))\n\n  (deftest to-js-string.3\n    (signals-error\n     (to-js-string)\n     match-error)))\n\n(defsuite to-lisp-function\n\n  (deftest to-lisp-function.1\n    (let ((#'parseInt (to-lisp-function (js-global \"parseInt\"))))\n      (js-eq (parseInt (to-js-string \"123\")) (to-js-number 123))))\n\n  (deftest to-lisp-function.1a\n    (let* ((#'parseInt (to-lisp-function (js-global \"parseInt\")))\n           (result (parseInt)))\n      (to-lisp-boolean (call-js-function (js-global \"isNaN\") result))))\n\n  (deftest to-lisp-function.2\n    (signals-error\n     (to-lisp-function 12)\n     type-error :datum 12 :expected-type \"function\")))\n\n(defsuite to-js-function\n\n  (deftest to-js-function.1\n    (let ((x 0))\n      (def env (the-environment))\n      (flet ((fun (v1 v2) (set env x (+ v1 v2))))\n        (call-js-function (to-js-function #'fun) 1 2))\n      (= x 3)))\n\n  (deftest to-js-function.2\n    (signals-error\n     (to-js-function 12)\n     type-error :datum 12 :expected-type 'operator)))\n\n(defsuite apply-js-function\n\n  (deftest apply-js-function.1\n    (js-eq (apply-js-function (js-global \"parseInt\") (list (to-js-string \"12\")))\n           (to-js-number 12)))\n\n  (deftest apply-js-function.2\n    (signals-error\n     (apply-js-function 12 (list))\n     type-error :datum 12 :expected-type \"function\"))\n\n  (deftest apply-js-function.3\n    (signals-error\n     (apply-js-function (js-global \"parseInt\"))\n     match-error))\n\n  (deftest apply-js-function.4\n    (signals-error\n     (apply-js-function)\n     match-error)))\n\n(defsuite call-js-function\n\n  (deftest call-js-function.1\n    (js-eq (call-js-function (js-global \"parseInt\") (to-js-string \"12\"))\n           (to-js-number 12)))\n\n  (deftest call-js-function.2\n    (signals-error\n     (call-js-function 12)\n     type-error :datum 12 :expected-type \"function\"))\n\n  (deftest call-js-function.3\n    (signals-error\n     (call-js-function)\n     match-error)))\n\n(defsuite js-lambda\n\n  (deftest js-lambda.1\n    (let ((x 0))\n      (def env (the-environment))\n      (let ((fun (js-lambda (v1 v2) (set env x (+ v1 v2)))))\n        (call-js-function fun 1 2))\n      (= x 3)))\n\n  (deftest js-lambda.2\n    (signals-error\n     (js-lambda)\n     match-error))\n\n  (deftest js-lambda.3\n    (signals-error\n     (js-lambda 12)\n     type-error :datum 12 :expected-type '(or symbol ignore list)))\n\n  (deftest js-lambda.4\n    (call-js-function (js-lambda ()))\n    #void)\n\n  (deftest js-lambda.root-prompt-is-set\n    (call-js-function (js-lambda () (take-subcont +root-prompt+ #ignore)))\n    #void)\n\n  (deftest js-lambda.barrier\n    (signals-error\n     (push-prompt 'foo\n       (call-js-function (js-lambda () (take-subcont 'foo k))))\n     prompt-not-found-error :prompt 'foo)))\n\n(defsuite js-global\n\n  (deftest js-global.1\n    (let ((Math (js-global \"Math\")))\n      (= 4 (to-lisp-number (call-js-method Math \"round\" (to-js-number 4.1))))))\n\n  (deftest js-global.2\n    (signals-error\n     (js-global)\n     match-error))\n\n  (deftest js-global.3\n    (progn\n      (js-eq +js-undefined+ (js-global \"ThisGlobalDoesNotExist\"))\n      (js-set-global \"ThisGlobalDoesNotExist\" (+ 12 12))\n      (= 24 (js-global \"ThisGlobalDoesNotExist\"))))\n\n  (deftest js-global.4\n    (signals-error\n     (js-global 12)\n     type-error :datum 12 :expected-type 'string)))\n\n(defsuite js-new\n\n  (deftest js-new.1\n    (let ((re (js-new (js-global \"RegExp\") (to-js-string \"abc\"))))\n      (and (to-lisp-boolean (call-js-method re \"test\" (to-js-string \"abcdef\")))\n           (not (to-lisp-boolean (call-js-method re \"test\" (to-js-string \"uvwxyz\")))))))\n\n  (deftest js-new.2\n    (signals-error\n     (js-new)\n     match-error))\n\n  (deftest js-new.3\n    (signals-error\n     (js-new \"foo\")\n     type-error :datum \"foo\" :expected-type \"function\")))\n\n(defsuite js-get\n\n  (deftest js-get.1\n    (let ((Math (js-global \"Math\")))\n      (to-lisp-string (call-js-method (js-get Math \"PI\") \"toString\")))\n    \"3.141592653589793\")\n\n  (deftest js-get.2\n    (signals-error\n     (js-get)\n     match-error))\n\n  (deftest js-get.3\n    (signals-error\n     (js-get 12)\n     match-error))\n\n  (deftest js-get.4\n    (signals-error\n     (js-get 12 'bar)\n     type-error :datum 'bar :expected-type 'string)))\n\n(defsuite js-arrays\n\n  (define-js-method array-push \"push\")\n  (define-js-method array-pop \"pop\")\n\n  (deftest js-array.1\n    (let ((arr (js-array)))\n      (assert (= (to-js-number 0) (js-get arr \"length\")))\n      (array-push arr \"a\")\n      (assert (= (to-js-number 1) (js-get arr \"length\")))\n      (array-push arr \"b\")\n      (assert (= (to-js-number 2) (js-get arr \"length\")))\n      (assert (= \"b\" (array-pop arr)))\n      (assert (= (to-js-number 1) (js-get arr \"length\")))\n      (assert (= \"a\" (array-pop arr)))\n      (assert (= (to-js-number 0) (js-get arr \"length\")))\n      #t))\n\n    (deftest js-array.2\n      (let ((arr (js-array (+ 1 1) (+ 2 2))))\n        (assert (= (to-js-number 2) (js-get arr \"length\")))\n        (assert (= 4 (array-pop arr)))\n        (assert (= 2 (array-pop arr)))\n        #t))\n\n    (deftest list-to-js-array.1\n      (js-array-to-list (list-to-js-array '(1 2 3)))\n      '(1 2 3))\n\n    (deftest list-to-js-array.2\n      (signals-error\n       (list-to-js-array)\n       match-error))\n\n    (deftest list-to-js-array.3\n      (signals-error\n       (list-to-js-array 12)\n       type-error :datum 12 :expected-type 'list))\n\n    (deftest js-array-to-list.1\n      (signals-error\n       (js-array-to-list)\n       match-error))\n\n    (deftest js-array-to-list.2\n      (signals-error\n       (js-array-to-list 12)\n       assertion-error))\n\n    (deftest js-array-to-list.3\n      (js-array-to-list (js-array 1 2 3))\n      '(1 2 3))\n\n    (deftest js-array-elt.1\n      (elt (js-array 1 2 3) 0)\n      1)\n\n    (deftest js-array-elt.2\n      (elt (js-array 1 2 3) 1)\n      2)\n\n    (deftest js-array-elt.3\n      (signals-error\n       (elt (js-array 1 2 3) 10)\n       out-of-bounds-error))\n\n    (deftest js-array-elt.4\n      (signals-error\n       (elt (js-array 1 2 3) -1)\n       out-of-bounds-error))\n\n    (deftest js-array-length.1\n      (length (js-array))\n      0)\n\n    (deftest js-array-length.2\n      (length (js-array 1 2 3))\n      3))\n\n(defsuite apply-js-method\n\n  (deftest apply-js-method.1\n    (js-eq\n     (apply-js-method (to-js-number 9.656) \"toFixed\" (list (to-js-number 2)))\n     (to-js-string \"9.66\")))\n\n  (deftest apply-js-method.2\n    (js-eq\n     (apply-js-method (to-js-number 9) \"toString\" (list))\n     (to-js-string \"9\")))\n\n  (deftest apply-js-method.3\n    (signals-error\n     (apply-js-method)\n     match-error))\n\n  (deftest apply-js-method.4\n    (signals-error\n     (apply-js-method \"foo\")\n     match-error))\n\n  (deftest apply-js-method.4a\n    (signals-error\n     (apply-js-method \"foo\" \"bar\")\n     match-error))\n\n  (deftest apply-js-method.5\n    (signals-error\n     (apply-js-method \"foo\" \"methodDoesNotExist\" (list))\n     type-error :expected-type \"function\"))\n\n  (deftest apply-js-method.6\n    (signals-error\n     (apply-js-method \"foo\" 12 (list))\n     type-error :datum 12 :expected-type 'string)))\n\n(defsuite call-js-method\n\n  (deftest call-js-method.1\n    (js-eq\n     (call-js-method (to-js-number 9.656) \"toFixed\" (to-js-number 2))\n     (to-js-string \"9.66\")))\n\n  (deftest call-js-method.2\n    (js-eq\n     (call-js-method (to-js-number 9) \"toString\")\n     (to-js-string \"9\")))\n\n  (deftest call-js-method.3\n    (signals-error\n     (call-js-method)\n     match-error))\n\n  (deftest call-js-method.4\n    (signals-error\n     (call-js-method \"foo\")\n     match-error))\n\n  (deftest call-js-method.5\n    (signals-error\n     (call-js-method \"foo\" \"methodDoesNotExist\")\n     type-error :expected-type \"function\"))\n\n  (deftest call-js-method.6\n    (signals-error\n     (call-js-method \"foo\" 12)\n     type-error :datum 12 :expected-type 'string)))\n\n(defsuite js-method\n\n  (deftest js-method.1\n    (progn\n      (def #'to-string (js-method \"toString\"))\n      (js-eq (to-string (to-js-number 12)) (to-js-string \"12\"))))\n\n  (deftest js-method.2\n    (signals-error\n     (js-method)\n     match-error)))\n\n(defsuite define-js-method\n\n  (deftest define-js-method.1\n    (progn\n      (define-js-method to-string \"toString\")\n      (js-eq (to-string (to-js-number 12)) (to-js-string \"12\"))))\n\n  (deftest define-js-method.2\n    (progn\n      (define-js-method regexp-test \"test\")\n      (let ((re (js-new (js-global \"RegExp\") (to-js-string \"abc\"))))\n        (and (to-lisp-boolean (regexp-test re (to-js-string \"abcdef\")))\n             (not (to-lisp-boolean (regexp-test re (to-js-string \"uvwxyz\"))))))))\n\n  (deftest define-js-method.3\n    (signals-error\n     (define-js-method)\n     match-error))\n\n  (deftest define-js-method.4\n    (signals-error\n     (define-js-method symbol)\n     match-error)))\n\n(defsuite js-undefined-option\n\n  (deftest js-undefined-option.1\n    (js-undefined-option (+ 12 12))\n    (some 24))\n\n  (deftest js-undefined-option.2\n    (js-undefined-option +js-undefined+)\n    #nil)\n\n  (deftest js-undefined-option.3\n    (signals-error\n     (js-undefined-option)\n     match-error)))\n\n(defsuite js-null-option\n\n  (deftest js-null-option.1\n    (js-null-option (+ 12 12))\n    (some 24))\n\n  (deftest js-null-option.2\n    (js-null-option +js-null+)\n    #nil)\n\n  (deftest js-null-option.3\n    (signals-error\n     (js-null-option)\n     match-error)))\n\n(defsuite await\n\n  (def Promise (js-global \"Promise\"))\n  (define-js-method resolve \"resolve\")\n  (define-js-method reject \"reject\")\n\n  (deftest await.1\n    (progn\n      (assert (= 1 (await (resolve Promise 1))))\n      (assert (= 2 (await (resolve Promise 2))))\n      3)\n    3)\n\n  (deftest await.2\n    (signals-error\n     (progn\n       (assert (= 1 (await (resolve Promise 1))))\n       (await (reject Promise (make-simple-error \"foo\"))))\n     simple-error :message \"foo\"))\n\n  (deftest await.3\n    (signals-error\n     (await)\n     match-error)))\n\n(defsuite sleep\n\n  (deftest sleep.1\n    (progn (sleep 1) #t))\n\n  (deftest sleep.2\n    (signals-error\n     (sleep)\n     match-error))\n\n  (deftest sleep.3\n    (signals-error\n     (sleep \"foo\")\n     type-error :datum \"foo\" :expected-type 'number)))\n\n(defsuite sync\n\n  (deftest sync.1\n    (signals-error\n     (sync)\n     match-error))\n\n  (deftest sync.2\n    (prog1 #t\n      ((sync #'%sleep) 1)))\n\n  (def Promise (js-global \"Promise\"))\n  (define-js-method resolve \"resolve\")\n  (define-js-method reject \"reject\")\n\n  (deftest sync.3\n    (progn\n      (assert (= 1 ((sync (lambda () (resolve Promise 1))))))\n      (assert (= 2 ((sync (lambda () (resolve Promise 2))))))\n      3)\n    3)\n\n  (deftest sync.4\n    (signals-error\n     (progn\n       (assert (= 1 ((sync (lambda () (resolve Promise 1))))))\n       ((sync (lambda () (reject Promise (make-simple-error \"foo\"))))))\n     simple-error :message \"foo\")))\n\n(defsuite define-js-method/sync\n\n  (def Promise (js-global \"Promise\"))\n\n  (deftest define-js-method/sync.1\n    (progn\n      (define-js-method/sync resolve-sync \"resolve\")\n      (= (resolve-sync Promise 12) 12)))\n\n  (deftest define-js-method/sync.2\n    (progn\n      (define-js-method/sync reject-sync \"reject\")\n      (signals-error\n       (reject-sync Promise (make-simple-error \"foo\"))\n       simple-error :message \"foo\"))))\n\n(defsuite js-misc\n\n  (deftest can-define-lisp-methods-on-js-objects\n    (progn\n      (defgeneric foo (obj))\n      (defmethod foo ((obj object)) 12)\n      (assert (= 12 (foo +js-null+)))\n      (assert (= 12 (foo (to-js-string \"foo\")))))\n    #void)\n\n  (deftest js-exception-becomes-condition\n    (block exit\n      (handler-bind ((object (lambda (c)\n                               (let ((message (call-js-method c \"toString\")))\n                                 (flet ((js-string-contains-p (string substring)\n                                          (not (js-eq (call-js-method string \"indexOf\" substring)\n                                                      (to-js-number -1)))))\n                                   (when (or (js-string-contains-p\n                                              message\n                                              (to-js-string \"ReferenceError: x is not defined\"))\n                                             (js-string-contains-p\n                                              message\n                                              (to-js-string \"Can't find variable: x\")))\n                                     (return-from exit #t)))))))\n        (call-js-function (js-global \"eval\") (to-js-string \"x\"))))))\n");
;// CONCATENATED MODULE: ./test/hierarchy-test.lispx
/* harmony default export */ const hierarchy_test_lispx = (";;; Class Hierarchy Integrity Test\n\n(defsuite class-hierarchy\n\n  (defun test-hierarchy (classes)\n    (let ((env (the-environment)))\n      (loop-let -test-classes- ((classes classes) (superclass #^object))\n        (dolist (c classes #t)\n          (etypecase c\n            (symbol\n             (let ((class (find-class c env)))\n               (assert (subclassp class superclass))))\n            (cons\n             (let ((class (find-class (car c) env)))\n               (assert (subclassp class superclass))\n               (-test-classes- (cdr c) class))))))))\n\n  (deftest test-hierarchy\n    (test-hierarchy\n     '(string\n       symbol\n       number\n       boolean\n       (list cons\n             nil)\n       void\n       ignore\n       environment\n       (class built-in-class\n              standard-class)\n       (operator built-in-operator\n                 fexpr\n                 function)\n       (input-stream string-input-stream)\n       (output-stream string-output-stream\n                      js-console-output-stream)\n       (standard-object dynamic\n                        (condition (error type-error\n                                          unbound-symbol-error\n                                          unbound-slot-error\n                                          unbound-method-error\n                                          assertion-error\n                                          match-error\n                                          (stream-error end-of-file)\n                                          reader-error\n                                          prompt-not-found-error\n                                          restart-error\n                                          simple-error))\n                        handler-frame\n                        restart-handler\n                        condition-handler)\n       continuation)))\n\n  (deftest test-hierarchy.sanity-check\n    (signals-error\n     (test-hierarchy\n      '((number\n         boolean)))\n     assertion-error)))\n");
;// CONCATENATED MODULE: ./test/lisp-tests.mjs














describe("Lisp Tests", () => {

    const vm = (0,external_lispx_vm_umd_min_js_.make_vm)();
    vm.eval_js_string(test_util_lispx);
    vm.eval_js_string(test_util_test_lispx);
    vm.eval_js_string(boot_test_lispx);
    vm.eval_js_string(cond_sys_test_lispx);
    vm.eval_js_string(control_test_lispx);
    vm.eval_js_string(delimcc_test_lispx);
    vm.eval_js_string(stream_test_lispx);
    vm.eval_js_string(read_test_lispx);
    vm.eval_js_string(print_test_lispx);
    vm.eval_js_string(js_test_lispx);
    vm.eval_js_string(hierarchy_test_lispx);

}).timeout(100000);

;// CONCATENATED MODULE: ./node_modules/chai/chai.js
var __defProp = Object.defineProperty;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __name = (target, value) => __defProp(target, "name", { value, configurable: true });
var __commonJS = (cb, mod) => function __require() {
  return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
};
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};

// (disabled):util
var require_util = __commonJS({
  "(disabled):util"() {
  }
});

// lib/chai/utils/index.js
var utils_exports = {};
__export(utils_exports, {
  addChainableMethod: () => addChainableMethod,
  addLengthGuard: () => addLengthGuard,
  addMethod: () => addMethod,
  addProperty: () => addProperty,
  checkError: () => check_error_exports,
  compareByInspect: () => compareByInspect,
  eql: () => deep_eql_default,
  expectTypes: () => expectTypes,
  flag: () => flag,
  getActual: () => getActual,
  getMessage: () => getMessage2,
  getName: () => getName,
  getOperator: () => getOperator,
  getOwnEnumerableProperties: () => getOwnEnumerableProperties,
  getOwnEnumerablePropertySymbols: () => getOwnEnumerablePropertySymbols,
  getPathInfo: () => getPathInfo,
  hasProperty: () => hasProperty,
  inspect: () => inspect2,
  isNaN: () => isNaN2,
  isProxyEnabled: () => isProxyEnabled,
  objDisplay: () => objDisplay,
  overwriteChainableMethod: () => overwriteChainableMethod,
  overwriteMethod: () => overwriteMethod,
  overwriteProperty: () => overwriteProperty,
  proxify: () => proxify,
  test: () => test,
  transferFlags: () => transferFlags,
  type: () => type
});

// node_modules/check-error/index.js
var check_error_exports = {};
__export(check_error_exports, {
  compatibleConstructor: () => compatibleConstructor,
  compatibleInstance: () => compatibleInstance,
  compatibleMessage: () => compatibleMessage,
  getConstructorName: () => getConstructorName,
  getMessage: () => getMessage
});
function compatibleInstance(thrown, errorLike) {
  return errorLike instanceof Error && thrown === errorLike;
}
__name(compatibleInstance, "compatibleInstance");
function compatibleConstructor(thrown, errorLike) {
  if (errorLike instanceof Error) {
    return thrown.constructor === errorLike.constructor || thrown instanceof errorLike.constructor;
  } else if (errorLike.prototype instanceof Error || errorLike === Error) {
    return thrown.constructor === errorLike || thrown instanceof errorLike;
  }
  return false;
}
__name(compatibleConstructor, "compatibleConstructor");
function compatibleMessage(thrown, errMatcher) {
  const comparisonString = typeof thrown === "string" ? thrown : thrown.message;
  if (errMatcher instanceof RegExp) {
    return errMatcher.test(comparisonString);
  } else if (typeof errMatcher === "string") {
    return comparisonString.indexOf(errMatcher) !== -1;
  }
  return false;
}
__name(compatibleMessage, "compatibleMessage");
function getConstructorName(errorLike) {
  let constructorName = errorLike;
  if (errorLike instanceof Error) {
    constructorName = errorLike.constructor.name;
  } else if (typeof errorLike === "function") {
    constructorName = errorLike.name;
    if (constructorName === "") {
      const newConstructorName = new errorLike().name;
      constructorName = newConstructorName || constructorName;
    }
  }
  return constructorName;
}
__name(getConstructorName, "getConstructorName");
function getMessage(errorLike) {
  let msg = "";
  if (errorLike && errorLike.message) {
    msg = errorLike.message;
  } else if (typeof errorLike === "string") {
    msg = errorLike;
  }
  return msg;
}
__name(getMessage, "getMessage");

// lib/chai/utils/flag.js
function flag(obj, key, value) {
  var flags = obj.__flags || (obj.__flags = /* @__PURE__ */ Object.create(null));
  if (arguments.length === 3) {
    flags[key] = value;
  } else {
    return flags[key];
  }
}
__name(flag, "flag");

// lib/chai/utils/test.js
function test(obj, args) {
  var negate = flag(obj, "negate"), expr = args[0];
  return negate ? !expr : expr;
}
__name(test, "test");

// lib/chai/utils/type-detect.js
function type(obj) {
  if (typeof obj === "undefined") {
    return "undefined";
  }
  if (obj === null) {
    return "null";
  }
  const stringTag = obj[Symbol.toStringTag];
  if (typeof stringTag === "string") {
    return stringTag;
  }
  const type3 = Object.prototype.toString.call(obj).slice(8, -1);
  return type3;
}
__name(type, "type");

// node_modules/assertion-error/index.js
var canElideFrames = "captureStackTrace" in Error;
var AssertionError = class _AssertionError extends Error {
  static {
    __name(this, "AssertionError");
  }
  message;
  get name() {
    return "AssertionError";
  }
  get ok() {
    return false;
  }
  constructor(message = "Unspecified AssertionError", props, ssf) {
    super(message);
    this.message = message;
    if (canElideFrames) {
      Error.captureStackTrace(this, ssf || _AssertionError);
    }
    for (const key in props) {
      if (!(key in this)) {
        this[key] = props[key];
      }
    }
  }
  toJSON(stack) {
    return {
      ...this,
      name: this.name,
      message: this.message,
      ok: false,
      stack: stack !== false ? this.stack : void 0
    };
  }
};

// lib/chai/utils/expectTypes.js
function expectTypes(obj, types) {
  var flagMsg = flag(obj, "message");
  var ssfi = flag(obj, "ssfi");
  flagMsg = flagMsg ? flagMsg + ": " : "";
  obj = flag(obj, "object");
  types = types.map(function(t) {
    return t.toLowerCase();
  });
  types.sort();
  var str = types.map(function(t, index) {
    var art = ~["a", "e", "i", "o", "u"].indexOf(t.charAt(0)) ? "an" : "a";
    var or = types.length > 1 && index === types.length - 1 ? "or " : "";
    return or + art + " " + t;
  }).join(", ");
  var objType = type(obj).toLowerCase();
  if (!types.some(function(expected) {
    return objType === expected;
  })) {
    throw new AssertionError(
      flagMsg + "object tested must be " + str + ", but " + objType + " given",
      void 0,
      ssfi
    );
  }
}
__name(expectTypes, "expectTypes");

// lib/chai/utils/getActual.js
function getActual(obj, args) {
  return args.length > 4 ? args[4] : obj._obj;
}
__name(getActual, "getActual");

// node_modules/loupe/lib/helpers.js
var ansiColors = {
  bold: ["1", "22"],
  dim: ["2", "22"],
  italic: ["3", "23"],
  underline: ["4", "24"],
  // 5 & 6 are blinking
  inverse: ["7", "27"],
  hidden: ["8", "28"],
  strike: ["9", "29"],
  // 10-20 are fonts
  // 21-29 are resets for 1-9
  black: ["30", "39"],
  red: ["31", "39"],
  green: ["32", "39"],
  yellow: ["33", "39"],
  blue: ["34", "39"],
  magenta: ["35", "39"],
  cyan: ["36", "39"],
  white: ["37", "39"],
  brightblack: ["30;1", "39"],
  brightred: ["31;1", "39"],
  brightgreen: ["32;1", "39"],
  brightyellow: ["33;1", "39"],
  brightblue: ["34;1", "39"],
  brightmagenta: ["35;1", "39"],
  brightcyan: ["36;1", "39"],
  brightwhite: ["37;1", "39"],
  grey: ["90", "39"]
};
var styles = {
  special: "cyan",
  number: "yellow",
  bigint: "yellow",
  boolean: "yellow",
  undefined: "grey",
  null: "bold",
  string: "green",
  symbol: "green",
  date: "magenta",
  regexp: "red"
};
var truncator = "\u2026";
function colorise(value, styleType) {
  const color = ansiColors[styles[styleType]] || ansiColors[styleType] || "";
  if (!color) {
    return String(value);
  }
  return `\x1B[${color[0]}m${String(value)}\x1B[${color[1]}m`;
}
__name(colorise, "colorise");
function normaliseOptions({
  showHidden = false,
  depth = 2,
  colors = false,
  customInspect = true,
  showProxy = false,
  maxArrayLength = Infinity,
  breakLength = Infinity,
  seen = [],
  // eslint-disable-next-line no-shadow
  truncate: truncate2 = Infinity,
  stylize = String
} = {}, inspect3) {
  const options = {
    showHidden: Boolean(showHidden),
    depth: Number(depth),
    colors: Boolean(colors),
    customInspect: Boolean(customInspect),
    showProxy: Boolean(showProxy),
    maxArrayLength: Number(maxArrayLength),
    breakLength: Number(breakLength),
    truncate: Number(truncate2),
    seen,
    inspect: inspect3,
    stylize
  };
  if (options.colors) {
    options.stylize = colorise;
  }
  return options;
}
__name(normaliseOptions, "normaliseOptions");
function truncate(string, length, tail = truncator) {
  string = String(string);
  const tailLength = tail.length;
  const stringLength = string.length;
  if (tailLength > length && stringLength > tailLength) {
    return tail;
  }
  if (stringLength > length && stringLength > tailLength) {
    return `${string.slice(0, length - tailLength)}${tail}`;
  }
  return string;
}
__name(truncate, "truncate");
function inspectList(list, options, inspectItem, separator = ", ") {
  inspectItem = inspectItem || options.inspect;
  const size = list.length;
  if (size === 0)
    return "";
  const originalLength = options.truncate;
  let output = "";
  let peek = "";
  let truncated = "";
  for (let i = 0; i < size; i += 1) {
    const last = i + 1 === list.length;
    const secondToLast = i + 2 === list.length;
    truncated = `${truncator}(${list.length - i})`;
    const value = list[i];
    options.truncate = originalLength - output.length - (last ? 0 : separator.length);
    const string = peek || inspectItem(value, options) + (last ? "" : separator);
    const nextLength = output.length + string.length;
    const truncatedLength = nextLength + truncated.length;
    if (last && nextLength > originalLength && output.length + truncated.length <= originalLength) {
      break;
    }
    if (!last && !secondToLast && truncatedLength > originalLength) {
      break;
    }
    peek = last ? "" : inspectItem(list[i + 1], options) + (secondToLast ? "" : separator);
    if (!last && secondToLast && truncatedLength > originalLength && nextLength + peek.length > originalLength) {
      break;
    }
    output += string;
    if (!last && !secondToLast && nextLength + peek.length >= originalLength) {
      truncated = `${truncator}(${list.length - i - 1})`;
      break;
    }
    truncated = "";
  }
  return `${output}${truncated}`;
}
__name(inspectList, "inspectList");
function quoteComplexKey(key) {
  if (key.match(/^[a-zA-Z_][a-zA-Z_0-9]*$/)) {
    return key;
  }
  return JSON.stringify(key).replace(/'/g, "\\'").replace(/\\"/g, '"').replace(/(^"|"$)/g, "'");
}
__name(quoteComplexKey, "quoteComplexKey");
function inspectProperty([key, value], options) {
  options.truncate -= 2;
  if (typeof key === "string") {
    key = quoteComplexKey(key);
  } else if (typeof key !== "number") {
    key = `[${options.inspect(key, options)}]`;
  }
  options.truncate -= key.length;
  value = options.inspect(value, options);
  return `${key}: ${value}`;
}
__name(inspectProperty, "inspectProperty");

// node_modules/loupe/lib/array.js
function inspectArray(array, options) {
  const nonIndexProperties = Object.keys(array).slice(array.length);
  if (!array.length && !nonIndexProperties.length)
    return "[]";
  options.truncate -= 4;
  const listContents = inspectList(array, options);
  options.truncate -= listContents.length;
  let propertyContents = "";
  if (nonIndexProperties.length) {
    propertyContents = inspectList(nonIndexProperties.map((key) => [key, array[key]]), options, inspectProperty);
  }
  return `[ ${listContents}${propertyContents ? `, ${propertyContents}` : ""} ]`;
}
__name(inspectArray, "inspectArray");

// node_modules/loupe/lib/typedarray.js
var getArrayName = /* @__PURE__ */ __name((array) => {
  if (typeof Buffer === "function" && array instanceof Buffer) {
    return "Buffer";
  }
  if (array[Symbol.toStringTag]) {
    return array[Symbol.toStringTag];
  }
  return array.constructor.name;
}, "getArrayName");
function inspectTypedArray(array, options) {
  const name = getArrayName(array);
  options.truncate -= name.length + 4;
  const nonIndexProperties = Object.keys(array).slice(array.length);
  if (!array.length && !nonIndexProperties.length)
    return `${name}[]`;
  let output = "";
  for (let i = 0; i < array.length; i++) {
    const string = `${options.stylize(truncate(array[i], options.truncate), "number")}${i === array.length - 1 ? "" : ", "}`;
    options.truncate -= string.length;
    if (array[i] !== array.length && options.truncate <= 3) {
      output += `${truncator}(${array.length - array[i] + 1})`;
      break;
    }
    output += string;
  }
  let propertyContents = "";
  if (nonIndexProperties.length) {
    propertyContents = inspectList(nonIndexProperties.map((key) => [key, array[key]]), options, inspectProperty);
  }
  return `${name}[ ${output}${propertyContents ? `, ${propertyContents}` : ""} ]`;
}
__name(inspectTypedArray, "inspectTypedArray");

// node_modules/loupe/lib/date.js
function inspectDate(dateObject, options) {
  const stringRepresentation = dateObject.toJSON();
  if (stringRepresentation === null) {
    return "Invalid Date";
  }
  const split = stringRepresentation.split("T");
  const date = split[0];
  return options.stylize(`${date}T${truncate(split[1], options.truncate - date.length - 1)}`, "date");
}
__name(inspectDate, "inspectDate");

// node_modules/loupe/lib/function.js
function inspectFunction(func, options) {
  const functionType = func[Symbol.toStringTag] || "Function";
  const name = func.name;
  if (!name) {
    return options.stylize(`[${functionType}]`, "special");
  }
  return options.stylize(`[${functionType} ${truncate(name, options.truncate - 11)}]`, "special");
}
__name(inspectFunction, "inspectFunction");

// node_modules/loupe/lib/map.js
function inspectMapEntry([key, value], options) {
  options.truncate -= 4;
  key = options.inspect(key, options);
  options.truncate -= key.length;
  value = options.inspect(value, options);
  return `${key} => ${value}`;
}
__name(inspectMapEntry, "inspectMapEntry");
function mapToEntries(map) {
  const entries = [];
  map.forEach((value, key) => {
    entries.push([key, value]);
  });
  return entries;
}
__name(mapToEntries, "mapToEntries");
function inspectMap(map, options) {
  const size = map.size - 1;
  if (size <= 0) {
    return "Map{}";
  }
  options.truncate -= 7;
  return `Map{ ${inspectList(mapToEntries(map), options, inspectMapEntry)} }`;
}
__name(inspectMap, "inspectMap");

// node_modules/loupe/lib/number.js
var chai_isNaN = Number.isNaN || ((i) => i !== i);
function inspectNumber(number, options) {
  if (chai_isNaN(number)) {
    return options.stylize("NaN", "number");
  }
  if (number === Infinity) {
    return options.stylize("Infinity", "number");
  }
  if (number === -Infinity) {
    return options.stylize("-Infinity", "number");
  }
  if (number === 0) {
    return options.stylize(1 / number === Infinity ? "+0" : "-0", "number");
  }
  return options.stylize(truncate(String(number), options.truncate), "number");
}
__name(inspectNumber, "inspectNumber");

// node_modules/loupe/lib/bigint.js
function inspectBigInt(number, options) {
  let nums = truncate(number.toString(), options.truncate - 1);
  if (nums !== truncator)
    nums += "n";
  return options.stylize(nums, "bigint");
}
__name(inspectBigInt, "inspectBigInt");

// node_modules/loupe/lib/regexp.js
function inspectRegExp(value, options) {
  const flags = value.toString().split("/")[2];
  const sourceLength = options.truncate - (2 + flags.length);
  const source = value.source;
  return options.stylize(`/${truncate(source, sourceLength)}/${flags}`, "regexp");
}
__name(inspectRegExp, "inspectRegExp");

// node_modules/loupe/lib/set.js
function arrayFromSet(set2) {
  const values = [];
  set2.forEach((value) => {
    values.push(value);
  });
  return values;
}
__name(arrayFromSet, "arrayFromSet");
function inspectSet(set2, options) {
  if (set2.size === 0)
    return "Set{}";
  options.truncate -= 7;
  return `Set{ ${inspectList(arrayFromSet(set2), options)} }`;
}
__name(inspectSet, "inspectSet");

// node_modules/loupe/lib/string.js
var stringEscapeChars = new RegExp("['\\u0000-\\u001f\\u007f-\\u009f\\u00ad\\u0600-\\u0604\\u070f\\u17b4\\u17b5\\u200c-\\u200f\\u2028-\\u202f\\u2060-\\u206f\\ufeff\\ufff0-\\uffff]", "g");
var escapeCharacters = {
  "\b": "\\b",
  "	": "\\t",
  "\n": "\\n",
  "\f": "\\f",
  "\r": "\\r",
  "'": "\\'",
  "\\": "\\\\"
};
var hex = 16;
var unicodeLength = 4;
function chai_escape(char) {
  return escapeCharacters[char] || `\\u${`0000${char.charCodeAt(0).toString(hex)}`.slice(-unicodeLength)}`;
}
__name(chai_escape, "escape");
function inspectString(string, options) {
  if (stringEscapeChars.test(string)) {
    string = string.replace(stringEscapeChars, chai_escape);
  }
  return options.stylize(`'${truncate(string, options.truncate - 2)}'`, "string");
}
__name(inspectString, "inspectString");

// node_modules/loupe/lib/symbol.js
function inspectSymbol(value) {
  if ("description" in Symbol.prototype) {
    return value.description ? `Symbol(${value.description})` : "Symbol()";
  }
  return value.toString();
}
__name(inspectSymbol, "inspectSymbol");

// node_modules/loupe/lib/promise.js
var getPromiseValue = /* @__PURE__ */ __name(() => "Promise{\u2026}", "getPromiseValue");
try {
  const { getPromiseDetails, kPending, kRejected } = process.binding("util");
  if (Array.isArray(getPromiseDetails(Promise.resolve()))) {
    getPromiseValue = /* @__PURE__ */ __name((value, options) => {
      const [state, innerValue] = getPromiseDetails(value);
      if (state === kPending) {
        return "Promise{<pending>}";
      }
      return `Promise${state === kRejected ? "!" : ""}{${options.inspect(innerValue, options)}}`;
    }, "getPromiseValue");
  }
} catch (notNode) {
}
var promise_default = getPromiseValue;

// node_modules/loupe/lib/object.js
function inspectObject(object, options) {
  const properties = Object.getOwnPropertyNames(object);
  const symbols = Object.getOwnPropertySymbols ? Object.getOwnPropertySymbols(object) : [];
  if (properties.length === 0 && symbols.length === 0) {
    return "{}";
  }
  options.truncate -= 4;
  options.seen = options.seen || [];
  if (options.seen.indexOf(object) >= 0) {
    return "[Circular]";
  }
  options.seen.push(object);
  const propertyContents = inspectList(properties.map((key) => [key, object[key]]), options, inspectProperty);
  const symbolContents = inspectList(symbols.map((key) => [key, object[key]]), options, inspectProperty);
  options.seen.pop();
  let sep = "";
  if (propertyContents && symbolContents) {
    sep = ", ";
  }
  return `{ ${propertyContents}${sep}${symbolContents} }`;
}
__name(inspectObject, "inspectObject");

// node_modules/loupe/lib/class.js
var toStringTag = typeof Symbol !== "undefined" && Symbol.toStringTag ? Symbol.toStringTag : false;
function inspectClass(value, options) {
  let name = "";
  if (toStringTag && toStringTag in value) {
    name = value[toStringTag];
  }
  name = name || value.constructor.name;
  if (!name || name === "_class") {
    name = "<Anonymous Class>";
  }
  options.truncate -= name.length;
  return `${name}${inspectObject(value, options)}`;
}
__name(inspectClass, "inspectClass");

// node_modules/loupe/lib/arguments.js
function inspectArguments(args, options) {
  if (args.length === 0)
    return "Arguments[]";
  options.truncate -= 13;
  return `Arguments[ ${inspectList(args, options)} ]`;
}
__name(inspectArguments, "inspectArguments");

// node_modules/loupe/lib/error.js
var errorKeys = [
  "stack",
  "line",
  "column",
  "name",
  "message",
  "fileName",
  "lineNumber",
  "columnNumber",
  "number",
  "description"
];
function inspectObject2(error, options) {
  const properties = Object.getOwnPropertyNames(error).filter((key) => errorKeys.indexOf(key) === -1);
  const name = error.name;
  options.truncate -= name.length;
  let message = "";
  if (typeof error.message === "string") {
    message = truncate(error.message, options.truncate);
  } else {
    properties.unshift("message");
  }
  message = message ? `: ${message}` : "";
  options.truncate -= message.length + 5;
  const propertyContents = inspectList(properties.map((key) => [key, error[key]]), options, inspectProperty);
  return `${name}${message}${propertyContents ? ` { ${propertyContents} }` : ""}`;
}
__name(inspectObject2, "inspectObject");

// node_modules/loupe/lib/html.js
function inspectAttribute([key, value], options) {
  options.truncate -= 3;
  if (!value) {
    return `${options.stylize(String(key), "yellow")}`;
  }
  return `${options.stylize(String(key), "yellow")}=${options.stylize(`"${value}"`, "string")}`;
}
__name(inspectAttribute, "inspectAttribute");
function inspectHTMLCollection(collection, options) {
  return inspectList(collection, options, inspectHTML, "\n");
}
__name(inspectHTMLCollection, "inspectHTMLCollection");
function inspectHTML(element, options) {
  const properties = element.getAttributeNames();
  const name = element.tagName.toLowerCase();
  const head = options.stylize(`<${name}`, "special");
  const headClose = options.stylize(`>`, "special");
  const tail = options.stylize(`</${name}>`, "special");
  options.truncate -= name.length * 2 + 5;
  let propertyContents = "";
  if (properties.length > 0) {
    propertyContents += " ";
    propertyContents += inspectList(properties.map((key) => [key, element.getAttribute(key)]), options, inspectAttribute, " ");
  }
  options.truncate -= propertyContents.length;
  const truncate2 = options.truncate;
  let children = inspectHTMLCollection(element.children, options);
  if (children && children.length > truncate2) {
    children = `${truncator}(${element.children.length})`;
  }
  return `${head}${propertyContents}${headClose}${children}${tail}`;
}
__name(inspectHTML, "inspectHTML");

// node_modules/loupe/lib/index.js
var symbolsSupported = typeof Symbol === "function" && typeof Symbol.for === "function";
var chaiInspect = symbolsSupported ? Symbol.for("chai/inspect") : "@@chai/inspect";
var nodeInspect = false;
try {
  const nodeUtil = require_util();
  nodeInspect = nodeUtil.inspect ? nodeUtil.inspect.custom : false;
} catch (noNodeInspect) {
  nodeInspect = false;
}
var constructorMap = /* @__PURE__ */ new WeakMap();
var stringTagMap = {};
var baseTypesMap = {
  undefined: (value, options) => options.stylize("undefined", "undefined"),
  null: (value, options) => options.stylize("null", "null"),
  boolean: (value, options) => options.stylize(String(value), "boolean"),
  Boolean: (value, options) => options.stylize(String(value), "boolean"),
  number: inspectNumber,
  Number: inspectNumber,
  bigint: inspectBigInt,
  BigInt: inspectBigInt,
  string: inspectString,
  String: inspectString,
  function: inspectFunction,
  Function: inspectFunction,
  symbol: inspectSymbol,
  // A Symbol polyfill will return `Symbol` not `symbol` from typedetect
  Symbol: inspectSymbol,
  Array: inspectArray,
  Date: inspectDate,
  Map: inspectMap,
  Set: inspectSet,
  RegExp: inspectRegExp,
  Promise: promise_default,
  // WeakSet, WeakMap are totally opaque to us
  WeakSet: (value, options) => options.stylize("WeakSet{\u2026}", "special"),
  WeakMap: (value, options) => options.stylize("WeakMap{\u2026}", "special"),
  Arguments: inspectArguments,
  Int8Array: inspectTypedArray,
  Uint8Array: inspectTypedArray,
  Uint8ClampedArray: inspectTypedArray,
  Int16Array: inspectTypedArray,
  Uint16Array: inspectTypedArray,
  Int32Array: inspectTypedArray,
  Uint32Array: inspectTypedArray,
  Float32Array: inspectTypedArray,
  Float64Array: inspectTypedArray,
  Generator: () => "",
  DataView: () => "",
  ArrayBuffer: () => "",
  Error: inspectObject2,
  HTMLCollection: inspectHTMLCollection,
  NodeList: inspectHTMLCollection
};
var inspectCustom = /* @__PURE__ */ __name((value, options, type3) => {
  if (chaiInspect in value && typeof value[chaiInspect] === "function") {
    return value[chaiInspect](options);
  }
  if (nodeInspect && nodeInspect in value && typeof value[nodeInspect] === "function") {
    return value[nodeInspect](options.depth, options);
  }
  if ("inspect" in value && typeof value.inspect === "function") {
    return value.inspect(options.depth, options);
  }
  if ("constructor" in value && constructorMap.has(value.constructor)) {
    return constructorMap.get(value.constructor)(value, options);
  }
  if (stringTagMap[type3]) {
    return stringTagMap[type3](value, options);
  }
  return "";
}, "inspectCustom");
var chai_toString = Object.prototype.toString;
function inspect(value, opts = {}) {
  const options = normaliseOptions(opts, inspect);
  const { customInspect } = options;
  let type3 = value === null ? "null" : typeof value;
  if (type3 === "object") {
    type3 = chai_toString.call(value).slice(8, -1);
  }
  if (type3 in baseTypesMap) {
    return baseTypesMap[type3](value, options);
  }
  if (customInspect && value) {
    const output = inspectCustom(value, options, type3);
    if (output) {
      if (typeof output === "string")
        return output;
      return inspect(output, options);
    }
  }
  const proto = value ? Object.getPrototypeOf(value) : false;
  if (proto === Object.prototype || proto === null) {
    return inspectObject(value, options);
  }
  if (value && typeof HTMLElement === "function" && value instanceof HTMLElement) {
    return inspectHTML(value, options);
  }
  if ("constructor" in value) {
    if (value.constructor !== Object) {
      return inspectClass(value, options);
    }
    return inspectObject(value, options);
  }
  if (value === Object(value)) {
    return inspectObject(value, options);
  }
  return options.stylize(String(value), type3);
}
__name(inspect, "inspect");

// lib/chai/config.js
var config = {
  /**
   * ### config.includeStack
   *
   * User configurable property, influences whether stack trace
   * is included in Assertion error message. Default of false
   * suppresses stack trace in the error message.
   *
   *     chai.config.includeStack = true;  // enable stack on error
   *
   * @param {Boolean}
   * @api public
   */
  includeStack: false,
  /**
   * ### config.showDiff
   *
   * User configurable property, influences whether or not
   * the `showDiff` flag should be included in the thrown
   * AssertionErrors. `false` will always be `false`; `true`
   * will be true when the assertion has requested a diff
   * be shown.
   *
   * @param {Boolean}
   * @api public
   */
  showDiff: true,
  /**
   * ### config.truncateThreshold
   *
   * User configurable property, sets length threshold for actual and
   * expected values in assertion errors. If this threshold is exceeded, for
   * example for large data structures, the value is replaced with something
   * like `[ Array(3) ]` or `{ Object (prop1, prop2) }`.
   *
   * Set it to zero if you want to disable truncating altogether.
   *
   * This is especially userful when doing assertions on arrays: having this
   * set to a reasonable large value makes the failure messages readily
   * inspectable.
   *
   *     chai.config.truncateThreshold = 0;  // disable truncating
   *
   * @param {Number}
   * @api public
   */
  truncateThreshold: 40,
  /**
   * ### config.useProxy
   *
   * User configurable property, defines if chai will use a Proxy to throw
   * an error when a non-existent property is read, which protects users
   * from typos when using property-based assertions.
   *
   * Set it to false if you want to disable this feature.
   *
   *     chai.config.useProxy = false;  // disable use of Proxy
   *
   * This feature is automatically disabled regardless of this config value
   * in environments that don't support proxies.
   *
   * @param {Boolean}
   * @api public
   */
  useProxy: true,
  /**
   * ### config.proxyExcludedKeys
   *
   * User configurable property, defines which properties should be ignored
   * instead of throwing an error if they do not exist on the assertion.
   * This is only applied if the environment Chai is running in supports proxies and
   * if the `useProxy` configuration setting is enabled.
   * By default, `then` and `inspect` will not throw an error if they do not exist on the
   * assertion object because the `.inspect` property is read by `util.inspect` (for example, when
   * using `console.log` on the assertion object) and `.then` is necessary for promise type-checking.
   *
   *     // By default these keys will not throw an error if they do not exist on the assertion object
   *     chai.config.proxyExcludedKeys = ['then', 'inspect'];
   *
   * @param {Array}
   * @api public
   */
  proxyExcludedKeys: ["then", "catch", "inspect", "toJSON"],
  /**
   * ### config.deepEqual
   *
   * User configurable property, defines which a custom function to use for deepEqual
   * comparisons.
   * By default, the function used is the one from the `deep-eql` package without custom comparator.
   *
   *     // use a custom comparator
   *     chai.config.deepEqual = (expected, actual) => {
   *        return chai.util.eql(expected, actual, {
   *           comparator: (expected, actual) => {
   *              // for non number comparison, use the default behavior
   *              if(typeof expected !== 'number') return null;
   *              // allow a difference of 10 between compared numbers
   *              return typeof actual === 'number' && Math.abs(actual - expected) < 10
   *           }
   *        })
   *     };
   *
   * @param {Function}
   * @api public
   */
  deepEqual: null
};

// lib/chai/utils/inspect.js
function inspect2(obj, showHidden, depth, colors) {
  var options = {
    colors,
    depth: typeof depth === "undefined" ? 2 : depth,
    showHidden,
    truncate: config.truncateThreshold ? config.truncateThreshold : Infinity
  };
  return inspect(obj, options);
}
__name(inspect2, "inspect");

// lib/chai/utils/objDisplay.js
function objDisplay(obj) {
  var str = inspect2(obj), type3 = Object.prototype.toString.call(obj);
  if (config.truncateThreshold && str.length >= config.truncateThreshold) {
    if (type3 === "[object Function]") {
      return !obj.name || obj.name === "" ? "[Function]" : "[Function: " + obj.name + "]";
    } else if (type3 === "[object Array]") {
      return "[ Array(" + obj.length + ") ]";
    } else if (type3 === "[object Object]") {
      var keys = Object.keys(obj), kstr = keys.length > 2 ? keys.splice(0, 2).join(", ") + ", ..." : keys.join(", ");
      return "{ Object (" + kstr + ") }";
    } else {
      return str;
    }
  } else {
    return str;
  }
}
__name(objDisplay, "objDisplay");

// lib/chai/utils/getMessage.js
function getMessage2(obj, args) {
  var negate = flag(obj, "negate"), val = flag(obj, "object"), expected = args[3], actual = getActual(obj, args), msg = negate ? args[2] : args[1], flagMsg = flag(obj, "message");
  if (typeof msg === "function")
    msg = msg();
  msg = msg || "";
  msg = msg.replace(/#\{this\}/g, function() {
    return objDisplay(val);
  }).replace(/#\{act\}/g, function() {
    return objDisplay(actual);
  }).replace(/#\{exp\}/g, function() {
    return objDisplay(expected);
  });
  return flagMsg ? flagMsg + ": " + msg : msg;
}
__name(getMessage2, "getMessage");

// lib/chai/utils/transferFlags.js
function transferFlags(assertion, object, includeAll) {
  var flags = assertion.__flags || (assertion.__flags = /* @__PURE__ */ Object.create(null));
  if (!object.__flags) {
    object.__flags = /* @__PURE__ */ Object.create(null);
  }
  includeAll = arguments.length === 3 ? includeAll : true;
  for (var flag3 in flags) {
    if (includeAll || flag3 !== "object" && flag3 !== "ssfi" && flag3 !== "lockSsfi" && flag3 != "message") {
      object.__flags[flag3] = flags[flag3];
    }
  }
}
__name(transferFlags, "transferFlags");

// node_modules/deep-eql/index.js
function type2(obj) {
  if (typeof obj === "undefined") {
    return "undefined";
  }
  if (obj === null) {
    return "null";
  }
  const stringTag = obj[Symbol.toStringTag];
  if (typeof stringTag === "string") {
    return stringTag;
  }
  const sliceStart = 8;
  const sliceEnd = -1;
  return Object.prototype.toString.call(obj).slice(sliceStart, sliceEnd);
}
__name(type2, "type");
function FakeMap() {
  this._key = "chai/deep-eql__" + Math.random() + Date.now();
}
__name(FakeMap, "FakeMap");
FakeMap.prototype = {
  get: /* @__PURE__ */ __name(function get(key) {
    return key[this._key];
  }, "get"),
  set: /* @__PURE__ */ __name(function set(key, value) {
    if (Object.isExtensible(key)) {
      Object.defineProperty(key, this._key, {
        value,
        configurable: true
      });
    }
  }, "set")
};
var MemoizeMap = typeof WeakMap === "function" ? WeakMap : FakeMap;
function memoizeCompare(leftHandOperand, rightHandOperand, memoizeMap) {
  if (!memoizeMap || isPrimitive(leftHandOperand) || isPrimitive(rightHandOperand)) {
    return null;
  }
  var leftHandMap = memoizeMap.get(leftHandOperand);
  if (leftHandMap) {
    var result = leftHandMap.get(rightHandOperand);
    if (typeof result === "boolean") {
      return result;
    }
  }
  return null;
}
__name(memoizeCompare, "memoizeCompare");
function memoizeSet(leftHandOperand, rightHandOperand, memoizeMap, result) {
  if (!memoizeMap || isPrimitive(leftHandOperand) || isPrimitive(rightHandOperand)) {
    return;
  }
  var leftHandMap = memoizeMap.get(leftHandOperand);
  if (leftHandMap) {
    leftHandMap.set(rightHandOperand, result);
  } else {
    leftHandMap = new MemoizeMap();
    leftHandMap.set(rightHandOperand, result);
    memoizeMap.set(leftHandOperand, leftHandMap);
  }
}
__name(memoizeSet, "memoizeSet");
var deep_eql_default = deepEqual;
function deepEqual(leftHandOperand, rightHandOperand, options) {
  if (options && options.comparator) {
    return extensiveDeepEqual(leftHandOperand, rightHandOperand, options);
  }
  var simpleResult = simpleEqual(leftHandOperand, rightHandOperand);
  if (simpleResult !== null) {
    return simpleResult;
  }
  return extensiveDeepEqual(leftHandOperand, rightHandOperand, options);
}
__name(deepEqual, "deepEqual");
function simpleEqual(leftHandOperand, rightHandOperand) {
  if (leftHandOperand === rightHandOperand) {
    return leftHandOperand !== 0 || 1 / leftHandOperand === 1 / rightHandOperand;
  }
  if (leftHandOperand !== leftHandOperand && // eslint-disable-line no-self-compare
  rightHandOperand !== rightHandOperand) {
    return true;
  }
  if (isPrimitive(leftHandOperand) || isPrimitive(rightHandOperand)) {
    return false;
  }
  return null;
}
__name(simpleEqual, "simpleEqual");
function extensiveDeepEqual(leftHandOperand, rightHandOperand, options) {
  options = options || {};
  options.memoize = options.memoize === false ? false : options.memoize || new MemoizeMap();
  var comparator = options && options.comparator;
  var memoizeResultLeft = memoizeCompare(leftHandOperand, rightHandOperand, options.memoize);
  if (memoizeResultLeft !== null) {
    return memoizeResultLeft;
  }
  var memoizeResultRight = memoizeCompare(rightHandOperand, leftHandOperand, options.memoize);
  if (memoizeResultRight !== null) {
    return memoizeResultRight;
  }
  if (comparator) {
    var comparatorResult = comparator(leftHandOperand, rightHandOperand);
    if (comparatorResult === false || comparatorResult === true) {
      memoizeSet(leftHandOperand, rightHandOperand, options.memoize, comparatorResult);
      return comparatorResult;
    }
    var simpleResult = simpleEqual(leftHandOperand, rightHandOperand);
    if (simpleResult !== null) {
      return simpleResult;
    }
  }
  var leftHandType = type2(leftHandOperand);
  if (leftHandType !== type2(rightHandOperand)) {
    memoizeSet(leftHandOperand, rightHandOperand, options.memoize, false);
    return false;
  }
  memoizeSet(leftHandOperand, rightHandOperand, options.memoize, true);
  var result = extensiveDeepEqualByType(leftHandOperand, rightHandOperand, leftHandType, options);
  memoizeSet(leftHandOperand, rightHandOperand, options.memoize, result);
  return result;
}
__name(extensiveDeepEqual, "extensiveDeepEqual");
function extensiveDeepEqualByType(leftHandOperand, rightHandOperand, leftHandType, options) {
  switch (leftHandType) {
    case "String":
    case "Number":
    case "Boolean":
    case "Date":
      return deepEqual(leftHandOperand.valueOf(), rightHandOperand.valueOf());
    case "Promise":
    case "Symbol":
    case "function":
    case "WeakMap":
    case "WeakSet":
      return leftHandOperand === rightHandOperand;
    case "Error":
      return keysEqual(leftHandOperand, rightHandOperand, ["name", "message", "code"], options);
    case "Arguments":
    case "Int8Array":
    case "Uint8Array":
    case "Uint8ClampedArray":
    case "Int16Array":
    case "Uint16Array":
    case "Int32Array":
    case "Uint32Array":
    case "Float32Array":
    case "Float64Array":
    case "Array":
      return iterableEqual(leftHandOperand, rightHandOperand, options);
    case "RegExp":
      return regexpEqual(leftHandOperand, rightHandOperand);
    case "Generator":
      return generatorEqual(leftHandOperand, rightHandOperand, options);
    case "DataView":
      return iterableEqual(new Uint8Array(leftHandOperand.buffer), new Uint8Array(rightHandOperand.buffer), options);
    case "ArrayBuffer":
      return iterableEqual(new Uint8Array(leftHandOperand), new Uint8Array(rightHandOperand), options);
    case "Set":
      return entriesEqual(leftHandOperand, rightHandOperand, options);
    case "Map":
      return entriesEqual(leftHandOperand, rightHandOperand, options);
    case "Temporal.PlainDate":
    case "Temporal.PlainTime":
    case "Temporal.PlainDateTime":
    case "Temporal.Instant":
    case "Temporal.ZonedDateTime":
    case "Temporal.PlainYearMonth":
    case "Temporal.PlainMonthDay":
      return leftHandOperand.equals(rightHandOperand);
    case "Temporal.Duration":
      return leftHandOperand.total("nanoseconds") === rightHandOperand.total("nanoseconds");
    case "Temporal.TimeZone":
    case "Temporal.Calendar":
      return leftHandOperand.toString() === rightHandOperand.toString();
    default:
      return objectEqual(leftHandOperand, rightHandOperand, options);
  }
}
__name(extensiveDeepEqualByType, "extensiveDeepEqualByType");
function regexpEqual(leftHandOperand, rightHandOperand) {
  return leftHandOperand.toString() === rightHandOperand.toString();
}
__name(regexpEqual, "regexpEqual");
function entriesEqual(leftHandOperand, rightHandOperand, options) {
  if (leftHandOperand.size !== rightHandOperand.size) {
    return false;
  }
  if (leftHandOperand.size === 0) {
    return true;
  }
  var leftHandItems = [];
  var rightHandItems = [];
  leftHandOperand.forEach(/* @__PURE__ */ __name(function gatherEntries(key, value) {
    leftHandItems.push([key, value]);
  }, "gatherEntries"));
  rightHandOperand.forEach(/* @__PURE__ */ __name(function gatherEntries(key, value) {
    rightHandItems.push([key, value]);
  }, "gatherEntries"));
  return iterableEqual(leftHandItems.sort(), rightHandItems.sort(), options);
}
__name(entriesEqual, "entriesEqual");
function iterableEqual(leftHandOperand, rightHandOperand, options) {
  var length = leftHandOperand.length;
  if (length !== rightHandOperand.length) {
    return false;
  }
  if (length === 0) {
    return true;
  }
  var index = -1;
  while (++index < length) {
    if (deepEqual(leftHandOperand[index], rightHandOperand[index], options) === false) {
      return false;
    }
  }
  return true;
}
__name(iterableEqual, "iterableEqual");
function generatorEqual(leftHandOperand, rightHandOperand, options) {
  return iterableEqual(getGeneratorEntries(leftHandOperand), getGeneratorEntries(rightHandOperand), options);
}
__name(generatorEqual, "generatorEqual");
function hasIteratorFunction(target) {
  return typeof Symbol !== "undefined" && typeof target === "object" && typeof Symbol.iterator !== "undefined" && typeof target[Symbol.iterator] === "function";
}
__name(hasIteratorFunction, "hasIteratorFunction");
function getIteratorEntries(target) {
  if (hasIteratorFunction(target)) {
    try {
      return getGeneratorEntries(target[Symbol.iterator]());
    } catch (iteratorError) {
      return [];
    }
  }
  return [];
}
__name(getIteratorEntries, "getIteratorEntries");
function getGeneratorEntries(generator) {
  var generatorResult = generator.next();
  var accumulator = [generatorResult.value];
  while (generatorResult.done === false) {
    generatorResult = generator.next();
    accumulator.push(generatorResult.value);
  }
  return accumulator;
}
__name(getGeneratorEntries, "getGeneratorEntries");
function getEnumerableKeys(target) {
  var keys = [];
  for (var key in target) {
    keys.push(key);
  }
  return keys;
}
__name(getEnumerableKeys, "getEnumerableKeys");
function getEnumerableSymbols(target) {
  var keys = [];
  var allKeys = Object.getOwnPropertySymbols(target);
  for (var i = 0; i < allKeys.length; i += 1) {
    var key = allKeys[i];
    if (Object.getOwnPropertyDescriptor(target, key).enumerable) {
      keys.push(key);
    }
  }
  return keys;
}
__name(getEnumerableSymbols, "getEnumerableSymbols");
function keysEqual(leftHandOperand, rightHandOperand, keys, options) {
  var length = keys.length;
  if (length === 0) {
    return true;
  }
  for (var i = 0; i < length; i += 1) {
    if (deepEqual(leftHandOperand[keys[i]], rightHandOperand[keys[i]], options) === false) {
      return false;
    }
  }
  return true;
}
__name(keysEqual, "keysEqual");
function objectEqual(leftHandOperand, rightHandOperand, options) {
  var leftHandKeys = getEnumerableKeys(leftHandOperand);
  var rightHandKeys = getEnumerableKeys(rightHandOperand);
  var leftHandSymbols = getEnumerableSymbols(leftHandOperand);
  var rightHandSymbols = getEnumerableSymbols(rightHandOperand);
  leftHandKeys = leftHandKeys.concat(leftHandSymbols);
  rightHandKeys = rightHandKeys.concat(rightHandSymbols);
  if (leftHandKeys.length && leftHandKeys.length === rightHandKeys.length) {
    if (iterableEqual(mapSymbols(leftHandKeys).sort(), mapSymbols(rightHandKeys).sort()) === false) {
      return false;
    }
    return keysEqual(leftHandOperand, rightHandOperand, leftHandKeys, options);
  }
  var leftHandEntries = getIteratorEntries(leftHandOperand);
  var rightHandEntries = getIteratorEntries(rightHandOperand);
  if (leftHandEntries.length && leftHandEntries.length === rightHandEntries.length) {
    leftHandEntries.sort();
    rightHandEntries.sort();
    return iterableEqual(leftHandEntries, rightHandEntries, options);
  }
  if (leftHandKeys.length === 0 && leftHandEntries.length === 0 && rightHandKeys.length === 0 && rightHandEntries.length === 0) {
    return true;
  }
  return false;
}
__name(objectEqual, "objectEqual");
function isPrimitive(value) {
  return value === null || typeof value !== "object";
}
__name(isPrimitive, "isPrimitive");
function mapSymbols(arr) {
  return arr.map(/* @__PURE__ */ __name(function mapSymbol(entry) {
    if (typeof entry === "symbol") {
      return entry.toString();
    }
    return entry;
  }, "mapSymbol"));
}
__name(mapSymbols, "mapSymbols");

// node_modules/pathval/index.js
function hasProperty(obj, name) {
  if (typeof obj === "undefined" || obj === null) {
    return false;
  }
  return name in Object(obj);
}
__name(hasProperty, "hasProperty");
function parsePath(path) {
  const str = path.replace(/([^\\])\[/g, "$1.[");
  const parts = str.match(/(\\\.|[^.]+?)+/g);
  return parts.map((value) => {
    if (value === "constructor" || value === "__proto__" || value === "prototype") {
      return {};
    }
    const regexp = /^\[(\d+)\]$/;
    const mArr = regexp.exec(value);
    let parsed = null;
    if (mArr) {
      parsed = { i: parseFloat(mArr[1]) };
    } else {
      parsed = { p: value.replace(/\\([.[\]])/g, "$1") };
    }
    return parsed;
  });
}
__name(parsePath, "parsePath");
function internalGetPathValue(obj, parsed, pathDepth) {
  let temporaryValue = obj;
  let res = null;
  pathDepth = typeof pathDepth === "undefined" ? parsed.length : pathDepth;
  for (let i = 0; i < pathDepth; i++) {
    const part = parsed[i];
    if (temporaryValue) {
      if (typeof part.p === "undefined") {
        temporaryValue = temporaryValue[part.i];
      } else {
        temporaryValue = temporaryValue[part.p];
      }
      if (i === pathDepth - 1) {
        res = temporaryValue;
      }
    }
  }
  return res;
}
__name(internalGetPathValue, "internalGetPathValue");
function getPathInfo(obj, path) {
  const parsed = parsePath(path);
  const last = parsed[parsed.length - 1];
  const info = {
    parent: parsed.length > 1 ? internalGetPathValue(obj, parsed, parsed.length - 1) : obj,
    name: last.p || last.i,
    value: internalGetPathValue(obj, parsed)
  };
  info.exists = hasProperty(info.parent, info.name);
  return info;
}
__name(getPathInfo, "getPathInfo");

// lib/chai/assertion.js
function Assertion(obj, msg, ssfi, lockSsfi) {
  flag(this, "ssfi", ssfi || Assertion);
  flag(this, "lockSsfi", lockSsfi);
  flag(this, "object", obj);
  flag(this, "message", msg);
  flag(this, "eql", config.deepEqual || deep_eql_default);
  return proxify(this);
}
__name(Assertion, "Assertion");
Object.defineProperty(Assertion, "includeStack", {
  get: function() {
    console.warn("Assertion.includeStack is deprecated, use chai.config.includeStack instead.");
    return config.includeStack;
  },
  set: function(value) {
    console.warn("Assertion.includeStack is deprecated, use chai.config.includeStack instead.");
    config.includeStack = value;
  }
});
Object.defineProperty(Assertion, "showDiff", {
  get: function() {
    console.warn("Assertion.showDiff is deprecated, use chai.config.showDiff instead.");
    return config.showDiff;
  },
  set: function(value) {
    console.warn("Assertion.showDiff is deprecated, use chai.config.showDiff instead.");
    config.showDiff = value;
  }
});
Assertion.addProperty = function(name, fn) {
  addProperty(this.prototype, name, fn);
};
Assertion.addMethod = function(name, fn) {
  addMethod(this.prototype, name, fn);
};
Assertion.addChainableMethod = function(name, fn, chainingBehavior) {
  addChainableMethod(this.prototype, name, fn, chainingBehavior);
};
Assertion.overwriteProperty = function(name, fn) {
  overwriteProperty(this.prototype, name, fn);
};
Assertion.overwriteMethod = function(name, fn) {
  overwriteMethod(this.prototype, name, fn);
};
Assertion.overwriteChainableMethod = function(name, fn, chainingBehavior) {
  overwriteChainableMethod(this.prototype, name, fn, chainingBehavior);
};
Assertion.prototype.assert = function(expr, msg, negateMsg, expected, _actual, showDiff) {
  var ok = test(this, arguments);
  if (false !== showDiff)
    showDiff = true;
  if (void 0 === expected && void 0 === _actual)
    showDiff = false;
  if (true !== config.showDiff)
    showDiff = false;
  if (!ok) {
    msg = getMessage2(this, arguments);
    var actual = getActual(this, arguments);
    var assertionErrorObjectProperties = {
      actual,
      expected,
      showDiff
    };
    var operator = getOperator(this, arguments);
    if (operator) {
      assertionErrorObjectProperties.operator = operator;
    }
    throw new AssertionError(
      msg,
      assertionErrorObjectProperties,
      config.includeStack ? this.assert : flag(this, "ssfi")
    );
  }
};
Object.defineProperty(
  Assertion.prototype,
  "_obj",
  {
    get: function() {
      return flag(this, "object");
    },
    set: function(val) {
      flag(this, "object", val);
    }
  }
);

// lib/chai/utils/isProxyEnabled.js
function isProxyEnabled() {
  return config.useProxy && typeof Proxy !== "undefined" && typeof Reflect !== "undefined";
}
__name(isProxyEnabled, "isProxyEnabled");

// lib/chai/utils/addProperty.js
function addProperty(ctx, name, getter) {
  getter = getter === void 0 ? function() {
  } : getter;
  Object.defineProperty(
    ctx,
    name,
    {
      get: /* @__PURE__ */ __name(function propertyGetter() {
        if (!isProxyEnabled() && !flag(this, "lockSsfi")) {
          flag(this, "ssfi", propertyGetter);
        }
        var result = getter.call(this);
        if (result !== void 0)
          return result;
        var newAssertion = new Assertion();
        transferFlags(this, newAssertion);
        return newAssertion;
      }, "propertyGetter"),
      configurable: true
    }
  );
}
__name(addProperty, "addProperty");

// lib/chai/utils/addLengthGuard.js
var fnLengthDesc = Object.getOwnPropertyDescriptor(function() {
}, "length");
function addLengthGuard(fn, assertionName, isChainable) {
  if (!fnLengthDesc.configurable)
    return fn;
  Object.defineProperty(fn, "length", {
    get: function() {
      if (isChainable) {
        throw Error("Invalid Chai property: " + assertionName + '.length. Due to a compatibility issue, "length" cannot directly follow "' + assertionName + '". Use "' + assertionName + '.lengthOf" instead.');
      }
      throw Error("Invalid Chai property: " + assertionName + '.length. See docs for proper usage of "' + assertionName + '".');
    }
  });
  return fn;
}
__name(addLengthGuard, "addLengthGuard");

// lib/chai/utils/getProperties.js
function getProperties(object) {
  var result = Object.getOwnPropertyNames(object);
  function addProperty2(property) {
    if (result.indexOf(property) === -1) {
      result.push(property);
    }
  }
  __name(addProperty2, "addProperty");
  var proto = Object.getPrototypeOf(object);
  while (proto !== null) {
    Object.getOwnPropertyNames(proto).forEach(addProperty2);
    proto = Object.getPrototypeOf(proto);
  }
  return result;
}
__name(getProperties, "getProperties");

// lib/chai/utils/proxify.js
var builtins = ["__flags", "__methods", "_obj", "assert"];
function proxify(obj, nonChainableMethodName) {
  if (!isProxyEnabled())
    return obj;
  return new Proxy(obj, {
    get: /* @__PURE__ */ __name(function proxyGetter(target, property) {
      if (typeof property === "string" && config.proxyExcludedKeys.indexOf(property) === -1 && !Reflect.has(target, property)) {
        if (nonChainableMethodName) {
          throw Error("Invalid Chai property: " + nonChainableMethodName + "." + property + '. See docs for proper usage of "' + nonChainableMethodName + '".');
        }
        var suggestion = null;
        var suggestionDistance = 4;
        getProperties(target).forEach(function(prop) {
          if (!Object.prototype.hasOwnProperty(prop) && builtins.indexOf(prop) === -1) {
            var dist = stringDistanceCapped(
              property,
              prop,
              suggestionDistance
            );
            if (dist < suggestionDistance) {
              suggestion = prop;
              suggestionDistance = dist;
            }
          }
        });
        if (suggestion !== null) {
          throw Error("Invalid Chai property: " + property + '. Did you mean "' + suggestion + '"?');
        } else {
          throw Error("Invalid Chai property: " + property);
        }
      }
      if (builtins.indexOf(property) === -1 && !flag(target, "lockSsfi")) {
        flag(target, "ssfi", proxyGetter);
      }
      return Reflect.get(target, property);
    }, "proxyGetter")
  });
}
__name(proxify, "proxify");
function stringDistanceCapped(strA, strB, cap) {
  if (Math.abs(strA.length - strB.length) >= cap) {
    return cap;
  }
  var memo = [];
  for (var i = 0; i <= strA.length; i++) {
    memo[i] = Array(strB.length + 1).fill(0);
    memo[i][0] = i;
  }
  for (var j = 0; j < strB.length; j++) {
    memo[0][j] = j;
  }
  for (var i = 1; i <= strA.length; i++) {
    var ch = strA.charCodeAt(i - 1);
    for (var j = 1; j <= strB.length; j++) {
      if (Math.abs(i - j) >= cap) {
        memo[i][j] = cap;
        continue;
      }
      memo[i][j] = Math.min(
        memo[i - 1][j] + 1,
        memo[i][j - 1] + 1,
        memo[i - 1][j - 1] + (ch === strB.charCodeAt(j - 1) ? 0 : 1)
      );
    }
  }
  return memo[strA.length][strB.length];
}
__name(stringDistanceCapped, "stringDistanceCapped");

// lib/chai/utils/addMethod.js
function addMethod(ctx, name, method) {
  var methodWrapper = /* @__PURE__ */ __name(function() {
    if (!flag(this, "lockSsfi")) {
      flag(this, "ssfi", methodWrapper);
    }
    var result = method.apply(this, arguments);
    if (result !== void 0)
      return result;
    var newAssertion = new Assertion();
    transferFlags(this, newAssertion);
    return newAssertion;
  }, "methodWrapper");
  addLengthGuard(methodWrapper, name, false);
  ctx[name] = proxify(methodWrapper, name);
}
__name(addMethod, "addMethod");

// lib/chai/utils/overwriteProperty.js
function overwriteProperty(ctx, name, getter) {
  var _get = Object.getOwnPropertyDescriptor(ctx, name), _super = /* @__PURE__ */ __name(function() {
  }, "_super");
  if (_get && "function" === typeof _get.get)
    _super = _get.get;
  Object.defineProperty(
    ctx,
    name,
    {
      get: /* @__PURE__ */ __name(function overwritingPropertyGetter() {
        if (!isProxyEnabled() && !flag(this, "lockSsfi")) {
          flag(this, "ssfi", overwritingPropertyGetter);
        }
        var origLockSsfi = flag(this, "lockSsfi");
        flag(this, "lockSsfi", true);
        var result = getter(_super).call(this);
        flag(this, "lockSsfi", origLockSsfi);
        if (result !== void 0) {
          return result;
        }
        var newAssertion = new Assertion();
        transferFlags(this, newAssertion);
        return newAssertion;
      }, "overwritingPropertyGetter"),
      configurable: true
    }
  );
}
__name(overwriteProperty, "overwriteProperty");

// lib/chai/utils/overwriteMethod.js
function overwriteMethod(ctx, name, method) {
  var _method = ctx[name], _super = /* @__PURE__ */ __name(function() {
    throw new Error(name + " is not a function");
  }, "_super");
  if (_method && "function" === typeof _method)
    _super = _method;
  var overwritingMethodWrapper = /* @__PURE__ */ __name(function() {
    if (!flag(this, "lockSsfi")) {
      flag(this, "ssfi", overwritingMethodWrapper);
    }
    var origLockSsfi = flag(this, "lockSsfi");
    flag(this, "lockSsfi", true);
    var result = method(_super).apply(this, arguments);
    flag(this, "lockSsfi", origLockSsfi);
    if (result !== void 0) {
      return result;
    }
    var newAssertion = new Assertion();
    transferFlags(this, newAssertion);
    return newAssertion;
  }, "overwritingMethodWrapper");
  addLengthGuard(overwritingMethodWrapper, name, false);
  ctx[name] = proxify(overwritingMethodWrapper, name);
}
__name(overwriteMethod, "overwriteMethod");

// lib/chai/utils/addChainableMethod.js
var canSetPrototype = typeof Object.setPrototypeOf === "function";
var testFn = /* @__PURE__ */ __name(function() {
}, "testFn");
var excludeNames = Object.getOwnPropertyNames(testFn).filter(function(name) {
  var propDesc = Object.getOwnPropertyDescriptor(testFn, name);
  if (typeof propDesc !== "object")
    return true;
  return !propDesc.configurable;
});
var call = Function.prototype.call;
var apply = Function.prototype.apply;
function addChainableMethod(ctx, name, method, chainingBehavior) {
  if (typeof chainingBehavior !== "function") {
    chainingBehavior = /* @__PURE__ */ __name(function() {
    }, "chainingBehavior");
  }
  var chainableBehavior = {
    method,
    chainingBehavior
  };
  if (!ctx.__methods) {
    ctx.__methods = {};
  }
  ctx.__methods[name] = chainableBehavior;
  Object.defineProperty(
    ctx,
    name,
    {
      get: /* @__PURE__ */ __name(function chainableMethodGetter() {
        chainableBehavior.chainingBehavior.call(this);
        var chainableMethodWrapper = /* @__PURE__ */ __name(function() {
          if (!flag(this, "lockSsfi")) {
            flag(this, "ssfi", chainableMethodWrapper);
          }
          var result = chainableBehavior.method.apply(this, arguments);
          if (result !== void 0) {
            return result;
          }
          var newAssertion = new Assertion();
          transferFlags(this, newAssertion);
          return newAssertion;
        }, "chainableMethodWrapper");
        addLengthGuard(chainableMethodWrapper, name, true);
        if (canSetPrototype) {
          var prototype = Object.create(this);
          prototype.call = call;
          prototype.apply = apply;
          Object.setPrototypeOf(chainableMethodWrapper, prototype);
        } else {
          var asserterNames = Object.getOwnPropertyNames(ctx);
          asserterNames.forEach(function(asserterName) {
            if (excludeNames.indexOf(asserterName) !== -1) {
              return;
            }
            var pd = Object.getOwnPropertyDescriptor(ctx, asserterName);
            Object.defineProperty(chainableMethodWrapper, asserterName, pd);
          });
        }
        transferFlags(this, chainableMethodWrapper);
        return proxify(chainableMethodWrapper);
      }, "chainableMethodGetter"),
      configurable: true
    }
  );
}
__name(addChainableMethod, "addChainableMethod");

// lib/chai/utils/overwriteChainableMethod.js
function overwriteChainableMethod(ctx, name, method, chainingBehavior) {
  var chainableBehavior = ctx.__methods[name];
  var _chainingBehavior = chainableBehavior.chainingBehavior;
  chainableBehavior.chainingBehavior = /* @__PURE__ */ __name(function overwritingChainableMethodGetter() {
    var result = chainingBehavior(_chainingBehavior).call(this);
    if (result !== void 0) {
      return result;
    }
    var newAssertion = new Assertion();
    transferFlags(this, newAssertion);
    return newAssertion;
  }, "overwritingChainableMethodGetter");
  var _method = chainableBehavior.method;
  chainableBehavior.method = /* @__PURE__ */ __name(function overwritingChainableMethodWrapper() {
    var result = method(_method).apply(this, arguments);
    if (result !== void 0) {
      return result;
    }
    var newAssertion = new Assertion();
    transferFlags(this, newAssertion);
    return newAssertion;
  }, "overwritingChainableMethodWrapper");
}
__name(overwriteChainableMethod, "overwriteChainableMethod");

// lib/chai/utils/compareByInspect.js
function compareByInspect(a, b) {
  return inspect2(a) < inspect2(b) ? -1 : 1;
}
__name(compareByInspect, "compareByInspect");

// lib/chai/utils/getOwnEnumerablePropertySymbols.js
function getOwnEnumerablePropertySymbols(obj) {
  if (typeof Object.getOwnPropertySymbols !== "function")
    return [];
  return Object.getOwnPropertySymbols(obj).filter(function(sym) {
    return Object.getOwnPropertyDescriptor(obj, sym).enumerable;
  });
}
__name(getOwnEnumerablePropertySymbols, "getOwnEnumerablePropertySymbols");

// lib/chai/utils/getOwnEnumerableProperties.js
function getOwnEnumerableProperties(obj) {
  return Object.keys(obj).concat(getOwnEnumerablePropertySymbols(obj));
}
__name(getOwnEnumerableProperties, "getOwnEnumerableProperties");

// lib/chai/utils/isNaN.js
function _isNaN(value) {
  return value !== value;
}
__name(_isNaN, "_isNaN");
var isNaN2 = Number.isNaN || _isNaN;

// lib/chai/utils/getOperator.js
function isObjectType(obj) {
  var objectType = type(obj);
  var objectTypes = ["Array", "Object", "Function"];
  return objectTypes.indexOf(objectType) !== -1;
}
__name(isObjectType, "isObjectType");
function getOperator(obj, args) {
  var operator = flag(obj, "operator");
  var negate = flag(obj, "negate");
  var expected = args[3];
  var msg = negate ? args[2] : args[1];
  if (operator) {
    return operator;
  }
  if (typeof msg === "function")
    msg = msg();
  msg = msg || "";
  if (!msg) {
    return void 0;
  }
  if (/\shave\s/.test(msg)) {
    return void 0;
  }
  var isObject = isObjectType(expected);
  if (/\snot\s/.test(msg)) {
    return isObject ? "notDeepStrictEqual" : "notStrictEqual";
  }
  return isObject ? "deepStrictEqual" : "strictEqual";
}
__name(getOperator, "getOperator");

// lib/chai/utils/index.js
function getName(fn) {
  return fn.name;
}
__name(getName, "getName");

// lib/chai/core/assertions.js
var { flag: flag2 } = utils_exports;
[
  "to",
  "be",
  "been",
  "is",
  "and",
  "has",
  "have",
  "with",
  "that",
  "which",
  "at",
  "of",
  "same",
  "but",
  "does",
  "still",
  "also"
].forEach(function(chain) {
  Assertion.addProperty(chain);
});
Assertion.addProperty("not", function() {
  flag2(this, "negate", true);
});
Assertion.addProperty("deep", function() {
  flag2(this, "deep", true);
});
Assertion.addProperty("nested", function() {
  flag2(this, "nested", true);
});
Assertion.addProperty("own", function() {
  flag2(this, "own", true);
});
Assertion.addProperty("ordered", function() {
  flag2(this, "ordered", true);
});
Assertion.addProperty("any", function() {
  flag2(this, "any", true);
  flag2(this, "all", false);
});
Assertion.addProperty("all", function() {
  flag2(this, "all", true);
  flag2(this, "any", false);
});
var functionTypes = {
  "function": ["function", "asyncfunction", "generatorfunction", "asyncgeneratorfunction"],
  "asyncfunction": ["asyncfunction", "asyncgeneratorfunction"],
  "generatorfunction": ["generatorfunction", "asyncgeneratorfunction"],
  "asyncgeneratorfunction": ["asyncgeneratorfunction"]
};
function an(type3, msg) {
  if (msg)
    flag2(this, "message", msg);
  type3 = type3.toLowerCase();
  var obj = flag2(this, "object"), article = ~["a", "e", "i", "o", "u"].indexOf(type3.charAt(0)) ? "an " : "a ";
  const detectedType = type(obj).toLowerCase();
  if (functionTypes["function"].includes(type3)) {
    this.assert(
      functionTypes[type3].includes(detectedType),
      "expected #{this} to be " + article + type3,
      "expected #{this} not to be " + article + type3
    );
  } else {
    this.assert(
      type3 === detectedType,
      "expected #{this} to be " + article + type3,
      "expected #{this} not to be " + article + type3
    );
  }
}
__name(an, "an");
Assertion.addChainableMethod("an", an);
Assertion.addChainableMethod("a", an);
function SameValueZero(a, b) {
  return isNaN2(a) && isNaN2(b) || a === b;
}
__name(SameValueZero, "SameValueZero");
function includeChainingBehavior() {
  flag2(this, "contains", true);
}
__name(includeChainingBehavior, "includeChainingBehavior");
function include(val, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), objType = type(obj).toLowerCase(), flagMsg = flag2(this, "message"), negate = flag2(this, "negate"), ssfi = flag2(this, "ssfi"), isDeep = flag2(this, "deep"), descriptor = isDeep ? "deep " : "", isEql = isDeep ? flag2(this, "eql") : SameValueZero;
  flagMsg = flagMsg ? flagMsg + ": " : "";
  var included = false;
  switch (objType) {
    case "string":
      included = obj.indexOf(val) !== -1;
      break;
    case "weakset":
      if (isDeep) {
        throw new AssertionError(
          flagMsg + "unable to use .deep.include with WeakSet",
          void 0,
          ssfi
        );
      }
      included = obj.has(val);
      break;
    case "map":
      obj.forEach(function(item) {
        included = included || isEql(item, val);
      });
      break;
    case "set":
      if (isDeep) {
        obj.forEach(function(item) {
          included = included || isEql(item, val);
        });
      } else {
        included = obj.has(val);
      }
      break;
    case "array":
      if (isDeep) {
        included = obj.some(function(item) {
          return isEql(item, val);
        });
      } else {
        included = obj.indexOf(val) !== -1;
      }
      break;
    default:
      if (val !== Object(val)) {
        throw new AssertionError(
          flagMsg + "the given combination of arguments (" + objType + " and " + type(val).toLowerCase() + ") is invalid for this assertion. You can use an array, a map, an object, a set, a string, or a weakset instead of a " + type(val).toLowerCase(),
          void 0,
          ssfi
        );
      }
      var props = Object.keys(val), firstErr = null, numErrs = 0;
      props.forEach(function(prop) {
        var propAssertion = new Assertion(obj);
        transferFlags(this, propAssertion, true);
        flag2(propAssertion, "lockSsfi", true);
        if (!negate || props.length === 1) {
          propAssertion.property(prop, val[prop]);
          return;
        }
        try {
          propAssertion.property(prop, val[prop]);
        } catch (err) {
          if (!check_error_exports.compatibleConstructor(err, AssertionError)) {
            throw err;
          }
          if (firstErr === null)
            firstErr = err;
          numErrs++;
        }
      }, this);
      if (negate && props.length > 1 && numErrs === props.length) {
        throw firstErr;
      }
      return;
  }
  this.assert(
    included,
    "expected #{this} to " + descriptor + "include " + inspect2(val),
    "expected #{this} to not " + descriptor + "include " + inspect2(val)
  );
}
__name(include, "include");
Assertion.addChainableMethod("include", include, includeChainingBehavior);
Assertion.addChainableMethod("contain", include, includeChainingBehavior);
Assertion.addChainableMethod("contains", include, includeChainingBehavior);
Assertion.addChainableMethod("includes", include, includeChainingBehavior);
Assertion.addProperty("ok", function() {
  this.assert(
    flag2(this, "object"),
    "expected #{this} to be truthy",
    "expected #{this} to be falsy"
  );
});
Assertion.addProperty("true", function() {
  this.assert(
    true === flag2(this, "object"),
    "expected #{this} to be true",
    "expected #{this} to be false",
    flag2(this, "negate") ? false : true
  );
});
Assertion.addProperty("callable", function() {
  const val = flag2(this, "object");
  const ssfi = flag2(this, "ssfi");
  const message = flag2(this, "message");
  const msg = message ? `${message}: ` : "";
  const negate = flag2(this, "negate");
  const assertionMessage = negate ? `${msg}expected ${inspect2(val)} not to be a callable function` : `${msg}expected ${inspect2(val)} to be a callable function`;
  const isCallable = ["Function", "AsyncFunction", "GeneratorFunction", "AsyncGeneratorFunction"].includes(type(val));
  if (isCallable && negate || !isCallable && !negate) {
    throw new AssertionError(
      assertionMessage,
      void 0,
      ssfi
    );
  }
});
Assertion.addProperty("false", function() {
  this.assert(
    false === flag2(this, "object"),
    "expected #{this} to be false",
    "expected #{this} to be true",
    flag2(this, "negate") ? true : false
  );
});
Assertion.addProperty("null", function() {
  this.assert(
    null === flag2(this, "object"),
    "expected #{this} to be null",
    "expected #{this} not to be null"
  );
});
Assertion.addProperty("undefined", function() {
  this.assert(
    void 0 === flag2(this, "object"),
    "expected #{this} to be undefined",
    "expected #{this} not to be undefined"
  );
});
Assertion.addProperty("NaN", function() {
  this.assert(
    isNaN2(flag2(this, "object")),
    "expected #{this} to be NaN",
    "expected #{this} not to be NaN"
  );
});
function assertExist() {
  var val = flag2(this, "object");
  this.assert(
    val !== null && val !== void 0,
    "expected #{this} to exist",
    "expected #{this} to not exist"
  );
}
__name(assertExist, "assertExist");
Assertion.addProperty("exist", assertExist);
Assertion.addProperty("exists", assertExist);
Assertion.addProperty("empty", function() {
  var val = flag2(this, "object"), ssfi = flag2(this, "ssfi"), flagMsg = flag2(this, "message"), itemsCount;
  flagMsg = flagMsg ? flagMsg + ": " : "";
  switch (type(val).toLowerCase()) {
    case "array":
    case "string":
      itemsCount = val.length;
      break;
    case "map":
    case "set":
      itemsCount = val.size;
      break;
    case "weakmap":
    case "weakset":
      throw new AssertionError(
        flagMsg + ".empty was passed a weak collection",
        void 0,
        ssfi
      );
    case "function":
      var msg = flagMsg + ".empty was passed a function " + getName(val);
      throw new AssertionError(msg.trim(), void 0, ssfi);
    default:
      if (val !== Object(val)) {
        throw new AssertionError(
          flagMsg + ".empty was passed non-string primitive " + inspect2(val),
          void 0,
          ssfi
        );
      }
      itemsCount = Object.keys(val).length;
  }
  this.assert(
    0 === itemsCount,
    "expected #{this} to be empty",
    "expected #{this} not to be empty"
  );
});
function checkArguments() {
  var obj = flag2(this, "object"), type3 = type(obj);
  this.assert(
    "Arguments" === type3,
    "expected #{this} to be arguments but got " + type3,
    "expected #{this} to not be arguments"
  );
}
__name(checkArguments, "checkArguments");
Assertion.addProperty("arguments", checkArguments);
Assertion.addProperty("Arguments", checkArguments);
function assertEqual(val, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object");
  if (flag2(this, "deep")) {
    var prevLockSsfi = flag2(this, "lockSsfi");
    flag2(this, "lockSsfi", true);
    this.eql(val);
    flag2(this, "lockSsfi", prevLockSsfi);
  } else {
    this.assert(
      val === obj,
      "expected #{this} to equal #{exp}",
      "expected #{this} to not equal #{exp}",
      val,
      this._obj,
      true
    );
  }
}
__name(assertEqual, "assertEqual");
Assertion.addMethod("equal", assertEqual);
Assertion.addMethod("equals", assertEqual);
Assertion.addMethod("eq", assertEqual);
function assertEql(obj, msg) {
  if (msg)
    flag2(this, "message", msg);
  var eql = flag2(this, "eql");
  this.assert(
    eql(obj, flag2(this, "object")),
    "expected #{this} to deeply equal #{exp}",
    "expected #{this} to not deeply equal #{exp}",
    obj,
    this._obj,
    true
  );
}
__name(assertEql, "assertEql");
Assertion.addMethod("eql", assertEql);
Assertion.addMethod("eqls", assertEql);
function assertAbove(n, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), doLength = flag2(this, "doLength"), flagMsg = flag2(this, "message"), msgPrefix = flagMsg ? flagMsg + ": " : "", ssfi = flag2(this, "ssfi"), objType = type(obj).toLowerCase(), nType = type(n).toLowerCase(), errorMessage, shouldThrow = true;
  if (doLength && objType !== "map" && objType !== "set") {
    new Assertion(obj, flagMsg, ssfi, true).to.have.property("length");
  }
  if (!doLength && (objType === "date" && nType !== "date")) {
    errorMessage = msgPrefix + "the argument to above must be a date";
  } else if (nType !== "number" && (doLength || objType === "number")) {
    errorMessage = msgPrefix + "the argument to above must be a number";
  } else if (!doLength && (objType !== "date" && objType !== "number")) {
    var printObj = objType === "string" ? "'" + obj + "'" : obj;
    errorMessage = msgPrefix + "expected " + printObj + " to be a number or a date";
  } else {
    shouldThrow = false;
  }
  if (shouldThrow) {
    throw new AssertionError(errorMessage, void 0, ssfi);
  }
  if (doLength) {
    var descriptor = "length", itemsCount;
    if (objType === "map" || objType === "set") {
      descriptor = "size";
      itemsCount = obj.size;
    } else {
      itemsCount = obj.length;
    }
    this.assert(
      itemsCount > n,
      "expected #{this} to have a " + descriptor + " above #{exp} but got #{act}",
      "expected #{this} to not have a " + descriptor + " above #{exp}",
      n,
      itemsCount
    );
  } else {
    this.assert(
      obj > n,
      "expected #{this} to be above #{exp}",
      "expected #{this} to be at most #{exp}",
      n
    );
  }
}
__name(assertAbove, "assertAbove");
Assertion.addMethod("above", assertAbove);
Assertion.addMethod("gt", assertAbove);
Assertion.addMethod("greaterThan", assertAbove);
function assertLeast(n, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), doLength = flag2(this, "doLength"), flagMsg = flag2(this, "message"), msgPrefix = flagMsg ? flagMsg + ": " : "", ssfi = flag2(this, "ssfi"), objType = type(obj).toLowerCase(), nType = type(n).toLowerCase(), errorMessage, shouldThrow = true;
  if (doLength && objType !== "map" && objType !== "set") {
    new Assertion(obj, flagMsg, ssfi, true).to.have.property("length");
  }
  if (!doLength && (objType === "date" && nType !== "date")) {
    errorMessage = msgPrefix + "the argument to least must be a date";
  } else if (nType !== "number" && (doLength || objType === "number")) {
    errorMessage = msgPrefix + "the argument to least must be a number";
  } else if (!doLength && (objType !== "date" && objType !== "number")) {
    var printObj = objType === "string" ? "'" + obj + "'" : obj;
    errorMessage = msgPrefix + "expected " + printObj + " to be a number or a date";
  } else {
    shouldThrow = false;
  }
  if (shouldThrow) {
    throw new AssertionError(errorMessage, void 0, ssfi);
  }
  if (doLength) {
    var descriptor = "length", itemsCount;
    if (objType === "map" || objType === "set") {
      descriptor = "size";
      itemsCount = obj.size;
    } else {
      itemsCount = obj.length;
    }
    this.assert(
      itemsCount >= n,
      "expected #{this} to have a " + descriptor + " at least #{exp} but got #{act}",
      "expected #{this} to have a " + descriptor + " below #{exp}",
      n,
      itemsCount
    );
  } else {
    this.assert(
      obj >= n,
      "expected #{this} to be at least #{exp}",
      "expected #{this} to be below #{exp}",
      n
    );
  }
}
__name(assertLeast, "assertLeast");
Assertion.addMethod("least", assertLeast);
Assertion.addMethod("gte", assertLeast);
Assertion.addMethod("greaterThanOrEqual", assertLeast);
function assertBelow(n, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), doLength = flag2(this, "doLength"), flagMsg = flag2(this, "message"), msgPrefix = flagMsg ? flagMsg + ": " : "", ssfi = flag2(this, "ssfi"), objType = type(obj).toLowerCase(), nType = type(n).toLowerCase(), errorMessage, shouldThrow = true;
  if (doLength && objType !== "map" && objType !== "set") {
    new Assertion(obj, flagMsg, ssfi, true).to.have.property("length");
  }
  if (!doLength && (objType === "date" && nType !== "date")) {
    errorMessage = msgPrefix + "the argument to below must be a date";
  } else if (nType !== "number" && (doLength || objType === "number")) {
    errorMessage = msgPrefix + "the argument to below must be a number";
  } else if (!doLength && (objType !== "date" && objType !== "number")) {
    var printObj = objType === "string" ? "'" + obj + "'" : obj;
    errorMessage = msgPrefix + "expected " + printObj + " to be a number or a date";
  } else {
    shouldThrow = false;
  }
  if (shouldThrow) {
    throw new AssertionError(errorMessage, void 0, ssfi);
  }
  if (doLength) {
    var descriptor = "length", itemsCount;
    if (objType === "map" || objType === "set") {
      descriptor = "size";
      itemsCount = obj.size;
    } else {
      itemsCount = obj.length;
    }
    this.assert(
      itemsCount < n,
      "expected #{this} to have a " + descriptor + " below #{exp} but got #{act}",
      "expected #{this} to not have a " + descriptor + " below #{exp}",
      n,
      itemsCount
    );
  } else {
    this.assert(
      obj < n,
      "expected #{this} to be below #{exp}",
      "expected #{this} to be at least #{exp}",
      n
    );
  }
}
__name(assertBelow, "assertBelow");
Assertion.addMethod("below", assertBelow);
Assertion.addMethod("lt", assertBelow);
Assertion.addMethod("lessThan", assertBelow);
function assertMost(n, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), doLength = flag2(this, "doLength"), flagMsg = flag2(this, "message"), msgPrefix = flagMsg ? flagMsg + ": " : "", ssfi = flag2(this, "ssfi"), objType = type(obj).toLowerCase(), nType = type(n).toLowerCase(), errorMessage, shouldThrow = true;
  if (doLength && objType !== "map" && objType !== "set") {
    new Assertion(obj, flagMsg, ssfi, true).to.have.property("length");
  }
  if (!doLength && (objType === "date" && nType !== "date")) {
    errorMessage = msgPrefix + "the argument to most must be a date";
  } else if (nType !== "number" && (doLength || objType === "number")) {
    errorMessage = msgPrefix + "the argument to most must be a number";
  } else if (!doLength && (objType !== "date" && objType !== "number")) {
    var printObj = objType === "string" ? "'" + obj + "'" : obj;
    errorMessage = msgPrefix + "expected " + printObj + " to be a number or a date";
  } else {
    shouldThrow = false;
  }
  if (shouldThrow) {
    throw new AssertionError(errorMessage, void 0, ssfi);
  }
  if (doLength) {
    var descriptor = "length", itemsCount;
    if (objType === "map" || objType === "set") {
      descriptor = "size";
      itemsCount = obj.size;
    } else {
      itemsCount = obj.length;
    }
    this.assert(
      itemsCount <= n,
      "expected #{this} to have a " + descriptor + " at most #{exp} but got #{act}",
      "expected #{this} to have a " + descriptor + " above #{exp}",
      n,
      itemsCount
    );
  } else {
    this.assert(
      obj <= n,
      "expected #{this} to be at most #{exp}",
      "expected #{this} to be above #{exp}",
      n
    );
  }
}
__name(assertMost, "assertMost");
Assertion.addMethod("most", assertMost);
Assertion.addMethod("lte", assertMost);
Assertion.addMethod("lessThanOrEqual", assertMost);
Assertion.addMethod("within", function(start, finish, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), doLength = flag2(this, "doLength"), flagMsg = flag2(this, "message"), msgPrefix = flagMsg ? flagMsg + ": " : "", ssfi = flag2(this, "ssfi"), objType = type(obj).toLowerCase(), startType = type(start).toLowerCase(), finishType = type(finish).toLowerCase(), errorMessage, shouldThrow = true, range = startType === "date" && finishType === "date" ? start.toISOString() + ".." + finish.toISOString() : start + ".." + finish;
  if (doLength && objType !== "map" && objType !== "set") {
    new Assertion(obj, flagMsg, ssfi, true).to.have.property("length");
  }
  if (!doLength && (objType === "date" && (startType !== "date" || finishType !== "date"))) {
    errorMessage = msgPrefix + "the arguments to within must be dates";
  } else if ((startType !== "number" || finishType !== "number") && (doLength || objType === "number")) {
    errorMessage = msgPrefix + "the arguments to within must be numbers";
  } else if (!doLength && (objType !== "date" && objType !== "number")) {
    var printObj = objType === "string" ? "'" + obj + "'" : obj;
    errorMessage = msgPrefix + "expected " + printObj + " to be a number or a date";
  } else {
    shouldThrow = false;
  }
  if (shouldThrow) {
    throw new AssertionError(errorMessage, void 0, ssfi);
  }
  if (doLength) {
    var descriptor = "length", itemsCount;
    if (objType === "map" || objType === "set") {
      descriptor = "size";
      itemsCount = obj.size;
    } else {
      itemsCount = obj.length;
    }
    this.assert(
      itemsCount >= start && itemsCount <= finish,
      "expected #{this} to have a " + descriptor + " within " + range,
      "expected #{this} to not have a " + descriptor + " within " + range
    );
  } else {
    this.assert(
      obj >= start && obj <= finish,
      "expected #{this} to be within " + range,
      "expected #{this} to not be within " + range
    );
  }
});
function assertInstanceOf(constructor, msg) {
  if (msg)
    flag2(this, "message", msg);
  var target = flag2(this, "object");
  var ssfi = flag2(this, "ssfi");
  var flagMsg = flag2(this, "message");
  try {
    var isInstanceOf = target instanceof constructor;
  } catch (err) {
    if (err instanceof TypeError) {
      flagMsg = flagMsg ? flagMsg + ": " : "";
      throw new AssertionError(
        flagMsg + "The instanceof assertion needs a constructor but " + type(constructor) + " was given.",
        void 0,
        ssfi
      );
    }
    throw err;
  }
  var name = getName(constructor);
  if (name == null) {
    name = "an unnamed constructor";
  }
  this.assert(
    isInstanceOf,
    "expected #{this} to be an instance of " + name,
    "expected #{this} to not be an instance of " + name
  );
}
__name(assertInstanceOf, "assertInstanceOf");
Assertion.addMethod("instanceof", assertInstanceOf);
Assertion.addMethod("instanceOf", assertInstanceOf);
function assertProperty(name, val, msg) {
  if (msg)
    flag2(this, "message", msg);
  var isNested = flag2(this, "nested"), isOwn = flag2(this, "own"), flagMsg = flag2(this, "message"), obj = flag2(this, "object"), ssfi = flag2(this, "ssfi"), nameType = typeof name;
  flagMsg = flagMsg ? flagMsg + ": " : "";
  if (isNested) {
    if (nameType !== "string") {
      throw new AssertionError(
        flagMsg + "the argument to property must be a string when using nested syntax",
        void 0,
        ssfi
      );
    }
  } else {
    if (nameType !== "string" && nameType !== "number" && nameType !== "symbol") {
      throw new AssertionError(
        flagMsg + "the argument to property must be a string, number, or symbol",
        void 0,
        ssfi
      );
    }
  }
  if (isNested && isOwn) {
    throw new AssertionError(
      flagMsg + 'The "nested" and "own" flags cannot be combined.',
      void 0,
      ssfi
    );
  }
  if (obj === null || obj === void 0) {
    throw new AssertionError(
      flagMsg + "Target cannot be null or undefined.",
      void 0,
      ssfi
    );
  }
  var isDeep = flag2(this, "deep"), negate = flag2(this, "negate"), pathInfo = isNested ? getPathInfo(obj, name) : null, value = isNested ? pathInfo.value : obj[name], isEql = isDeep ? flag2(this, "eql") : (val1, val2) => val1 === val2;
  var descriptor = "";
  if (isDeep)
    descriptor += "deep ";
  if (isOwn)
    descriptor += "own ";
  if (isNested)
    descriptor += "nested ";
  descriptor += "property ";
  var hasProperty2;
  if (isOwn)
    hasProperty2 = Object.prototype.hasOwnProperty.call(obj, name);
  else if (isNested)
    hasProperty2 = pathInfo.exists;
  else
    hasProperty2 = hasProperty(obj, name);
  if (!negate || arguments.length === 1) {
    this.assert(
      hasProperty2,
      "expected #{this} to have " + descriptor + inspect2(name),
      "expected #{this} to not have " + descriptor + inspect2(name)
    );
  }
  if (arguments.length > 1) {
    this.assert(
      hasProperty2 && isEql(val, value),
      "expected #{this} to have " + descriptor + inspect2(name) + " of #{exp}, but got #{act}",
      "expected #{this} to not have " + descriptor + inspect2(name) + " of #{act}",
      val,
      value
    );
  }
  flag2(this, "object", value);
}
__name(assertProperty, "assertProperty");
Assertion.addMethod("property", assertProperty);
function assertOwnProperty(name, value, msg) {
  flag2(this, "own", true);
  assertProperty.apply(this, arguments);
}
__name(assertOwnProperty, "assertOwnProperty");
Assertion.addMethod("ownProperty", assertOwnProperty);
Assertion.addMethod("haveOwnProperty", assertOwnProperty);
function assertOwnPropertyDescriptor(name, descriptor, msg) {
  if (typeof descriptor === "string") {
    msg = descriptor;
    descriptor = null;
  }
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object");
  var actualDescriptor = Object.getOwnPropertyDescriptor(Object(obj), name);
  var eql = flag2(this, "eql");
  if (actualDescriptor && descriptor) {
    this.assert(
      eql(descriptor, actualDescriptor),
      "expected the own property descriptor for " + inspect2(name) + " on #{this} to match " + inspect2(descriptor) + ", got " + inspect2(actualDescriptor),
      "expected the own property descriptor for " + inspect2(name) + " on #{this} to not match " + inspect2(descriptor),
      descriptor,
      actualDescriptor,
      true
    );
  } else {
    this.assert(
      actualDescriptor,
      "expected #{this} to have an own property descriptor for " + inspect2(name),
      "expected #{this} to not have an own property descriptor for " + inspect2(name)
    );
  }
  flag2(this, "object", actualDescriptor);
}
__name(assertOwnPropertyDescriptor, "assertOwnPropertyDescriptor");
Assertion.addMethod("ownPropertyDescriptor", assertOwnPropertyDescriptor);
Assertion.addMethod("haveOwnPropertyDescriptor", assertOwnPropertyDescriptor);
function assertLengthChain() {
  flag2(this, "doLength", true);
}
__name(assertLengthChain, "assertLengthChain");
function assertLength(n, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), objType = type(obj).toLowerCase(), flagMsg = flag2(this, "message"), ssfi = flag2(this, "ssfi"), descriptor = "length", itemsCount;
  switch (objType) {
    case "map":
    case "set":
      descriptor = "size";
      itemsCount = obj.size;
      break;
    default:
      new Assertion(obj, flagMsg, ssfi, true).to.have.property("length");
      itemsCount = obj.length;
  }
  this.assert(
    itemsCount == n,
    "expected #{this} to have a " + descriptor + " of #{exp} but got #{act}",
    "expected #{this} to not have a " + descriptor + " of #{act}",
    n,
    itemsCount
  );
}
__name(assertLength, "assertLength");
Assertion.addChainableMethod("length", assertLength, assertLengthChain);
Assertion.addChainableMethod("lengthOf", assertLength, assertLengthChain);
function assertMatch(re, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object");
  this.assert(
    re.exec(obj),
    "expected #{this} to match " + re,
    "expected #{this} not to match " + re
  );
}
__name(assertMatch, "assertMatch");
Assertion.addMethod("match", assertMatch);
Assertion.addMethod("matches", assertMatch);
Assertion.addMethod("string", function(str, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), flagMsg = flag2(this, "message"), ssfi = flag2(this, "ssfi");
  new Assertion(obj, flagMsg, ssfi, true).is.a("string");
  this.assert(
    ~obj.indexOf(str),
    "expected #{this} to contain " + inspect2(str),
    "expected #{this} to not contain " + inspect2(str)
  );
});
function assertKeys(keys) {
  var obj = flag2(this, "object"), objType = type(obj), keysType = type(keys), ssfi = flag2(this, "ssfi"), isDeep = flag2(this, "deep"), str, deepStr = "", actual, ok = true, flagMsg = flag2(this, "message");
  flagMsg = flagMsg ? flagMsg + ": " : "";
  var mixedArgsMsg = flagMsg + "when testing keys against an object or an array you must give a single Array|Object|String argument or multiple String arguments";
  if (objType === "Map" || objType === "Set") {
    deepStr = isDeep ? "deeply " : "";
    actual = [];
    obj.forEach(function(val, key) {
      actual.push(key);
    });
    if (keysType !== "Array") {
      keys = Array.prototype.slice.call(arguments);
    }
  } else {
    actual = getOwnEnumerableProperties(obj);
    switch (keysType) {
      case "Array":
        if (arguments.length > 1) {
          throw new AssertionError(mixedArgsMsg, void 0, ssfi);
        }
        break;
      case "Object":
        if (arguments.length > 1) {
          throw new AssertionError(mixedArgsMsg, void 0, ssfi);
        }
        keys = Object.keys(keys);
        break;
      default:
        keys = Array.prototype.slice.call(arguments);
    }
    keys = keys.map(function(val) {
      return typeof val === "symbol" ? val : String(val);
    });
  }
  if (!keys.length) {
    throw new AssertionError(flagMsg + "keys required", void 0, ssfi);
  }
  var len = keys.length, any = flag2(this, "any"), all = flag2(this, "all"), expected = keys, isEql = isDeep ? flag2(this, "eql") : (val1, val2) => val1 === val2;
  if (!any && !all) {
    all = true;
  }
  if (any) {
    ok = expected.some(function(expectedKey) {
      return actual.some(function(actualKey) {
        return isEql(expectedKey, actualKey);
      });
    });
  }
  if (all) {
    ok = expected.every(function(expectedKey) {
      return actual.some(function(actualKey) {
        return isEql(expectedKey, actualKey);
      });
    });
    if (!flag2(this, "contains")) {
      ok = ok && keys.length == actual.length;
    }
  }
  if (len > 1) {
    keys = keys.map(function(key) {
      return inspect2(key);
    });
    var last = keys.pop();
    if (all) {
      str = keys.join(", ") + ", and " + last;
    }
    if (any) {
      str = keys.join(", ") + ", or " + last;
    }
  } else {
    str = inspect2(keys[0]);
  }
  str = (len > 1 ? "keys " : "key ") + str;
  str = (flag2(this, "contains") ? "contain " : "have ") + str;
  this.assert(
    ok,
    "expected #{this} to " + deepStr + str,
    "expected #{this} to not " + deepStr + str,
    expected.slice(0).sort(compareByInspect),
    actual.sort(compareByInspect),
    true
  );
}
__name(assertKeys, "assertKeys");
Assertion.addMethod("keys", assertKeys);
Assertion.addMethod("key", assertKeys);
function assertThrows(errorLike, errMsgMatcher, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), ssfi = flag2(this, "ssfi"), flagMsg = flag2(this, "message"), negate = flag2(this, "negate") || false;
  new Assertion(obj, flagMsg, ssfi, true).is.a("function");
  if (errorLike instanceof RegExp || typeof errorLike === "string") {
    errMsgMatcher = errorLike;
    errorLike = null;
  }
  var caughtErr;
  try {
    obj();
  } catch (err) {
    caughtErr = err;
  }
  var everyArgIsUndefined = errorLike === void 0 && errMsgMatcher === void 0;
  var everyArgIsDefined = Boolean(errorLike && errMsgMatcher);
  var errorLikeFail = false;
  var errMsgMatcherFail = false;
  if (everyArgIsUndefined || !everyArgIsUndefined && !negate) {
    var errorLikeString = "an error";
    if (errorLike instanceof Error) {
      errorLikeString = "#{exp}";
    } else if (errorLike) {
      errorLikeString = check_error_exports.getConstructorName(errorLike);
    }
    this.assert(
      caughtErr,
      "expected #{this} to throw " + errorLikeString,
      "expected #{this} to not throw an error but #{act} was thrown",
      errorLike && errorLike.toString(),
      caughtErr instanceof Error ? caughtErr.toString() : typeof caughtErr === "string" ? caughtErr : caughtErr && check_error_exports.getConstructorName(caughtErr)
    );
  }
  if (errorLike && caughtErr) {
    if (errorLike instanceof Error) {
      var isCompatibleInstance = check_error_exports.compatibleInstance(caughtErr, errorLike);
      if (isCompatibleInstance === negate) {
        if (everyArgIsDefined && negate) {
          errorLikeFail = true;
        } else {
          this.assert(
            negate,
            "expected #{this} to throw #{exp} but #{act} was thrown",
            "expected #{this} to not throw #{exp}" + (caughtErr && !negate ? " but #{act} was thrown" : ""),
            errorLike.toString(),
            caughtErr.toString()
          );
        }
      }
    }
    var isCompatibleConstructor = check_error_exports.compatibleConstructor(caughtErr, errorLike);
    if (isCompatibleConstructor === negate) {
      if (everyArgIsDefined && negate) {
        errorLikeFail = true;
      } else {
        this.assert(
          negate,
          "expected #{this} to throw #{exp} but #{act} was thrown",
          "expected #{this} to not throw #{exp}" + (caughtErr ? " but #{act} was thrown" : ""),
          errorLike instanceof Error ? errorLike.toString() : errorLike && check_error_exports.getConstructorName(errorLike),
          caughtErr instanceof Error ? caughtErr.toString() : caughtErr && check_error_exports.getConstructorName(caughtErr)
        );
      }
    }
  }
  if (caughtErr && errMsgMatcher !== void 0 && errMsgMatcher !== null) {
    var placeholder = "including";
    if (errMsgMatcher instanceof RegExp) {
      placeholder = "matching";
    }
    var isCompatibleMessage = check_error_exports.compatibleMessage(caughtErr, errMsgMatcher);
    if (isCompatibleMessage === negate) {
      if (everyArgIsDefined && negate) {
        errMsgMatcherFail = true;
      } else {
        this.assert(
          negate,
          "expected #{this} to throw error " + placeholder + " #{exp} but got #{act}",
          "expected #{this} to throw error not " + placeholder + " #{exp}",
          errMsgMatcher,
          check_error_exports.getMessage(caughtErr)
        );
      }
    }
  }
  if (errorLikeFail && errMsgMatcherFail) {
    this.assert(
      negate,
      "expected #{this} to throw #{exp} but #{act} was thrown",
      "expected #{this} to not throw #{exp}" + (caughtErr ? " but #{act} was thrown" : ""),
      errorLike instanceof Error ? errorLike.toString() : errorLike && check_error_exports.getConstructorName(errorLike),
      caughtErr instanceof Error ? caughtErr.toString() : caughtErr && check_error_exports.getConstructorName(caughtErr)
    );
  }
  flag2(this, "object", caughtErr);
}
__name(assertThrows, "assertThrows");
Assertion.addMethod("throw", assertThrows);
Assertion.addMethod("throws", assertThrows);
Assertion.addMethod("Throw", assertThrows);
function respondTo(method, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), itself = flag2(this, "itself"), context = "function" === typeof obj && !itself ? obj.prototype[method] : obj[method];
  this.assert(
    "function" === typeof context,
    "expected #{this} to respond to " + inspect2(method),
    "expected #{this} to not respond to " + inspect2(method)
  );
}
__name(respondTo, "respondTo");
Assertion.addMethod("respondTo", respondTo);
Assertion.addMethod("respondsTo", respondTo);
Assertion.addProperty("itself", function() {
  flag2(this, "itself", true);
});
function satisfy(matcher, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object");
  var result = matcher(obj);
  this.assert(
    result,
    "expected #{this} to satisfy " + objDisplay(matcher),
    "expected #{this} to not satisfy" + objDisplay(matcher),
    flag2(this, "negate") ? false : true,
    result
  );
}
__name(satisfy, "satisfy");
Assertion.addMethod("satisfy", satisfy);
Assertion.addMethod("satisfies", satisfy);
function closeTo(expected, delta, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), flagMsg = flag2(this, "message"), ssfi = flag2(this, "ssfi");
  new Assertion(obj, flagMsg, ssfi, true).is.a("number");
  if (typeof expected !== "number" || typeof delta !== "number") {
    flagMsg = flagMsg ? flagMsg + ": " : "";
    var deltaMessage = delta === void 0 ? ", and a delta is required" : "";
    throw new AssertionError(
      flagMsg + "the arguments to closeTo or approximately must be numbers" + deltaMessage,
      void 0,
      ssfi
    );
  }
  this.assert(
    Math.abs(obj - expected) <= delta,
    "expected #{this} to be close to " + expected + " +/- " + delta,
    "expected #{this} not to be close to " + expected + " +/- " + delta
  );
}
__name(closeTo, "closeTo");
Assertion.addMethod("closeTo", closeTo);
Assertion.addMethod("approximately", closeTo);
function isSubsetOf(subset, superset, cmp, contains, ordered) {
  if (!contains) {
    if (subset.length !== superset.length)
      return false;
    superset = superset.slice();
  }
  return subset.every(function(elem, idx) {
    if (ordered)
      return cmp ? cmp(elem, superset[idx]) : elem === superset[idx];
    if (!cmp) {
      var matchIdx = superset.indexOf(elem);
      if (matchIdx === -1)
        return false;
      if (!contains)
        superset.splice(matchIdx, 1);
      return true;
    }
    return superset.some(function(elem2, matchIdx2) {
      if (!cmp(elem, elem2))
        return false;
      if (!contains)
        superset.splice(matchIdx2, 1);
      return true;
    });
  });
}
__name(isSubsetOf, "isSubsetOf");
Assertion.addMethod("members", function(subset, msg) {
  if (msg)
    flag2(this, "message", msg);
  var obj = flag2(this, "object"), flagMsg = flag2(this, "message"), ssfi = flag2(this, "ssfi");
  new Assertion(obj, flagMsg, ssfi, true).to.be.an("array");
  new Assertion(subset, flagMsg, ssfi, true).to.be.an("array");
  var contains = flag2(this, "contains");
  var ordered = flag2(this, "ordered");
  var subject, failMsg, failNegateMsg;
  if (contains) {
    subject = ordered ? "an ordered superset" : "a superset";
    failMsg = "expected #{this} to be " + subject + " of #{exp}";
    failNegateMsg = "expected #{this} to not be " + subject + " of #{exp}";
  } else {
    subject = ordered ? "ordered members" : "members";
    failMsg = "expected #{this} to have the same " + subject + " as #{exp}";
    failNegateMsg = "expected #{this} to not have the same " + subject + " as #{exp}";
  }
  var cmp = flag2(this, "deep") ? flag2(this, "eql") : void 0;
  this.assert(
    isSubsetOf(subset, obj, cmp, contains, ordered),
    failMsg,
    failNegateMsg,
    subset,
    obj,
    true
  );
});
function oneOf(list, msg) {
  if (msg)
    flag2(this, "message", msg);
  var expected = flag2(this, "object"), flagMsg = flag2(this, "message"), ssfi = flag2(this, "ssfi"), contains = flag2(this, "contains"), isDeep = flag2(this, "deep"), eql = flag2(this, "eql");
  new Assertion(list, flagMsg, ssfi, true).to.be.an("array");
  if (contains) {
    this.assert(
      list.some(function(possibility) {
        return expected.indexOf(possibility) > -1;
      }),
      "expected #{this} to contain one of #{exp}",
      "expected #{this} to not contain one of #{exp}",
      list,
      expected
    );
  } else {
    if (isDeep) {
      this.assert(
        list.some(function(possibility) {
          return eql(expected, possibility);
        }),
        "expected #{this} to deeply equal one of #{exp}",
        "expected #{this} to deeply equal one of #{exp}",
        list,
        expected
      );
    } else {
      this.assert(
        list.indexOf(expected) > -1,
        "expected #{this} to be one of #{exp}",
        "expected #{this} to not be one of #{exp}",
        list,
        expected
      );
    }
  }
}
__name(oneOf, "oneOf");
Assertion.addMethod("oneOf", oneOf);
function assertChanges(subject, prop, msg) {
  if (msg)
    flag2(this, "message", msg);
  var fn = flag2(this, "object"), flagMsg = flag2(this, "message"), ssfi = flag2(this, "ssfi");
  new Assertion(fn, flagMsg, ssfi, true).is.a("function");
  var initial;
  if (!prop) {
    new Assertion(subject, flagMsg, ssfi, true).is.a("function");
    initial = subject();
  } else {
    new Assertion(subject, flagMsg, ssfi, true).to.have.property(prop);
    initial = subject[prop];
  }
  fn();
  var final = prop === void 0 || prop === null ? subject() : subject[prop];
  var msgObj = prop === void 0 || prop === null ? initial : "." + prop;
  flag2(this, "deltaMsgObj", msgObj);
  flag2(this, "initialDeltaValue", initial);
  flag2(this, "finalDeltaValue", final);
  flag2(this, "deltaBehavior", "change");
  flag2(this, "realDelta", final !== initial);
  this.assert(
    initial !== final,
    "expected " + msgObj + " to change",
    "expected " + msgObj + " to not change"
  );
}
__name(assertChanges, "assertChanges");
Assertion.addMethod("change", assertChanges);
Assertion.addMethod("changes", assertChanges);
function assertIncreases(subject, prop, msg) {
  if (msg)
    flag2(this, "message", msg);
  var fn = flag2(this, "object"), flagMsg = flag2(this, "message"), ssfi = flag2(this, "ssfi");
  new Assertion(fn, flagMsg, ssfi, true).is.a("function");
  var initial;
  if (!prop) {
    new Assertion(subject, flagMsg, ssfi, true).is.a("function");
    initial = subject();
  } else {
    new Assertion(subject, flagMsg, ssfi, true).to.have.property(prop);
    initial = subject[prop];
  }
  new Assertion(initial, flagMsg, ssfi, true).is.a("number");
  fn();
  var final = prop === void 0 || prop === null ? subject() : subject[prop];
  var msgObj = prop === void 0 || prop === null ? initial : "." + prop;
  flag2(this, "deltaMsgObj", msgObj);
  flag2(this, "initialDeltaValue", initial);
  flag2(this, "finalDeltaValue", final);
  flag2(this, "deltaBehavior", "increase");
  flag2(this, "realDelta", final - initial);
  this.assert(
    final - initial > 0,
    "expected " + msgObj + " to increase",
    "expected " + msgObj + " to not increase"
  );
}
__name(assertIncreases, "assertIncreases");
Assertion.addMethod("increase", assertIncreases);
Assertion.addMethod("increases", assertIncreases);
function assertDecreases(subject, prop, msg) {
  if (msg)
    flag2(this, "message", msg);
  var fn = flag2(this, "object"), flagMsg = flag2(this, "message"), ssfi = flag2(this, "ssfi");
  new Assertion(fn, flagMsg, ssfi, true).is.a("function");
  var initial;
  if (!prop) {
    new Assertion(subject, flagMsg, ssfi, true).is.a("function");
    initial = subject();
  } else {
    new Assertion(subject, flagMsg, ssfi, true).to.have.property(prop);
    initial = subject[prop];
  }
  new Assertion(initial, flagMsg, ssfi, true).is.a("number");
  fn();
  var final = prop === void 0 || prop === null ? subject() : subject[prop];
  var msgObj = prop === void 0 || prop === null ? initial : "." + prop;
  flag2(this, "deltaMsgObj", msgObj);
  flag2(this, "initialDeltaValue", initial);
  flag2(this, "finalDeltaValue", final);
  flag2(this, "deltaBehavior", "decrease");
  flag2(this, "realDelta", initial - final);
  this.assert(
    final - initial < 0,
    "expected " + msgObj + " to decrease",
    "expected " + msgObj + " to not decrease"
  );
}
__name(assertDecreases, "assertDecreases");
Assertion.addMethod("decrease", assertDecreases);
Assertion.addMethod("decreases", assertDecreases);
function assertDelta(delta, msg) {
  if (msg)
    flag2(this, "message", msg);
  var msgObj = flag2(this, "deltaMsgObj");
  var initial = flag2(this, "initialDeltaValue");
  var final = flag2(this, "finalDeltaValue");
  var behavior = flag2(this, "deltaBehavior");
  var realDelta = flag2(this, "realDelta");
  var expression;
  if (behavior === "change") {
    expression = Math.abs(final - initial) === Math.abs(delta);
  } else {
    expression = realDelta === Math.abs(delta);
  }
  this.assert(
    expression,
    "expected " + msgObj + " to " + behavior + " by " + delta,
    "expected " + msgObj + " to not " + behavior + " by " + delta
  );
}
__name(assertDelta, "assertDelta");
Assertion.addMethod("by", assertDelta);
Assertion.addProperty("extensible", function() {
  var obj = flag2(this, "object");
  var isExtensible = obj === Object(obj) && Object.isExtensible(obj);
  this.assert(
    isExtensible,
    "expected #{this} to be extensible",
    "expected #{this} to not be extensible"
  );
});
Assertion.addProperty("sealed", function() {
  var obj = flag2(this, "object");
  var isSealed = obj === Object(obj) ? Object.isSealed(obj) : true;
  this.assert(
    isSealed,
    "expected #{this} to be sealed",
    "expected #{this} to not be sealed"
  );
});
Assertion.addProperty("frozen", function() {
  var obj = flag2(this, "object");
  var isFrozen = obj === Object(obj) ? Object.isFrozen(obj) : true;
  this.assert(
    isFrozen,
    "expected #{this} to be frozen",
    "expected #{this} to not be frozen"
  );
});
Assertion.addProperty("finite", function(msg) {
  var obj = flag2(this, "object");
  this.assert(
    typeof obj === "number" && isFinite(obj),
    "expected #{this} to be a finite number",
    "expected #{this} to not be a finite number"
  );
});

// lib/chai/interface/expect.js
function expect(val, message) {
  return new Assertion(val, message);
}
__name(expect, "expect");
expect.fail = function(actual, expected, message, operator) {
  if (arguments.length < 2) {
    message = actual;
    actual = void 0;
  }
  message = message || "expect.fail()";
  throw new AssertionError(message, {
    actual,
    expected,
    operator
  }, expect.fail);
};

// lib/chai/interface/should.js
var should_exports = {};
__export(should_exports, {
  Should: () => Should,
  should: () => should
});
function loadShould() {
  function shouldGetter() {
    if (this instanceof String || this instanceof Number || this instanceof Boolean || typeof Symbol === "function" && this instanceof Symbol || typeof BigInt === "function" && this instanceof BigInt) {
      return new Assertion(this.valueOf(), null, shouldGetter);
    }
    return new Assertion(this, null, shouldGetter);
  }
  __name(shouldGetter, "shouldGetter");
  function shouldSetter(value) {
    Object.defineProperty(this, "should", {
      value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  }
  __name(shouldSetter, "shouldSetter");
  Object.defineProperty(Object.prototype, "should", {
    set: shouldSetter,
    get: shouldGetter,
    configurable: true
  });
  var should2 = {};
  should2.fail = function(actual, expected, message, operator) {
    if (arguments.length < 2) {
      message = actual;
      actual = void 0;
    }
    message = message || "should.fail()";
    throw new AssertionError(message, {
      actual,
      expected,
      operator
    }, should2.fail);
  };
  should2.equal = function(val1, val2, msg) {
    new Assertion(val1, msg).to.equal(val2);
  };
  should2.Throw = function(fn, errt, errs, msg) {
    new Assertion(fn, msg).to.Throw(errt, errs);
  };
  should2.exist = function(val, msg) {
    new Assertion(val, msg).to.exist;
  };
  should2.not = {};
  should2.not.equal = function(val1, val2, msg) {
    new Assertion(val1, msg).to.not.equal(val2);
  };
  should2.not.Throw = function(fn, errt, errs, msg) {
    new Assertion(fn, msg).to.not.Throw(errt, errs);
  };
  should2.not.exist = function(val, msg) {
    new Assertion(val, msg).to.not.exist;
  };
  should2["throw"] = should2["Throw"];
  should2.not["throw"] = should2.not["Throw"];
  return should2;
}
__name(loadShould, "loadShould");
var should = loadShould;
var Should = loadShould;

// lib/chai/interface/assert.js
function assert(express, errmsg) {
  var test2 = new Assertion(null, null, assert, true);
  test2.assert(
    express,
    errmsg,
    "[ negation message unavailable ]"
  );
}
__name(assert, "assert");
assert.fail = function(actual, expected, message, operator) {
  if (arguments.length < 2) {
    message = actual;
    actual = void 0;
  }
  message = message || "assert.fail()";
  throw new AssertionError(message, {
    actual,
    expected,
    operator
  }, assert.fail);
};
assert.isOk = function(val, msg) {
  new Assertion(val, msg, assert.isOk, true).is.ok;
};
assert.isNotOk = function(val, msg) {
  new Assertion(val, msg, assert.isNotOk, true).is.not.ok;
};
assert.equal = function(act, exp, msg) {
  var test2 = new Assertion(act, msg, assert.equal, true);
  test2.assert(
    exp == flag(test2, "object"),
    "expected #{this} to equal #{exp}",
    "expected #{this} to not equal #{act}",
    exp,
    act,
    true
  );
};
assert.notEqual = function(act, exp, msg) {
  var test2 = new Assertion(act, msg, assert.notEqual, true);
  test2.assert(
    exp != flag(test2, "object"),
    "expected #{this} to not equal #{exp}",
    "expected #{this} to equal #{act}",
    exp,
    act,
    true
  );
};
assert.strictEqual = function(act, exp, msg) {
  new Assertion(act, msg, assert.strictEqual, true).to.equal(exp);
};
assert.notStrictEqual = function(act, exp, msg) {
  new Assertion(act, msg, assert.notStrictEqual, true).to.not.equal(exp);
};
assert.deepEqual = assert.deepStrictEqual = function(act, exp, msg) {
  new Assertion(act, msg, assert.deepEqual, true).to.eql(exp);
};
assert.notDeepEqual = function(act, exp, msg) {
  new Assertion(act, msg, assert.notDeepEqual, true).to.not.eql(exp);
};
assert.isAbove = function(val, abv, msg) {
  new Assertion(val, msg, assert.isAbove, true).to.be.above(abv);
};
assert.isAtLeast = function(val, atlst, msg) {
  new Assertion(val, msg, assert.isAtLeast, true).to.be.least(atlst);
};
assert.isBelow = function(val, blw, msg) {
  new Assertion(val, msg, assert.isBelow, true).to.be.below(blw);
};
assert.isAtMost = function(val, atmst, msg) {
  new Assertion(val, msg, assert.isAtMost, true).to.be.most(atmst);
};
assert.isTrue = function(val, msg) {
  new Assertion(val, msg, assert.isTrue, true).is["true"];
};
assert.isNotTrue = function(val, msg) {
  new Assertion(val, msg, assert.isNotTrue, true).to.not.equal(true);
};
assert.isFalse = function(val, msg) {
  new Assertion(val, msg, assert.isFalse, true).is["false"];
};
assert.isNotFalse = function(val, msg) {
  new Assertion(val, msg, assert.isNotFalse, true).to.not.equal(false);
};
assert.isNull = function(val, msg) {
  new Assertion(val, msg, assert.isNull, true).to.equal(null);
};
assert.isNotNull = function(val, msg) {
  new Assertion(val, msg, assert.isNotNull, true).to.not.equal(null);
};
assert.isNaN = function(val, msg) {
  new Assertion(val, msg, assert.isNaN, true).to.be.NaN;
};
assert.isNotNaN = function(val, msg) {
  new Assertion(val, msg, assert.isNotNaN, true).not.to.be.NaN;
};
assert.exists = function(val, msg) {
  new Assertion(val, msg, assert.exists, true).to.exist;
};
assert.notExists = function(val, msg) {
  new Assertion(val, msg, assert.notExists, true).to.not.exist;
};
assert.isUndefined = function(val, msg) {
  new Assertion(val, msg, assert.isUndefined, true).to.equal(void 0);
};
assert.isDefined = function(val, msg) {
  new Assertion(val, msg, assert.isDefined, true).to.not.equal(void 0);
};
assert.isCallable = function(val, msg) {
  new Assertion(val, msg, assert.isCallable, true).is.callable;
};
assert.isNotCallable = function(val, msg) {
  new Assertion(val, msg, assert.isNotCallable, true).is.not.callable;
};
assert.isObject = function(val, msg) {
  new Assertion(val, msg, assert.isObject, true).to.be.a("object");
};
assert.isNotObject = function(val, msg) {
  new Assertion(val, msg, assert.isNotObject, true).to.not.be.a("object");
};
assert.isArray = function(val, msg) {
  new Assertion(val, msg, assert.isArray, true).to.be.an("array");
};
assert.isNotArray = function(val, msg) {
  new Assertion(val, msg, assert.isNotArray, true).to.not.be.an("array");
};
assert.isString = function(val, msg) {
  new Assertion(val, msg, assert.isString, true).to.be.a("string");
};
assert.isNotString = function(val, msg) {
  new Assertion(val, msg, assert.isNotString, true).to.not.be.a("string");
};
assert.isNumber = function(val, msg) {
  new Assertion(val, msg, assert.isNumber, true).to.be.a("number");
};
assert.isNotNumber = function(val, msg) {
  new Assertion(val, msg, assert.isNotNumber, true).to.not.be.a("number");
};
assert.isFinite = function(val, msg) {
  new Assertion(val, msg, assert.isFinite, true).to.be.finite;
};
assert.isBoolean = function(val, msg) {
  new Assertion(val, msg, assert.isBoolean, true).to.be.a("boolean");
};
assert.isNotBoolean = function(val, msg) {
  new Assertion(val, msg, assert.isNotBoolean, true).to.not.be.a("boolean");
};
assert.typeOf = function(val, type3, msg) {
  new Assertion(val, msg, assert.typeOf, true).to.be.a(type3);
};
assert.notTypeOf = function(val, type3, msg) {
  new Assertion(val, msg, assert.notTypeOf, true).to.not.be.a(type3);
};
assert.instanceOf = function(val, type3, msg) {
  new Assertion(val, msg, assert.instanceOf, true).to.be.instanceOf(type3);
};
assert.notInstanceOf = function(val, type3, msg) {
  new Assertion(val, msg, assert.notInstanceOf, true).to.not.be.instanceOf(type3);
};
assert.include = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.include, true).include(inc);
};
assert.notInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.notInclude, true).not.include(inc);
};
assert.deepInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.deepInclude, true).deep.include(inc);
};
assert.notDeepInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.notDeepInclude, true).not.deep.include(inc);
};
assert.nestedInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.nestedInclude, true).nested.include(inc);
};
assert.notNestedInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.notNestedInclude, true).not.nested.include(inc);
};
assert.deepNestedInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.deepNestedInclude, true).deep.nested.include(inc);
};
assert.notDeepNestedInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.notDeepNestedInclude, true).not.deep.nested.include(inc);
};
assert.ownInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.ownInclude, true).own.include(inc);
};
assert.notOwnInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.notOwnInclude, true).not.own.include(inc);
};
assert.deepOwnInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.deepOwnInclude, true).deep.own.include(inc);
};
assert.notDeepOwnInclude = function(exp, inc, msg) {
  new Assertion(exp, msg, assert.notDeepOwnInclude, true).not.deep.own.include(inc);
};
assert.match = function(exp, re, msg) {
  new Assertion(exp, msg, assert.match, true).to.match(re);
};
assert.notMatch = function(exp, re, msg) {
  new Assertion(exp, msg, assert.notMatch, true).to.not.match(re);
};
assert.property = function(obj, prop, msg) {
  new Assertion(obj, msg, assert.property, true).to.have.property(prop);
};
assert.notProperty = function(obj, prop, msg) {
  new Assertion(obj, msg, assert.notProperty, true).to.not.have.property(prop);
};
assert.propertyVal = function(obj, prop, val, msg) {
  new Assertion(obj, msg, assert.propertyVal, true).to.have.property(prop, val);
};
assert.notPropertyVal = function(obj, prop, val, msg) {
  new Assertion(obj, msg, assert.notPropertyVal, true).to.not.have.property(prop, val);
};
assert.deepPropertyVal = function(obj, prop, val, msg) {
  new Assertion(obj, msg, assert.deepPropertyVal, true).to.have.deep.property(prop, val);
};
assert.notDeepPropertyVal = function(obj, prop, val, msg) {
  new Assertion(obj, msg, assert.notDeepPropertyVal, true).to.not.have.deep.property(prop, val);
};
assert.ownProperty = function(obj, prop, msg) {
  new Assertion(obj, msg, assert.ownProperty, true).to.have.own.property(prop);
};
assert.notOwnProperty = function(obj, prop, msg) {
  new Assertion(obj, msg, assert.notOwnProperty, true).to.not.have.own.property(prop);
};
assert.ownPropertyVal = function(obj, prop, value, msg) {
  new Assertion(obj, msg, assert.ownPropertyVal, true).to.have.own.property(prop, value);
};
assert.notOwnPropertyVal = function(obj, prop, value, msg) {
  new Assertion(obj, msg, assert.notOwnPropertyVal, true).to.not.have.own.property(prop, value);
};
assert.deepOwnPropertyVal = function(obj, prop, value, msg) {
  new Assertion(obj, msg, assert.deepOwnPropertyVal, true).to.have.deep.own.property(prop, value);
};
assert.notDeepOwnPropertyVal = function(obj, prop, value, msg) {
  new Assertion(obj, msg, assert.notDeepOwnPropertyVal, true).to.not.have.deep.own.property(prop, value);
};
assert.nestedProperty = function(obj, prop, msg) {
  new Assertion(obj, msg, assert.nestedProperty, true).to.have.nested.property(prop);
};
assert.notNestedProperty = function(obj, prop, msg) {
  new Assertion(obj, msg, assert.notNestedProperty, true).to.not.have.nested.property(prop);
};
assert.nestedPropertyVal = function(obj, prop, val, msg) {
  new Assertion(obj, msg, assert.nestedPropertyVal, true).to.have.nested.property(prop, val);
};
assert.notNestedPropertyVal = function(obj, prop, val, msg) {
  new Assertion(obj, msg, assert.notNestedPropertyVal, true).to.not.have.nested.property(prop, val);
};
assert.deepNestedPropertyVal = function(obj, prop, val, msg) {
  new Assertion(obj, msg, assert.deepNestedPropertyVal, true).to.have.deep.nested.property(prop, val);
};
assert.notDeepNestedPropertyVal = function(obj, prop, val, msg) {
  new Assertion(obj, msg, assert.notDeepNestedPropertyVal, true).to.not.have.deep.nested.property(prop, val);
};
assert.lengthOf = function(exp, len, msg) {
  new Assertion(exp, msg, assert.lengthOf, true).to.have.lengthOf(len);
};
assert.hasAnyKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.hasAnyKeys, true).to.have.any.keys(keys);
};
assert.hasAllKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.hasAllKeys, true).to.have.all.keys(keys);
};
assert.containsAllKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.containsAllKeys, true).to.contain.all.keys(keys);
};
assert.doesNotHaveAnyKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.doesNotHaveAnyKeys, true).to.not.have.any.keys(keys);
};
assert.doesNotHaveAllKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.doesNotHaveAllKeys, true).to.not.have.all.keys(keys);
};
assert.hasAnyDeepKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.hasAnyDeepKeys, true).to.have.any.deep.keys(keys);
};
assert.hasAllDeepKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.hasAllDeepKeys, true).to.have.all.deep.keys(keys);
};
assert.containsAllDeepKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.containsAllDeepKeys, true).to.contain.all.deep.keys(keys);
};
assert.doesNotHaveAnyDeepKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.doesNotHaveAnyDeepKeys, true).to.not.have.any.deep.keys(keys);
};
assert.doesNotHaveAllDeepKeys = function(obj, keys, msg) {
  new Assertion(obj, msg, assert.doesNotHaveAllDeepKeys, true).to.not.have.all.deep.keys(keys);
};
assert.throws = function(fn, errorLike, errMsgMatcher, msg) {
  if ("string" === typeof errorLike || errorLike instanceof RegExp) {
    errMsgMatcher = errorLike;
    errorLike = null;
  }
  var assertErr = new Assertion(fn, msg, assert.throws, true).to.throw(errorLike, errMsgMatcher);
  return flag(assertErr, "object");
};
assert.doesNotThrow = function(fn, errorLike, errMsgMatcher, msg) {
  if ("string" === typeof errorLike || errorLike instanceof RegExp) {
    errMsgMatcher = errorLike;
    errorLike = null;
  }
  new Assertion(fn, msg, assert.doesNotThrow, true).to.not.throw(errorLike, errMsgMatcher);
};
assert.operator = function(val, operator, val2, msg) {
  var ok;
  switch (operator) {
    case "==":
      ok = val == val2;
      break;
    case "===":
      ok = val === val2;
      break;
    case ">":
      ok = val > val2;
      break;
    case ">=":
      ok = val >= val2;
      break;
    case "<":
      ok = val < val2;
      break;
    case "<=":
      ok = val <= val2;
      break;
    case "!=":
      ok = val != val2;
      break;
    case "!==":
      ok = val !== val2;
      break;
    default:
      msg = msg ? msg + ": " : msg;
      throw new AssertionError(
        msg + 'Invalid operator "' + operator + '"',
        void 0,
        assert.operator
      );
  }
  var test2 = new Assertion(ok, msg, assert.operator, true);
  test2.assert(
    true === flag(test2, "object"),
    "expected " + inspect2(val) + " to be " + operator + " " + inspect2(val2),
    "expected " + inspect2(val) + " to not be " + operator + " " + inspect2(val2)
  );
};
assert.closeTo = function(act, exp, delta, msg) {
  new Assertion(act, msg, assert.closeTo, true).to.be.closeTo(exp, delta);
};
assert.approximately = function(act, exp, delta, msg) {
  new Assertion(act, msg, assert.approximately, true).to.be.approximately(exp, delta);
};
assert.sameMembers = function(set1, set2, msg) {
  new Assertion(set1, msg, assert.sameMembers, true).to.have.same.members(set2);
};
assert.notSameMembers = function(set1, set2, msg) {
  new Assertion(set1, msg, assert.notSameMembers, true).to.not.have.same.members(set2);
};
assert.sameDeepMembers = function(set1, set2, msg) {
  new Assertion(set1, msg, assert.sameDeepMembers, true).to.have.same.deep.members(set2);
};
assert.notSameDeepMembers = function(set1, set2, msg) {
  new Assertion(set1, msg, assert.notSameDeepMembers, true).to.not.have.same.deep.members(set2);
};
assert.sameOrderedMembers = function(set1, set2, msg) {
  new Assertion(set1, msg, assert.sameOrderedMembers, true).to.have.same.ordered.members(set2);
};
assert.notSameOrderedMembers = function(set1, set2, msg) {
  new Assertion(set1, msg, assert.notSameOrderedMembers, true).to.not.have.same.ordered.members(set2);
};
assert.sameDeepOrderedMembers = function(set1, set2, msg) {
  new Assertion(set1, msg, assert.sameDeepOrderedMembers, true).to.have.same.deep.ordered.members(set2);
};
assert.notSameDeepOrderedMembers = function(set1, set2, msg) {
  new Assertion(set1, msg, assert.notSameDeepOrderedMembers, true).to.not.have.same.deep.ordered.members(set2);
};
assert.includeMembers = function(superset, subset, msg) {
  new Assertion(superset, msg, assert.includeMembers, true).to.include.members(subset);
};
assert.notIncludeMembers = function(superset, subset, msg) {
  new Assertion(superset, msg, assert.notIncludeMembers, true).to.not.include.members(subset);
};
assert.includeDeepMembers = function(superset, subset, msg) {
  new Assertion(superset, msg, assert.includeDeepMembers, true).to.include.deep.members(subset);
};
assert.notIncludeDeepMembers = function(superset, subset, msg) {
  new Assertion(superset, msg, assert.notIncludeDeepMembers, true).to.not.include.deep.members(subset);
};
assert.includeOrderedMembers = function(superset, subset, msg) {
  new Assertion(superset, msg, assert.includeOrderedMembers, true).to.include.ordered.members(subset);
};
assert.notIncludeOrderedMembers = function(superset, subset, msg) {
  new Assertion(superset, msg, assert.notIncludeOrderedMembers, true).to.not.include.ordered.members(subset);
};
assert.includeDeepOrderedMembers = function(superset, subset, msg) {
  new Assertion(superset, msg, assert.includeDeepOrderedMembers, true).to.include.deep.ordered.members(subset);
};
assert.notIncludeDeepOrderedMembers = function(superset, subset, msg) {
  new Assertion(superset, msg, assert.notIncludeDeepOrderedMembers, true).to.not.include.deep.ordered.members(subset);
};
assert.oneOf = function(inList, list, msg) {
  new Assertion(inList, msg, assert.oneOf, true).to.be.oneOf(list);
};
assert.changes = function(fn, obj, prop, msg) {
  if (arguments.length === 3 && typeof obj === "function") {
    msg = prop;
    prop = null;
  }
  new Assertion(fn, msg, assert.changes, true).to.change(obj, prop);
};
assert.changesBy = function(fn, obj, prop, delta, msg) {
  if (arguments.length === 4 && typeof obj === "function") {
    var tmpMsg = delta;
    delta = prop;
    msg = tmpMsg;
  } else if (arguments.length === 3) {
    delta = prop;
    prop = null;
  }
  new Assertion(fn, msg, assert.changesBy, true).to.change(obj, prop).by(delta);
};
assert.doesNotChange = function(fn, obj, prop, msg) {
  if (arguments.length === 3 && typeof obj === "function") {
    msg = prop;
    prop = null;
  }
  return new Assertion(fn, msg, assert.doesNotChange, true).to.not.change(obj, prop);
};
assert.changesButNotBy = function(fn, obj, prop, delta, msg) {
  if (arguments.length === 4 && typeof obj === "function") {
    var tmpMsg = delta;
    delta = prop;
    msg = tmpMsg;
  } else if (arguments.length === 3) {
    delta = prop;
    prop = null;
  }
  new Assertion(fn, msg, assert.changesButNotBy, true).to.change(obj, prop).but.not.by(delta);
};
assert.increases = function(fn, obj, prop, msg) {
  if (arguments.length === 3 && typeof obj === "function") {
    msg = prop;
    prop = null;
  }
  return new Assertion(fn, msg, assert.increases, true).to.increase(obj, prop);
};
assert.increasesBy = function(fn, obj, prop, delta, msg) {
  if (arguments.length === 4 && typeof obj === "function") {
    var tmpMsg = delta;
    delta = prop;
    msg = tmpMsg;
  } else if (arguments.length === 3) {
    delta = prop;
    prop = null;
  }
  new Assertion(fn, msg, assert.increasesBy, true).to.increase(obj, prop).by(delta);
};
assert.doesNotIncrease = function(fn, obj, prop, msg) {
  if (arguments.length === 3 && typeof obj === "function") {
    msg = prop;
    prop = null;
  }
  return new Assertion(fn, msg, assert.doesNotIncrease, true).to.not.increase(obj, prop);
};
assert.increasesButNotBy = function(fn, obj, prop, delta, msg) {
  if (arguments.length === 4 && typeof obj === "function") {
    var tmpMsg = delta;
    delta = prop;
    msg = tmpMsg;
  } else if (arguments.length === 3) {
    delta = prop;
    prop = null;
  }
  new Assertion(fn, msg, assert.increasesButNotBy, true).to.increase(obj, prop).but.not.by(delta);
};
assert.decreases = function(fn, obj, prop, msg) {
  if (arguments.length === 3 && typeof obj === "function") {
    msg = prop;
    prop = null;
  }
  return new Assertion(fn, msg, assert.decreases, true).to.decrease(obj, prop);
};
assert.decreasesBy = function(fn, obj, prop, delta, msg) {
  if (arguments.length === 4 && typeof obj === "function") {
    var tmpMsg = delta;
    delta = prop;
    msg = tmpMsg;
  } else if (arguments.length === 3) {
    delta = prop;
    prop = null;
  }
  new Assertion(fn, msg, assert.decreasesBy, true).to.decrease(obj, prop).by(delta);
};
assert.doesNotDecrease = function(fn, obj, prop, msg) {
  if (arguments.length === 3 && typeof obj === "function") {
    msg = prop;
    prop = null;
  }
  return new Assertion(fn, msg, assert.doesNotDecrease, true).to.not.decrease(obj, prop);
};
assert.doesNotDecreaseBy = function(fn, obj, prop, delta, msg) {
  if (arguments.length === 4 && typeof obj === "function") {
    var tmpMsg = delta;
    delta = prop;
    msg = tmpMsg;
  } else if (arguments.length === 3) {
    delta = prop;
    prop = null;
  }
  return new Assertion(fn, msg, assert.doesNotDecreaseBy, true).to.not.decrease(obj, prop).by(delta);
};
assert.decreasesButNotBy = function(fn, obj, prop, delta, msg) {
  if (arguments.length === 4 && typeof obj === "function") {
    var tmpMsg = delta;
    delta = prop;
    msg = tmpMsg;
  } else if (arguments.length === 3) {
    delta = prop;
    prop = null;
  }
  new Assertion(fn, msg, assert.decreasesButNotBy, true).to.decrease(obj, prop).but.not.by(delta);
};
assert.ifError = function(val) {
  if (val) {
    throw val;
  }
};
assert.isExtensible = function(obj, msg) {
  new Assertion(obj, msg, assert.isExtensible, true).to.be.extensible;
};
assert.isNotExtensible = function(obj, msg) {
  new Assertion(obj, msg, assert.isNotExtensible, true).to.not.be.extensible;
};
assert.isSealed = function(obj, msg) {
  new Assertion(obj, msg, assert.isSealed, true).to.be.sealed;
};
assert.isNotSealed = function(obj, msg) {
  new Assertion(obj, msg, assert.isNotSealed, true).to.not.be.sealed;
};
assert.isFrozen = function(obj, msg) {
  new Assertion(obj, msg, assert.isFrozen, true).to.be.frozen;
};
assert.isNotFrozen = function(obj, msg) {
  new Assertion(obj, msg, assert.isNotFrozen, true).to.not.be.frozen;
};
assert.isEmpty = function(val, msg) {
  new Assertion(val, msg, assert.isEmpty, true).to.be.empty;
};
assert.isNotEmpty = function(val, msg) {
  new Assertion(val, msg, assert.isNotEmpty, true).to.not.be.empty;
};
(/* @__PURE__ */ __name(function alias(name, as) {
  assert[as] = assert[name];
  return alias;
}, "alias"))("isOk", "ok")("isNotOk", "notOk")("throws", "throw")("throws", "Throw")("isExtensible", "extensible")("isNotExtensible", "notExtensible")("isSealed", "sealed")("isNotSealed", "notSealed")("isFrozen", "frozen")("isNotFrozen", "notFrozen")("isEmpty", "empty")("isNotEmpty", "notEmpty")("isCallable", "isFunction")("isNotCallable", "isNotFunction");

// lib/chai.js
var used = [];
function use(fn) {
  const exports = {
    AssertionError,
    util: utils_exports,
    config,
    expect,
    assert,
    Assertion,
    ...should_exports
  };
  if (!~used.indexOf(fn)) {
    fn(exports, utils_exports);
    used.push(fn);
  }
  return exports;
}
__name(use, "use");

/*!
 * Chai - flag utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - test utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Module dependencies
 */
/*!
 * Chai - expectTypes utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - getActual utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - message composition utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - transferFlags utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * chai
 * http://chaijs.com
 * Copyright(c) 2011-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Assertion Constructor
 *
 * Creates object for chaining.
 *
 * `Assertion` objects contain metadata in the form of flags. Three flags can
 * be assigned during instantiation by passing arguments to this constructor:
 *
 * - `object`: This flag contains the target of the assertion. For example, in
 *   the assertion `expect(numKittens).to.equal(7);`, the `object` flag will
 *   contain `numKittens` so that the `equal` assertion can reference it when
 *   needed.
 *
 * - `message`: This flag contains an optional custom error message to be
 *   prepended to the error message that's generated by the assertion when it
 *   fails.
 *
 * - `ssfi`: This flag stands for "start stack function indicator". It
 *   contains a function reference that serves as the starting point for
 *   removing frames from the stack trace of the error that's created by the
 *   assertion when it fails. The goal is to provide a cleaner stack trace to
 *   end users by removing Chai's internal functions. Note that it only works
 *   in environments that support `Error.captureStackTrace`, and only when
 *   `Chai.config.includeStack` hasn't been set to `false`.
 *
 * - `lockSsfi`: This flag controls whether or not the given `ssfi` flag
 *   should retain its current value, even as assertions are chained off of
 *   this object. This is usually set to `true` when creating a new assertion
 *   from within another assertion. It's also temporarily set to `true` before
 *   an overwritten assertion gets called by the overwriting assertion.
 *
 * - `eql`: This flag contains the deepEqual function to be used by the assertion.
 *
 * @param {Mixed} obj target of the assertion
 * @param {String} msg (optional) custom error message
 * @param {Function} ssfi (optional) starting point for removing stack frames
 * @param {Boolean} lockSsfi (optional) whether or not the ssfi flag is locked
 * @api private
 */
/*!
 * ### ._obj
 *
 * Quick reference to stored `actual` value for plugin developers.
 *
 * @api private
 */
/*!
 * Chai - isProxyEnabled helper
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - addProperty utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - addLengthGuard utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - getProperties utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - proxify utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - addMethod utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - overwriteProperty utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - overwriteMethod utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - addChainingMethod utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Module variables
 */
/*!
 * Chai - overwriteChainableMethod utility
 * Copyright(c) 2012-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - compareByInspect utility
 * Copyright(c) 2011-2016 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - getOwnEnumerablePropertySymbols utility
 * Copyright(c) 2011-2016 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - getOwnEnumerableProperties utility
 * Copyright(c) 2011-2016 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Chai - isNaN utility
 * Copyright(c) 2012-2015 Sakthipriyan Vairamani <thechargingvolcano@gmail.com>
 * MIT Licensed
 */
/*!
 * chai
 * Copyright(c) 2011 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * Dependencies that are used for multiple exports are required here only once
 */
/*!
 * test utility
 */
/*!
 * type utility
 */
/*!
 * expectTypes utility
 */
/*!
 * message utility
 */
/*!
 * actual utility
 */
/*!
 * Inspect util
 */
/*!
 * Object Display util
 */
/*!
 * Flag utility
 */
/*!
 * Flag transferring utility
 */
/*!
 * Deep equal utility
 */
/*!
 * Deep path info
 */
/*!
 * Function name
 */
/*!
 * add Property
 */
/*!
 * add Method
 */
/*!
 * overwrite Property
 */
/*!
 * overwrite Method
 */
/*!
 * Add a chainable method
 */
/*!
 * Overwrite chainable method
 */
/*!
 * Compare by inspect method
 */
/*!
 * Get own enumerable property symbols method
 */
/*!
 * Get own enumerable properties method
 */
/*!
 * Checks error against a given set of criteria
 */
/*!
 * Proxify util
 */
/*!
 * addLengthGuard util
 */
/*!
 * isProxyEnabled helper
 */
/*!
 * isNaN method
 */
/*!
 * getOperator method
 */
/*!
 * chai
 * Copyright(c) 2011-2014 Jake Luer <jake@alogicalparadox.com>
 * MIT Licensed
 */
/*!
 * ### .ifError(object)
 *
 * Asserts if value is not a false value, and throws if it is a true value.
 * This is added to allow for chai to be a drop-in replacement for Node's
 * assert class.
 *
 *     var err = new Error('I am a custom error');
 *     assert.ifError(err); // Rethrows err!
 *
 * @name ifError
 * @param {Object} object
 * @namespace Assert
 * @api public
 */
/*!
 * Aliases.
 */
/*!
 * Assertion Error
 */
/*!
 * Utility Functions
 */
/*!
 * Configuration
 */
/*!
 * Primary `Assertion` prototype
 */
/*!
 * Expect interface
 */
/*!
 * Should interface
 */
/*!
 * Assert interface
 */
/*! Bundled license information:

deep-eql/index.js:
  (*!
   * deep-eql
   * Copyright(c) 2013 Jake Luer <jake@alogicalparadox.com>
   * MIT Licensed
   *)
  (*!
   * Check to see if the MemoizeMap has recorded a result of the two operands
   *
   * @param {Mixed} leftHandOperand
   * @param {Mixed} rightHandOperand
   * @param {MemoizeMap} memoizeMap
   * @returns {Boolean|null} result
  *)
  (*!
   * Set the result of the equality into the MemoizeMap
   *
   * @param {Mixed} leftHandOperand
   * @param {Mixed} rightHandOperand
   * @param {MemoizeMap} memoizeMap
   * @param {Boolean} result
  *)
  (*!
   * Primary Export
   *)
  (*!
   * The main logic of the `deepEqual` function.
   *
   * @param {Mixed} leftHandOperand
   * @param {Mixed} rightHandOperand
   * @param {Object} [options] (optional) Additional options
   * @param {Array} [options.comparator] (optional) Override default algorithm, determining custom equality.
   * @param {Array} [options.memoize] (optional) Provide a custom memoization object which will cache the results of
      complex objects for a speed boost. By passing `false` you can disable memoization, but this will cause circular
      references to blow the stack.
   * @return {Boolean} equal match
  *)
  (*!
   * Compare two Regular Expressions for equality.
   *
   * @param {RegExp} leftHandOperand
   * @param {RegExp} rightHandOperand
   * @return {Boolean} result
   *)
  (*!
   * Compare two Sets/Maps for equality. Faster than other equality functions.
   *
   * @param {Set} leftHandOperand
   * @param {Set} rightHandOperand
   * @param {Object} [options] (Optional)
   * @return {Boolean} result
   *)
  (*!
   * Simple equality for flat iterable objects such as Arrays, TypedArrays or Node.js buffers.
   *
   * @param {Iterable} leftHandOperand
   * @param {Iterable} rightHandOperand
   * @param {Object} [options] (Optional)
   * @return {Boolean} result
   *)
  (*!
   * Simple equality for generator objects such as those returned by generator functions.
   *
   * @param {Iterable} leftHandOperand
   * @param {Iterable} rightHandOperand
   * @param {Object} [options] (Optional)
   * @return {Boolean} result
   *)
  (*!
   * Determine if the given object has an @@iterator function.
   *
   * @param {Object} target
   * @return {Boolean} `true` if the object has an @@iterator function.
   *)
  (*!
   * Gets all iterator entries from the given Object. If the Object has no @@iterator function, returns an empty array.
   * This will consume the iterator - which could have side effects depending on the @@iterator implementation.
   *
   * @param {Object} target
   * @returns {Array} an array of entries from the @@iterator function
   *)
  (*!
   * Gets all entries from a Generator. This will consume the generator - which could have side effects.
   *
   * @param {Generator} target
   * @returns {Array} an array of entries from the Generator.
   *)
  (*!
   * Gets all own and inherited enumerable keys from a target.
   *
   * @param {Object} target
   * @returns {Array} an array of own and inherited enumerable keys from the target.
   *)
  (*!
   * Determines if two objects have matching values, given a set of keys. Defers to deepEqual for the equality check of
   * each key. If any value of the given key is not equal, the function will return false (early).
   *
   * @param {Mixed} leftHandOperand
   * @param {Mixed} rightHandOperand
   * @param {Array} keys An array of keys to compare the values of leftHandOperand and rightHandOperand against
   * @param {Object} [options] (Optional)
   * @return {Boolean} result
   *)
  (*!
   * Recursively check the equality of two Objects. Once basic sameness has been established it will defer to `deepEqual`
   * for each enumerable key in the object.
   *
   * @param {Mixed} leftHandOperand
   * @param {Mixed} rightHandOperand
   * @param {Object} [options] (Optional)
   * @return {Boolean} result
   *)
  (*!
   * Returns true if the argument is a primitive.
   *
   * This intentionally returns true for all objects that can be compared by reference,
   * including functions and symbols.
   *
   * @param {Mixed} value
   * @return {Boolean} result
   *)
*/

;// CONCATENATED MODULE: ./test/test-util.mjs
/*
 * Test Utilities.
 */



/*
 * Verify that a class is properly set up.
 */
function check_class(vm, name, js_class, js_superclass, js_metaclass)
{
    check_class_name(vm, js_class, name);
    check_superclass(vm, js_class, js_superclass);
    check_metaclass(vm, js_class, js_metaclass);
    check_class_linkage(vm, js_class);
};

/*
 * Verify that a class is properly named and registered in the root environment.
 */
function check_class_name(vm, js_class, name)
{
    const lisp_class = vm.lisp_class(js_class);
    const name_sym = vm.sym(name);

    // The name is an ordinary symbol in the variable namespace.
    assert.equal(lisp_class.get_name(), name_sym);

    // The symbol it's registered under in the environment is a
    // symbol in the class namespace.
    assert.equal(vm.get_environment().lookup(name_sym.to_class_symbol()),
                 lisp_class);
};

/*
 * Verify that a class is properly connected with its superclass.
 */
function check_superclass(vm, js_class, js_superclass)
{
    assert.equal(vm.lisp_class(js_class).get_superclass(),
                 vm.lisp_class(js_superclass));

    assert(js_class.prototype instanceof js_superclass);
};

/*
 * Verify that a class is properly connected with its metaclass.
 */
function check_metaclass(vm, js_class, js_metaclass)
{
    // Thanks to our nice object system, the following things
    // are both true:

    // 1) The Lisp class metaobject points to the Lisp
    // metaclass metaobject.
    assert.equal(vm.class_of(vm.lisp_class(js_class)),
                 vm.lisp_class(js_metaclass));

    // 2) The Lisp class metaobject is also an instance of the
    // JS metaclass.
    assert.instanceOf(vm.lisp_class(js_class),
                      js_metaclass);

    // (and of course of CLASS and OBJECT, too)
    assert.instanceOf(vm.lisp_class(js_class),
                      vm.Class);
    assert.instanceOf(vm.lisp_class(js_class),
                      vm.Object);
};

/*
 * Verify that a JS class is properly linked to its class metaobject.
 */
function check_class_linkage(vm, js_class)
{
    assert.equal(vm.lisp_class(js_class).get_js_class(),
                 js_class);
};

;// CONCATENATED MODULE: ./test/vm-test.mjs






const vm = (0,external_lispx_vm_umd_min_js_.make_vm)();

describe("Objects", () => {

    it("Lisp and JS objects can be distinguished.", () => {

        assert(vm.is_lisp_object(vm.str("foo")));
        assert(vm.is_lisp_object(vm.sym("foo")));
        assert(vm.is_lisp_object(vm.lisp_class(vm.Object)));
        assert(vm.is_lisp_object(vm.lisp_class(vm.Class)));

        assert.isFalse(vm.is_lisp_object(null));
        assert.isFalse(vm.is_lisp_object(undefined));
        assert.isFalse(vm.is_lisp_object(true));
        assert.isFalse(vm.is_lisp_object(false));
        assert.isFalse(vm.is_lisp_object(12));
        assert.isFalse(vm.is_lisp_object("foo"));
        assert.isFalse(vm.is_lisp_object({}));
        assert.isFalse(vm.is_lisp_object([]));
        assert.isFalse(vm.is_lisp_object(String));

    });

    it("JS objects have OBJECT as class metaobject.", () => {

        assert.equal(vm.class_of(null), vm.lisp_class(vm.Object));
        assert.equal(vm.class_of(undefined), vm.lisp_class(vm.Object));
        assert.equal(vm.class_of(true), vm.lisp_class(vm.Object));
        assert.equal(vm.class_of(false), vm.lisp_class(vm.Object));
        assert.equal(vm.class_of(12), vm.lisp_class(vm.Object));
        assert.equal(vm.class_of("foo"), vm.lisp_class(vm.Object));
        assert.equal(vm.class_of({}), vm.lisp_class(vm.Object));
        assert.equal(vm.class_of([]), vm.lisp_class(vm.Object));
        assert.equal(vm.class_of(String), vm.lisp_class(vm.Object));

    });

    it("Value equality has strict equality semantics for JS objects.", () => {

        assert(vm.equal(null, null));
        assert(vm.equal(undefined, undefined));
        assert(vm.equal("foo", "foo"));
        assert(vm.equal(12, 12));

        assert.isFalse(vm.equal(null, undefined));
        assert.isFalse(vm.equal(undefined, null));
        assert.isFalse(vm.equal({}, {}));

    });

    it("Objects of different classes or JS objects can not be compared.", () => {

        const msg = "Type assertion failed";

        assert.throws(() => vm.compare(vm.num(1), vm.str("foo")), msg);
        assert.throws(() => vm.compare(vm.str("foo"), vm.num(1)), msg);
        assert.throws(() => vm.compare([], vm.num(1)), msg);

    });

});

describe("Classes", () => {

    /*
     * These tests could be improved by using the check_class() utility
     * from test-util.mjs but so far I have been too lazy to do this.
     */

    it("Classes are named and registered in the root environment.", () => {

        function check_vm_class_name(vm, js_class, name)
        {
            /*
             * Use check from utility.
             */
            check_class_name(vm, js_class, name);

            /*
             * Add additional check that the JS constructor functions
             * of VM classes have a proper name starting with "Lisp_".
             */
            const js_name = "Lisp_" + name.replace(/-/g, "_");
            assert.equal(js_class.name, js_name);
        }

        check_vm_class_name(vm, vm.Object, "object");
        check_vm_class_name(vm, vm.String, "string");
        check_vm_class_name(vm, vm.Symbol, "symbol");
        check_vm_class_name(vm, vm.Number, "number");
        check_vm_class_name(vm, vm.Boolean, "boolean");
        check_vm_class_name(vm, vm.List, "list");
        check_vm_class_name(vm, vm.Cons, "cons");
        check_vm_class_name(vm, vm.Nil, "nil");
        check_vm_class_name(vm, vm.Void, "void");
        check_vm_class_name(vm, vm.Ignore, "ignore");
        check_vm_class_name(vm, vm.Environment, "environment");
        check_vm_class_name(vm, vm.Class, "class");
        check_vm_class_name(vm, vm.Built_in_class, "built-in-class");
        check_vm_class_name(vm, vm.Standard_class, "standard-class");
        check_vm_class_name(vm, vm.Operator, "operator");
        check_vm_class_name(vm, vm.Built_in_operator, "built-in-operator");
        check_vm_class_name(vm, vm.Fexpr, "fexpr");
        check_vm_class_name(vm, vm.Function, "function");
        check_vm_class_name(vm, vm.Continuation, "continuation");
        check_vm_class_name(vm, vm.Dynamic, "dynamic");
        check_vm_class_name(vm, vm.Input_stream, "input-stream");
        check_vm_class_name(vm, vm.String_input_stream, "string-input-stream");
        check_vm_class_name(vm, vm.Output_stream, "output-stream");
        check_vm_class_name(vm, vm.String_output_stream, "string-output-stream");
        check_vm_class_name(vm, vm.JS_console_output_stream, "js-console-output-stream");

        check_vm_class_name(vm, vm.Standard_object, "standard-object");
        check_vm_class_name(vm, vm.Condition, "condition");
        check_vm_class_name(vm, vm.Error, "error");
        check_vm_class_name(vm, vm.Type_error, "type-error");
        check_vm_class_name(vm, vm.Unbound_symbol_error, "unbound-symbol-error");
        check_vm_class_name(vm, vm.Unbound_slot_error, "unbound-slot-error");
        check_vm_class_name(vm, vm.Unbound_method_error, "unbound-method-error");
        check_vm_class_name(vm, vm.Assertion_error, "assertion-error");
        check_vm_class_name(vm, vm.Out_of_bounds_error, "out-of-bounds-error");
        check_vm_class_name(vm, vm.Match_error, "match-error");
        check_vm_class_name(vm, vm.Stream_error, "stream-error");
        check_vm_class_name(vm, vm.End_of_file, "end-of-file");
        check_vm_class_name(vm, vm.Reader_error, "reader-error");
        check_vm_class_name(vm, vm.Prompt_not_found_error, "prompt-not-found-error");

    });

    it("Classes have correct superclasses set.", () => {

        assert.isNull(vm.lisp_class(vm.Object).get_superclass());
        check_superclass(vm, vm.String, vm.Object);
        check_superclass(vm, vm.Symbol, vm.Object);
        check_superclass(vm, vm.Number, vm.Object);
        check_superclass(vm, vm.Boolean, vm.Object);
        check_superclass(vm, vm.List, vm.Object);
        check_superclass(vm, vm.Cons, vm.List);
        check_superclass(vm, vm.Nil, vm.List);
        check_superclass(vm, vm.Void, vm.Object);
        check_superclass(vm, vm.Ignore, vm.Object);
        check_superclass(vm, vm.Environment, vm.Object);
        check_superclass(vm, vm.Class, vm.Object);
        check_superclass(vm, vm.Built_in_class, vm.Class);
        check_superclass(vm, vm.Standard_class, vm.Class);
        check_superclass(vm, vm.Operator, vm.Object);
        check_superclass(vm, vm.Built_in_operator, vm.Operator);
        check_superclass(vm, vm.Fexpr, vm.Operator);
        check_superclass(vm, vm.Function, vm.Operator);
        check_superclass(vm, vm.Continuation, vm.Object);
        check_superclass(vm, vm.Input_stream, vm.Object);
        check_superclass(vm, vm.String_input_stream, vm.Input_stream);
        check_superclass(vm, vm.Output_stream, vm.Object);
        check_superclass(vm, vm.String_output_stream, vm.Output_stream);
        check_superclass(vm, vm.JS_console_output_stream, vm.Output_stream);

        check_superclass(vm, vm.Standard_object, vm.Object);
        check_superclass(vm, vm.Dynamic, vm.Standard_object);
        check_superclass(vm, vm.Condition, vm.Standard_object);
        check_superclass(vm, vm.Error, vm.Condition);
        check_superclass(vm, vm.Type_error, vm.Error);
        check_superclass(vm, vm.Unbound_symbol_error, vm.Error);
        check_superclass(vm, vm.Unbound_slot_error, vm.Error);
        check_superclass(vm, vm.Unbound_method_error, vm.Error);
        check_superclass(vm, vm.Assertion_error, vm.Error);
        check_superclass(vm, vm.Out_of_bounds_error, vm.Error);
        check_superclass(vm, vm.Match_error, vm.Error);
        check_superclass(vm, vm.Stream_error, vm.Error);
        check_superclass(vm, vm.End_of_file, vm.Stream_error);
        check_superclass(vm, vm.Reader_error, vm.Error);
        check_superclass(vm, vm.Prompt_not_found_error, vm.Error);

    });

    it("Classes have correct metaclasses set.", () => {

        check_metaclass(vm, vm.Object, vm.Built_in_class);
        check_metaclass(vm, vm.String, vm.Built_in_class);
        check_metaclass(vm, vm.Symbol, vm.Built_in_class);
        check_metaclass(vm, vm.Number, vm.Built_in_class);
        check_metaclass(vm, vm.Boolean, vm.Built_in_class);
        check_metaclass(vm, vm.List, vm.Built_in_class);
        check_metaclass(vm, vm.Cons, vm.Built_in_class);
        check_metaclass(vm, vm.Nil, vm.Built_in_class);
        check_metaclass(vm, vm.Void, vm.Built_in_class);
        check_metaclass(vm, vm.Ignore, vm.Built_in_class);
        check_metaclass(vm, vm.Environment, vm.Built_in_class);
        check_metaclass(vm, vm.Class, vm.Built_in_class);
        check_metaclass(vm, vm.Built_in_class, vm.Built_in_class);
        check_metaclass(vm, vm.Standard_class, vm.Built_in_class);
        check_metaclass(vm, vm.Operator, vm.Built_in_class);
        check_metaclass(vm, vm.Built_in_operator, vm.Built_in_class);
        check_metaclass(vm, vm.Fexpr, vm.Built_in_class);
        check_metaclass(vm, vm.Function, vm.Built_in_class);
        check_metaclass(vm, vm.Continuation, vm.Built_in_class);
        check_metaclass(vm, vm.Input_stream, vm.Built_in_class);
        check_metaclass(vm, vm.String_input_stream, vm.Built_in_class);
        check_metaclass(vm, vm.Output_stream, vm.Built_in_class);
        check_metaclass(vm, vm.String_output_stream, vm.Built_in_class);
        check_metaclass(vm, vm.JS_console_output_stream, vm.Built_in_class);

        check_metaclass(vm, vm.Standard_object, vm.Standard_class);
        check_metaclass(vm, vm.Dynamic, vm.Standard_class);
        check_metaclass(vm, vm.Condition, vm.Standard_class);
        check_metaclass(vm, vm.Error, vm.Standard_class);
        check_metaclass(vm, vm.Type_error, vm.Standard_class);
        check_metaclass(vm, vm.Unbound_symbol_error, vm.Standard_class);
        check_metaclass(vm, vm.Unbound_slot_error, vm.Standard_class);
        check_metaclass(vm, vm.Unbound_method_error, vm.Standard_class);
        check_metaclass(vm, vm.Assertion_error, vm.Standard_class);
        check_metaclass(vm, vm.Out_of_bounds_error, vm.Standard_class);
        check_metaclass(vm, vm.Match_error, vm.Standard_class);
        check_metaclass(vm, vm.Stream_error, vm.Standard_class);
        check_metaclass(vm, vm.End_of_file, vm.Standard_class);
        check_metaclass(vm, vm.Reader_error, vm.Standard_class);
        check_metaclass(vm, vm.Prompt_not_found_error, vm.Standard_class);

    });

    it("JS classes are linked to their class metaobjects and vice versa.", () => {

        check_class_linkage(vm, vm.Object);
        check_class_linkage(vm, vm.String);
        check_class_linkage(vm, vm.Symbol);
        check_class_linkage(vm, vm.Number);
        check_class_linkage(vm, vm.Boolean);
        check_class_linkage(vm, vm.List);
        check_class_linkage(vm, vm.Cons);
        check_class_linkage(vm, vm.Nil);
        check_class_linkage(vm, vm.Void);
        check_class_linkage(vm, vm.Ignore);
        check_class_linkage(vm, vm.Environment);
        check_class_linkage(vm, vm.Class);
        check_class_linkage(vm, vm.Built_in_class);
        check_class_linkage(vm, vm.Standard_class);
        check_class_linkage(vm, vm.Operator);
        check_class_linkage(vm, vm.Built_in_operator);
        check_class_linkage(vm, vm.Fexpr);
        check_class_linkage(vm, vm.Function);
        check_class_linkage(vm, vm.Continuation);
        check_class_linkage(vm, vm.Dynamic);
        check_class_linkage(vm, vm.Input_stream);
        check_class_linkage(vm, vm.String_input_stream);
        check_class_linkage(vm, vm.Output_stream);
        check_class_linkage(vm, vm.String_output_stream);
        check_class_linkage(vm, vm.JS_console_output_stream);

        check_class_linkage(vm, vm.Standard_object);
        check_class_linkage(vm, vm.Condition);
        check_class_linkage(vm, vm.Error);
        check_class_linkage(vm, vm.Type_error);
        check_class_linkage(vm, vm.Unbound_symbol_error);
        check_class_linkage(vm, vm.Unbound_slot_error);
        check_class_linkage(vm, vm.Assertion_error);
        check_class_linkage(vm, vm.Match_error);
        check_class_linkage(vm, vm.Stream_error);
        check_class_linkage(vm, vm.End_of_file);
        check_class_linkage(vm, vm.Reader_error);

    });

    it("Test is_subclass().", () => {

        function is_subclass(sub, sup)
        {
            return vm.is_subclass(vm.lisp_class(sub), vm.lisp_class(sup));
        }

        assert(is_subclass(vm.Object, vm.Object));

        assert(is_subclass(vm.Class, vm.Object));
        assert.isFalse(is_subclass(vm.Object, vm.Class));

        assert(is_subclass(vm.Standard_class, vm.Object));
        assert(is_subclass(vm.Standard_class, vm.Class));

    });

    it("Classes can be created dynamically.", () => {

        const point2d_class = vm.make_standard_class(vm.sym("point-2d"),
                                                     vm.lisp_class(vm.Standard_object));
        assert.instanceOf(point2d_class, vm.Standard_class);
        assert.instanceOf(point2d_class, vm.Class);
        assert.instanceOf(point2d_class, vm.Object);
        assert(vm.equal(vm.class_of(point2d_class), vm.lisp_class(vm.Standard_class)));
        assert.typeOf(point2d_class.get_js_class(), "function");
        assert(vm.is_subclass(point2d_class, vm.lisp_class(vm.Standard_object)));
        assert(vm.is_subclass(point2d_class, vm.lisp_class(vm.Object)));

        const point2d = vm.make_instance(point2d_class, vm.sym("x"), 1, vm.sym("y"), 2);
        assert.instanceOf(point2d, point2d_class.get_js_class());
        assert.instanceOf(point2d, vm.Standard_object);
        assert.instanceOf(point2d, vm.Object);
        assert(vm.equal(vm.class_of(point2d), point2d_class));
        assert(vm.equal(point2d.slot_value(vm.sym("x")), 1));
        assert(vm.equal(point2d.slot_value(vm.sym("y")), 2));

        const point3d_class = vm.make_standard_class(vm.sym("point-3d"),
                                                     point2d_class);
        assert.instanceOf(point3d_class, vm.Standard_class);
        assert.instanceOf(point3d_class, vm.Class);
        assert.instanceOf(point3d_class, vm.Object);
        assert(vm.equal(vm.class_of(point3d_class), vm.lisp_class(vm.Standard_class)));
        assert.typeOf(point3d_class.get_js_class(), "function");
        assert(vm.is_subclass(point3d_class, point2d_class));
        assert(vm.is_subclass(point3d_class, vm.lisp_class(vm.Standard_object)));
        assert(vm.is_subclass(point3d_class, vm.lisp_class(vm.Object)));

        const point3d = vm.make_instance(point3d_class);
        assert.instanceOf(point3d, point3d_class.get_js_class());
        assert.instanceOf(point3d, point2d_class.get_js_class());
        assert.instanceOf(point3d, vm.Standard_object);
        assert.instanceOf(point3d, vm.Object);
        assert(vm.equal(vm.class_of(point3d), point3d_class));

        assert.equal(point2d_class.get_js_class().name, "point-2d");
        assert.equal(point3d_class.get_js_class().name, "point-3d");
        assert.equal(point2d.constructor.name, "point-2d");
        assert.equal(point3d.constructor.name, "point-3d");

    });

    it("has_lisp_class() can be used to distinguish Lisp classes from other classes.", () => {

        assert(vm.has_lisp_class(vm.Object));
        assert(vm.has_lisp_class(vm.Class));

        assert(!vm.has_lisp_class(Object));
        assert(!vm.has_lisp_class(String));

    });

});

describe("Strings", () => {

    it("Lisp strings can be created from JS strings.", () => {

        const js_euro = "\u{20AC}";
        const lisp_euro = vm.str(js_euro);
        assert.instanceOf(lisp_euro, vm.String);
        assert.instanceOf(lisp_euro, vm.Object);
        assert.equal(vm.class_of(lisp_euro), vm.lisp_class(vm.String));
        assert.equal(lisp_euro.get_utf8_bytes(), "\u{E2}\u{82}\u{AC}");
        assert.equal(lisp_euro.to_js_string(), js_euro);

    });

    it("Lisp strings have value equality.", () => {

        assert(vm.equal(vm.str("foo"), vm.str("foo")));
        assert.isFalse(vm.equal(vm.str("foo"), vm.str("bar")));
        assert.isFalse(vm.equal(vm.str("foo"), vm.sym("foo")));

    });

});

describe("Symbols", () => {

    it("Symbols can be created from strings.", () => {

        const sym = vm.sym("henlo world");
        assert.instanceOf(sym, vm.Symbol);
        assert.instanceOf(sym, vm.Object);
        assert.equal(vm.class_of(sym), vm.lisp_class(vm.Symbol));
        assert(vm.equal(sym.get_string(), vm.str("henlo world")));

    });

    it("Symbols are interned.", () => {

        assert(vm.sym("foo") === vm.sym("foo"));
        assert(vm.sym("foo") !== vm.sym("bar"));

        // Can also use intern() on strings.
        assert(vm.sym("foo") === vm.intern(vm.str("foo")));

    });

    it("Equality works for symbols.", () => {

        assert(vm.equal(vm.sym("foo"), vm.sym("foo")));
        assert.isFalse(vm.equal(vm.sym("foo"), vm.sym("bar")));

    });

    it("Symbols have string names.", () => {

        assert(vm.equal(vm.sym("foo").get_string(), vm.str("foo")));

    });

    it("Symbols have namespaces.", () => {

        assert(vm.equal(vm.sym("foo").get_namespace(),
                        vm.VARIABLE_NAMESPACE));
        assert(vm.equal(vm.sym("foo").to_variable_symbol().get_namespace(),
                        vm.VARIABLE_NAMESPACE));
        assert(vm.equal(vm.sym("foo").to_function_symbol().get_namespace(),
                        vm.FUNCTION_NAMESPACE));
        assert(vm.equal(vm.sym("foo").to_class_symbol().get_namespace(),
                        vm.CLASS_NAMESPACE));
        assert(vm.equal(vm.sym("foo").to_keyword_symbol().get_namespace(),
                        vm.KEYWORD_NAMESPACE));

    });

    it("Symbols in different namespaces are distinct.", () => {

        assert(vm.sym("foo") !== vm.sym("foo").to_function_symbol());
        assert(vm.sym("foo") !== vm.sym("foo").to_class_symbol());
        assert(vm.sym("foo") !== vm.sym("foo").to_keyword_symbol());

    });

    it("Symbols in the same namespace are the same.", () => {

        assert(vm.sym("foo").to_variable_symbol() === vm.sym("foo").to_variable_symbol());
        assert(vm.sym("foo").to_function_symbol() === vm.sym("foo").to_function_symbol());
        assert(vm.sym("foo").to_class_symbol() === vm.sym("foo").to_class_symbol());
        assert(vm.sym("foo").to_keyword_symbol() === vm.sym("foo").to_keyword_symbol());

    });

    it("The namespace of a symbol is a string.", () => {

        assert.equal(vm.sym("foo").get_namespace(), "variable");
        assert.equal(vm.sym("foo").to_variable_symbol().get_namespace(), "variable");
        assert.equal(vm.sym("foo").to_function_symbol().get_namespace(), "function");
        assert.equal(vm.sym("foo").to_class_symbol().get_namespace(), "class");
        assert.equal(vm.sym("foo").to_keyword_symbol().get_namespace(), "keyword");

    });

    it("Namespaces can be passed to intern().", () => {

        assert.equal(vm.sym("foo"), vm.intern(vm.str("foo")));

        for (const ns of [vm.VARIABLE_NAMESPACE,
                          vm.FUNCTION_NAMESPACE,
                          vm.CLASS_NAMESPACE,
                          vm.KEYWORD_NAMESPACE]) {

            assert.equal(vm.sym("foo", ns), vm.intern(vm.str("foo"), ns));
        }

    });

    it("fsym() creates function symbols.", () => {

        assert.equal(vm.fsym("foo"), vm.sym("foo").to_function_symbol());

    });

    it("csym() creates class symbols.", () => {

        assert.equal(vm.csym("foo"), vm.sym("foo").to_class_symbol());

    });

    it("kwd() creates keyword symbols.", () => {

        assert.equal(vm.kwd("foo"), vm.sym("foo").to_keyword_symbol());

    });

});

describe("Numbers", () => {

    it("Zero and one are defined.", () => {

        assert(vm.equal(vm.zero(), vm.num(0)));
        assert(vm.equal(vm.one(), vm.num(1)));

    });

    it("Lisp numbers can be created from JS strings and numbers.", () => {

        const n1 = vm.num(1);
        const n2 = vm.num("1");
        const n3 = vm.num("21092183098213098210938092183092131221420943032.3292103283");
        const n4 = vm.num("21092183098213098210938092183092131221420943032.3292103283");
        assert(vm.equal(n1, n2));
        assert(vm.equal(n3, n4));
        assert.isFalse(vm.equal(n1, n3));
        assert.isFalse(vm.equal(n1, n4));

        assert.instanceOf(n1, vm.Number);
        assert.instanceOf(n1, vm.Object);
        assert.equal(vm.class_of(n1), vm.lisp_class(vm.Number));

    });

    it("Lisp numbers can be turned into strings.", () => {

        const s1 = "-21092183098213098210938092183092131221420943032.329210328332921032833292";
        const s2 = "21092183098213098210938092183092131221420943032.329210328332921032833292";
        const n1 = vm.num(s1);
        const n2 = vm.num(s2);

        assert(vm.equal(vm.str(s1), n1.to_string()));
        assert(vm.equal(vm.str(s2), n2.to_string()));

    });

    it("Lisp numbers can be transformed to JS numbers.", () => {

        assert(vm.equal(vm.num(-1).to_js_number(), -1));
        assert(vm.equal(vm.num(0).to_js_number(), 0));
        assert(vm.equal(vm.num(1).to_js_number(), 1));

        assert(vm.equal(vm.num(-1.123).to_js_number(), -1.123));
        assert(vm.equal(vm.num(1.123).to_js_number(), 1.123));

        assert(vm.equal(vm.num(Number.MIN_SAFE_INTEGER).to_js_number(),
                        Number.MIN_SAFE_INTEGER));
        assert(vm.equal(vm.num(Number.MAX_SAFE_INTEGER).to_js_number(),
                        Number.MAX_SAFE_INTEGER));

        assert(vm.equal(vm.num("-10000000000000000000000000000000000000").to_js_number(),
                        -1e+37));
        assert(vm.equal(vm.num("10000000000000000000000000000000000000").to_js_number(),
                        1e+37));

    });

    it("Numbers can be compared.", () => {

        assert(vm.compare(vm.num(1), vm.num(1)) === 0);
        assert(vm.compare(vm.num(1), vm.num(2)) === -1);
        assert(vm.compare(vm.num(2), vm.num(1)) === 1);

    });

    it("Numbers can be added.", () => {

        assert(vm.equal(vm.add(vm.num(10), vm.num(20)), vm.num(30)));

    });

    it("Numbers can be subtracted.", () => {

        assert(vm.equal(vm.subtract(vm.num(10), vm.num(20)), vm.num(-10)));

    });

    it("Numbers can be multiplied.", () => {

        assert(vm.equal(vm.multiply(vm.num(10), vm.num(20)), vm.num(200)));

    });

    it("Numbers can be divided.", () => {

        assert(vm.equal(vm.divide(vm.num(10), vm.num(20)), vm.num(0.5)));

    });

});

describe("Booleans", () => {

    it("The booleans, er, exist.", () => {

        assert(vm.equal(vm.t(), vm.t()));
        assert(vm.equal(vm.f(), vm.f()));
        assert.isFalse(vm.equal(vm.t(), vm.f()));

        assert.instanceOf(vm.t(), vm.Boolean);
        assert.instanceOf(vm.t(), vm.Object);
        assert.instanceOf(vm.f(), vm.Boolean);
        assert.instanceOf(vm.f(), vm.Object);

        assert.equal(vm.class_of(vm.t()), vm.lisp_class(vm.Boolean));
        assert.equal(vm.class_of(vm.f()), vm.lisp_class(vm.Boolean));

        assert(vm.t().to_js_boolean());
        assert.isFalse(vm.f().to_js_boolean());

    });

});

describe("Lists", () => {

    it("cons() creates conses.", () => {

        const c1 = vm.cons(vm.num(1), vm.num(2));

        assert.instanceOf(c1, vm.Cons);
        assert.instanceOf(c1, vm.List);
        assert.instanceOf(c1, vm.Object);
        assert.equal(vm.class_of(c1), vm.lisp_class(vm.Cons));

        assert(vm.equal(c1.car(), vm.num(1)));
        assert(vm.equal(c1.cdr(), vm.num(2)));

    });

    it("Conses have value equality.", () => {

        const c1 = vm.cons(vm.num(1), vm.num(2));
        const c2 = vm.cons(vm.num(1), vm.num(2));
        const c3 = vm.cons(vm.num(100), vm.num(200));

        assert(vm.equal(c1, c2));
        assert.isFalse(vm.equal(c1, c3));

    });

    it("car and cdr are mutable.", () => {

        const c1 = vm.cons(vm.num(1), vm.num(2));
        c1.set_car(vm.str("foo"));
        c1.set_cdr(vm.str("bar"));
        assert(vm.equal(c1, vm.cons(vm.str("foo"), vm.str("bar"))));

    });

    it("Test elt().", () => {

        assert.throws(() => vm.elt(vm.nil(), 0), "Type assertion failed");
        assert.throws(() => vm.elt(vm.nil(), 1), "Type assertion failed");

        assert.equal(vm.elt(vm.list(1, 2, 3), 0), 1);
        assert.equal(vm.elt(vm.list(1, 2, 3), 1), 2);
        assert.equal(vm.elt(vm.list(1, 2, 3), 2), 3);

        assert.throws(() => vm.elt(vm.list(1, 2, 3), 3), "Type assertion failed");
        assert.throws(() => vm.elt(vm.list(1, 2, 3), 4), "Type assertion failed");

    });

    it("Test array_to_list() and list_to_array().", () => {

        let examples = [
            [[],
             vm.nil()],
            [[vm.num(1)],
             vm.cons(vm.num(1), vm.nil())],
            [[vm.num(1), vm.num(2)],
             vm.cons(vm.num(1), vm.cons(vm.num(2), vm.nil()))]
        ];

        for (const [array, list] of examples) {
            assert(vm.equal(vm.array_to_list(array), list));
            // Need deepEqual for arrays.
            assert.deepEqual(vm.list_to_array(list), array);
        }

    });

    it("Test list().", () => {

        assert(vm.equal(vm.list(),
                        vm.nil()));
        assert(vm.equal(vm.list(vm.num(1)),
                        vm.array_to_list([vm.num(1)])));
        assert(vm.equal(vm.list(vm.num(1), vm.num(2)),
                        vm.array_to_list([vm.num(1), vm.num(2)])));

    });

    it("Test reverse().", () => {

        assert(vm.equal(vm.reverse(vm.nil()), vm.nil()));
        assert(vm.equal(vm.reverse(vm.list(1, 2, 3)), vm.list(3, 2, 1)));

    });

});

describe("#NIL", () => {

    it("Exists.", () => {

        assert.instanceOf(vm.nil(), vm.Nil);
        assert.instanceOf(vm.nil(), vm.List);
        assert.instanceOf(vm.nil(), vm.Object);
        assert.equal(vm.class_of(vm.nil()), vm.lisp_class(vm.Nil));

    });

});

describe("#VOID", () => {

    it("Exists.", () => {

        assert.instanceOf(vm.void(), vm.Void);
        assert.instanceOf(vm.void(), vm.Object);
        assert.equal(vm.class_of(vm.void()), vm.lisp_class(vm.Void));

    });

});

describe("#IGNORE", () => {

    it("Exists.", () => {

        assert.instanceOf(vm.ignore(), vm.Ignore);
        assert.instanceOf(vm.ignore(), vm.Object);
        assert.equal(vm.class_of(vm.ignore()), vm.lisp_class(vm.Ignore));

    });

});

describe("Environments", () => {

    it("Unbound variables lose.", () => {

        const e1 = vm.make_environment();
        const e2 = vm.make_environment(e1);
        const msg = "Unbound variable";
        assert.throws(() => e1.lookup(vm.sym("x")), msg);
        assert.throws(() => e2.lookup(vm.sym("x")), msg);

        assert.isFalse(e1.is_bound(vm.sym("x")));
        assert.isFalse(e2.is_bound(vm.sym("x")));

    });

    it("JS undefined can be used as a value.", () => {

        const e1 = vm.make_environment();
        const e2 = vm.make_environment(e1);

        e1.put(vm.sym("x"), undefined);
        assert.equal(e1.lookup(vm.sym("x")), undefined);
        assert.equal(e2.lookup(vm.sym("x")), undefined);

        assert(e1.is_bound(vm.sym("x")));
        assert(e2.is_bound(vm.sym("x")));

    });

    it("Values are inherited from the parent environment.", () => {

        const e1 = vm.make_environment();
        const e2 = vm.make_environment(e1);

        e1.put(vm.sym("x"), 12);
        e1.put(vm.sym("y"), 14);

        assert.equal(e1.lookup(vm.sym("x")), 12);
        assert.equal(e1.lookup(vm.sym("y")), 14);
        assert.equal(e2.lookup(vm.sym("x")), 12);
        assert.equal(e2.lookup(vm.sym("y")), 14);

        e2.put(vm.sym("x"), 42);

        assert.equal(e1.lookup(vm.sym("x")), 12);
        assert.equal(e1.lookup(vm.sym("y")), 14);
        assert.equal(e2.lookup(vm.sym("x")), 42);
        assert.equal(e2.lookup(vm.sym("y")), 14);

        assert(e1.is_bound(vm.sym("x")));
        assert(e1.is_bound(vm.sym("y")));
        assert(e2.is_bound(vm.sym("x")));
        assert(e2.is_bound(vm.sym("y")));

    });

    it("Values can be mutated.", () => {

        const e1 = vm.make_environment();
        const e2 = vm.make_environment(e1);

        e1.put(vm.sym("x"), 12);

        assert.equal(e1.lookup(vm.sym("x")), 12);
        assert.equal(e2.lookup(vm.sym("x")), 12);
        assert(e1.is_bound(vm.sym("x")));
        assert(e2.is_bound(vm.sym("x")));

        e1.put(vm.sym("x"), 42);

        assert.equal(e1.lookup(vm.sym("x")), 42);
        assert.equal(e2.lookup(vm.sym("x")), 42);
        assert(e1.is_bound(vm.sym("x")));
        assert(e2.is_bound(vm.sym("x")));

        e2.put(vm.sym("x"), 64);

        assert.equal(e1.lookup(vm.sym("x")), 42);
        assert.equal(e2.lookup(vm.sym("x")), 64);
        assert(e1.is_bound(vm.sym("x")));
        assert(e2.is_bound(vm.sym("x")));

    });

    it("An environment can contain same-named symbols with different namespaces.", () => {

        const e1 = vm.make_environment();

        const v = vm.sym("x");
        const f = vm.fsym("x");
        const c = vm.csym("x");

        e1.put(v, 1);
        e1.put(f, 2);
        e1.put(c, 3);

        assert.equal(e1.lookup(v), 1);
        assert.equal(e1.lookup(f), 2);
        assert.equal(e1.lookup(c), 3);

        assert(e1.is_bound(vm.sym("x")));
        assert(e1.is_bound(vm.fsym("x")));
        assert(e1.is_bound(vm.csym("x")));

    });

});

describe("Standard objects", () => {

    it("make_instance() creates new standard objects.", () => {

        const obj = vm.make_instance(vm.lisp_class(vm.Standard_object));
        vm.assert(obj instanceof vm.Standard_object);
        vm.assert(obj instanceof vm.Object);
        vm.assert(vm.equal(vm.class_of(obj), vm.lisp_class(vm.Standard_object)));

    });

    it("Slots can be created by make_instance() and accessed with slot_value().", () => {

        const obj = vm.make_instance(vm.lisp_class(vm.Standard_object),
                                     vm.sym("x"), vm.num(12),
                                     vm.kwd("y"), vm.num(24));

        assert(vm.equal(obj.slot_value(vm.sym("x")),
                        vm.num(12)));
        assert(vm.equal(obj.slot_value(vm.sym("y")),
                        vm.num(24)));

        // Can also use keyword symbols.
        assert(vm.equal(obj.slot_value(vm.kwd("x")),
                        vm.num(12)));
        assert(vm.equal(obj.slot_value(vm.kwd("y")),
                        vm.num(24)));

    });

    it("Slots can be updated with set_slot_value().", () => {

        const obj = vm.make_instance(vm.lisp_class(vm.Standard_object));
        obj.set_slot_value(vm.sym("foo"), vm.num(1));
        assert(vm.equal(obj.slot_value(vm.sym("foo")),
                        vm.num(1)));
        obj.set_slot_value(vm.sym("foo"), vm.num(2));
        assert(vm.equal(obj.slot_value(vm.sym("foo")),
                        vm.num(2)));

    });

    it("is_slot_bound() checks if slots are bound.", () => {

        const obj = vm.make_instance(vm.lisp_class(vm.Standard_object));
        assert.isFalse(obj.is_slot_bound(vm.sym("foo")));
        assert.isFalse(obj.is_slot_bound(vm.sym("bar")));

        obj.set_slot_value(vm.sym("foo"), vm.num(1));
        assert(obj.is_slot_bound(vm.sym("foo")));
        assert.isFalse(obj.is_slot_bound(vm.sym("bar")));

    });

});

describe("assert()", () => {

    it("It does nothing if the boolean is true.", () => {
        assert.equal(vm.assert(true), undefined);
    });

    it("It throws an exception if the boolean is false.", () => {
        assert.throws(() => vm.assert(false), "Assertion failed");
    });

    it("Exceptions are instances of vm.Assertion_error.", () => {
        try {
            vm.assert(false);
        } catch(e) {
            assert.instanceOf(e, vm.Assertion_error);
            return;
        }
        assert(false);
    });

    it("It supports a message.", () => {
        try {
            vm.assert(false, "Message");
        } catch(e) {
            assert.equal(e.message, "Message");
            assert.instanceOf(e, vm.Assertion_error);
            return;
        }
        assert(false);
    });

    it("The message defaults to 'Assertion failed'.", () => {
        try {
            vm.assert(false);
        } catch(e) {
            assert.equal(e.message, "Assertion failed");
            assert.instanceOf(e, vm.Assertion_error);
            return;
        }
        assert(false);
    });

});

describe("abstract_method()", () => {

    it("It throws an error.", () => {
        assert.throws(() => vm.abstract_method(), "Congratulations");
    });

});

/***** Types *****/

describe("has_type()", () => {

    it("It supports string type specs.", () => {
        assert(vm.has_type("foo", "string"));
        assert(vm.has_type(12, "number"));
        assert(vm.has_type({}, "object"));

        assert.isFalse(vm.has_type(12, "string"));
        assert.isFalse(vm.has_type("foo", "object"));
        assert.isFalse(vm.has_type({}, "number"));
    });

    it("It supports function type specs.", () => {
        assert(vm.has_type(vm.str("12"), vm.Object));
        assert(vm.has_type(vm.str("12"), vm.String));
        assert(vm.has_type(vm.num(12), vm.Number));
        assert(vm.has_type(vm.num(12), vm.Object));

        assert.isFalse(vm.has_type(vm.str("foo"), vm.Number));
        assert.isFalse(vm.has_type(vm.num(12), vm.String));
    });

    it("It supports 'any' type specs.", () => {
        assert(vm.has_type(null, vm.TYPE_ANY));
        assert(vm.has_type(12, vm.TYPE_ANY));
        assert(vm.has_type("foo", vm.TYPE_ANY));
    });

    it("It supports 'null' type specs.", () => {
        assert(vm.has_type(null, vm.TYPE_NULL));

        assert.isFalse(vm.has_type(12, vm.TYPE_NULL));
        assert.isFalse(vm.has_type("foo", vm.TYPE_NULL));
    });

    it("It supports n-ary 'or' type specs.", () => {
        assert.isFalse(vm.has_type(null, vm.type_or()));

        assert(vm.has_type(null, vm.type_or(vm.TYPE_NULL)));

        assert(vm.has_type(null, vm.type_or(vm.TYPE_NULL, "string")));
        assert(vm.has_type(null, vm.type_or("string", vm.TYPE_NULL)));

        assert(vm.has_type(null, vm.type_or(vm.TYPE_NULL, "string", "number")));
        assert(vm.has_type(null, vm.type_or("string", vm.TYPE_NULL, "number")));
        assert(vm.has_type(null, vm.type_or("string", "number", vm.TYPE_NULL)));

        assert(vm.has_type(12, vm.type_or(vm.TYPE_NULL, "string", "number")));
        assert(vm.has_type(12, vm.type_or("string", vm.TYPE_NULL, "number")));
        assert(vm.has_type(12, vm.type_or("string", "number", vm.TYPE_NULL)));

        assert.isFalse(vm.has_type(true, vm.type_or(vm.TYPE_NULL, "string", "number")));
        assert.isFalse(vm.has_type(true, vm.type_or("string", vm.TYPE_NULL, "number")));
        assert.isFalse(vm.has_type(true, vm.type_or("string", "number", vm.TYPE_NULL)));

        assert.isFalse(vm.has_type(12, vm.type_or(vm.TYPE_NULL, "string")));
        assert.isFalse(vm.has_type(12, vm.type_or("string", vm.TYPE_NULL)));

        assert(vm.has_type(12, vm.type_or("number", "string")));
        assert(vm.has_type(12, vm.type_or("string", "number")));

        assert(vm.has_type("foo", vm.type_or("number", "string")));
        assert(vm.has_type("foo", vm.type_or("string", "number")));
    });

    it("It throws if the type spec is illegal.", () => {
        var msg = "Unknown type spec";
        assert.throws(() => vm.has_type(1), msg);
        assert.throws(() => vm.has_type(1, 1), msg);
        assert.throws(() => vm.has_type(1, null), msg);
        assert.throws(() => vm.has_type(1, { foo: 12 }), msg);
    });

    it("Type specs can be converted to symbolic Lisp data.", () => {
        assert(vm.equal(vm.to_lisp_type_spec(vm.TYPE_ANY),
                        vm.sym("object")));
        assert(vm.equal(vm.to_lisp_type_spec(vm.TYPE_NULL),
                        vm.str("null")));
        assert(vm.equal(vm.to_lisp_type_spec("function"),
                        vm.str("function")));
        assert(vm.equal(vm.to_lisp_type_spec(vm.type_or()),
                        vm.list(vm.sym("or"))));
        assert(vm.equal(vm.to_lisp_type_spec(vm.type_or(vm.TYPE_NULL, vm.String)),
                        vm.list(vm.sym("or"), vm.str("null"), vm.sym("string"))));
    });

});

describe("assert_type()", () => {

    it("It supports type specs and returns the datum.", () => {
        assert.equal(vm.assert_type(vm.t(), vm.Boolean), vm.t());
        assert.equal(vm.assert_type(vm.sym("foo"), vm.Symbol), vm.sym("foo"));
        assert.equal(vm.assert_type("foo", "string"), "foo");
        assert.equal(vm.assert_type(12, "number"), 12);
        assert.deepEqual(vm.assert_type({}, "object"), {});
    });

    it("It throws if the datum doesn't match the type spec.", () => {
        try {
            vm.assert_type(12, "string");
        } catch(e) {
            assert.equal(e.message, "Type assertion failed: expected \"string\" got #<<js number 12>>");
            assert.equal(e.lisp_slot_datum, 12);
            assert(vm.equal(e["lisp_slot_expected-type"], vm.str("string")));
            return;
        }
        assert(false);
    });

    it("Error messages contain the type spec.", () => {
        assert.throws(() => vm.assert_type(vm.nil(), vm.Cons),
                      "Type assertion failed: expected cons got ()");
    });

    it("It supports ordinary, non-Lisp JS classes.", () => {
        assert(vm.assert_type(new String("foo"), String));
        assert.throws(() => vm.assert_type(new String("foo"), Number));
    });

});

/***** UTF-8 *****/

describe("utf8_encode()", () => {

    it("It transforms UTF-16 to UTF-8.", () => {
        // The Unicode code point for the Euro sign, 0x20AC, becomes
        // three bytes in UTF-8:
        // https://en.wikipedia.org/wiki/UTF-8#Examples
        const euro_utf16 = "\u{20AC}";
        assert.equal(euro_utf16.length, 1);
        assert.equal(euro_utf16.charCodeAt(0), 0x20AC);
        const euro_utf8 = vm.utf8_encode(euro_utf16);
        assert.equal(euro_utf8.length, 3);
        assert.equal(euro_utf8.charCodeAt(0), 0xE2);
        assert.equal(euro_utf8.charCodeAt(1), 0x82);
        assert.equal(euro_utf8.charCodeAt(2), 0xAC);
    });

});

describe("utf8_decode()", () => {

    it("It transforms UTF-8 to UTF-16.", () => {
        const euro_utf8 = "\u{E2}\u{82}\u{AC}";
        assert.equal(euro_utf8.length, 3);
        assert.equal(euro_utf8.charCodeAt(0), 0xE2);
        assert.equal(euro_utf8.charCodeAt(1), 0x82);
        assert.equal(euro_utf8.charCodeAt(2), 0xAC);
        const euro_utf16 = vm.utf8_decode(euro_utf8);
        assert.equal(euro_utf16.length, 1);
        assert.equal(euro_utf16.charCodeAt(0), 0x20AC);
    });

});

;// CONCATENATED MODULE: ./test/eval-test.mjs













const eval_test_vm = time("Boot LispX", () => (0,external_lispx_vm_umd_min_js_.make_vm)());

/*
 * This stream is used to prevent stack traces being printed for some
 * tests that cause panics (via INVOKE-DEBUGGER).
 */
const MUFFLED_STREAM = new eval_test_vm.JS_console_output_stream(() => null);

/*
 * Utilities.
 */

function time(name, fun)
{
    const start = new Date().getTime();
    const result = fun();
    const end = new Date().getTime();
    const time = end - start;
    console.log(name + ": "  + time + "ms");
    return result;
}

function make_child_environment()
{
    return eval_test_vm.make_environment(eval_test_vm.get_environment());
}

function quote(expr)
{
    return eval_test_vm.list(eval_test_vm.sym("quote"), expr);
}

describe("Evaluation & Operation", () => {

    it("Built-in operators are defined.", () => {

        const operators = [
            "%vau",
            "%def",
            "%progn",
            "%if",
            "%loop",
            "%unwind-protect",
        ];

        for (const name of operators) {
            const op = eval_test_vm.get_environment().lookup(eval_test_vm.fsym(name));
            eval_test_vm.assert_type(op, eval_test_vm.Built_in_operator);
        }

    });

    it("Built-in functions are defined.", () => {

        const functions = [
            "%*",
            "%+",
            "%-",
            "%/",
            "%<",
            "%<=",
            "%=",
            "%>",
            "%>=",
            "%add-method",
            "%boundp",
            "%car",
            "%catch",
            "%cdr",
            "%class-name",
            "%class-of",
            "%class-symbol",
            "%cons",
            "%eq",
            "%eval",
            "%find-method",
            "%function-symbol",
            "%intern",
            "%keyword-symbol",
            "%list*",
            "%list-length",
            "%list-subseq",
            "%make-environment",
            "%make-instance",
            "%make-standard-class",
            "%nth",
            "%nthcdr",
            "%panic",
            "%progv",
            "%push-delim-subcont",
            "%push-prompt",
            "%push-subcont-barrier",
            "%reinitialize-standard-class",
            "%reverse",
            "%set-slot-value",
            "%slot-bound-p",
            "%slot-value",
            "%string-subseq",
            "%subclassp",
            "%symbol-name",
            "%take-subcont",
            "%throw",
            "%typep",
            "%unwrap",
            "%variable-symbol",
            "%wrap",
        ];

        for (const name of functions) {
            const op = eval_test_vm.get_environment().lookup(eval_test_vm.fsym(name));
            eval_test_vm.assert_type(op, eval_test_vm.Function);
        }

    });

    it("Many objects evaluate to themselves.", () => {

        const examples = [
            eval_test_vm.num(1),
            eval_test_vm.kwd("foo"),
            eval_test_vm.t(),
            eval_test_vm.f(),
            eval_test_vm.nil(),
            eval_test_vm.void(),
            eval_test_vm.ignore(),
            // Plain JS objects, too.
            12,
            true,
            false,
            undefined,
            { hello: "world" },
            [1,2],
        ];

        for (const ex of examples)
            for (const eval_fun of [eval_test_vm.eval, eval_test_vm.eval_form])
                assert.equal(eval_fun(ex), ex);

    });

    it("Symbols evaluate to the value they are bound to.", () => {

        const examples = [
            [eval_test_vm.csym("object"), eval_test_vm.lisp_class(eval_test_vm.Object)],
            [eval_test_vm.csym("class"), eval_test_vm.lisp_class(eval_test_vm.Class)],
        ];

        for (const [symbol, value] of examples)
            for (const eval_fun of [eval_test_vm.eval, eval_test_vm.eval_form])
                assert.equal(eval_fun(symbol), value);

    });

    /*
     * The following two tests exercise only eval_form() and not
     * eval().  The reason is that they cause an error, and so
     * INVOKE-DEBUGGER is called, which attempts to print a stack
     * trace.  This works for eval_form() which pushes the root
     * prompt, but not for eval(), which doesn't.
     */

    it("Evaluating an unbound symbol causes an error.", () => {

        for (const eval_fun of [eval_test_vm.eval_form]) {
            assert.throws(() => eval_fun(eval_test_vm.sym("this-is-not-bound")),
                          "Unbound variable: this-is-not-bound");
            assert.throws(() => eval_fun(eval_test_vm.fsym("this-is-not-bound")),
                          "Unbound function: this-is-not-bound");
            assert.throws(() => eval_fun(eval_test_vm.csym("this-is-not-bound")),
                          "Unbound class: this-is-not-bound");
        }

    });

    it("Evaluating a cons whose car is not an operator causes an error.", () => {

        const examples = [ eval_test_vm.nil(), undefined, "foo", eval_test_vm.str("foo"), eval_test_vm.num(1) ];

        for (const ex of examples)
            for (const eval_fun of [eval_test_vm.eval_form])
                assert.throws(() => eval_fun(eval_test_vm.list(ex)),
                              "Type assertion failed");

    });

    it("The environment to evaluate in can be specified.", () => {

        const env = make_child_environment();
        env.put(eval_test_vm.sym("foo"), eval_test_vm.num(12));
        for (const eval_fun of [eval_test_vm.eval, eval_test_vm.eval_form])
            assert(eval_test_vm.equal(eval_fun(eval_test_vm.sym("foo"), env),
                            eval_test_vm.num(12)));

    });

    it("vm.eval_form() doesn't swallow suspensions.", () => {

        const form = eval_test_vm.list(eval_test_vm.sym("take-subcont"), eval_test_vm.str("prompt"), eval_test_vm.sym("k"));
        assert.throws(() => eval_test_vm.eval_form(form), "Prompt not found: \"prompt\"");
        assert.instanceOf(eval_test_vm.eval(form), eval_test_vm.Suspension);

    });

    it("%EVAL uses the root environment if no environment is specified.", () => {

        assert.instanceOf(eval_test_vm.eval(eval_test_vm.list(eval_test_vm.sym("%eval"), quote(eval_test_vm.csym("object")))),
                          eval_test_vm.Class);

    });

    it("Symbols in the operator position are looked up in the function namespace.", () => {

        const env = make_child_environment();
        env.put(eval_test_vm.fsym("foo"), eval_test_vm.eval_js_string("(%vau #ignore #ignore 100)"));
        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(foo)", env), eval_test_vm.num(100)));

    });

    it("Non-symbol expressions in the operator position are evaluated normally.", () => {

        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("((%vau #ignore #ignore 200))"), eval_test_vm.num(200)));

    });

    it("Test vm.operate().", () => {

        const list_op = eval_test_vm.get_environment().lookup(eval_test_vm.fsym("list"));
        assert(eval_test_vm.equal(eval_test_vm.operate(list_op, eval_test_vm.nil()), eval_test_vm.nil()));
        assert(eval_test_vm.equal(eval_test_vm.operate(list_op, eval_test_vm.list(eval_test_vm.num(1))), eval_test_vm.list(eval_test_vm.num(1))));

    });

    it("The environment to operate in can be specified.", () => {

        const def_op = eval_test_vm.get_environment().lookup(eval_test_vm.fsym("%def"));
        const env = make_child_environment();
        eval_test_vm.operate(def_op, eval_test_vm.list(eval_test_vm.sym("x"), eval_test_vm.num(1)), env);
        assert(eval_test_vm.equal(env.lookup(eval_test_vm.sym("x")), eval_test_vm.num(1)));

    });

    it("match() binds symbols to their operands.", () => {

        const env = eval_test_vm.make_environment();
        const rhs = eval_test_vm.num(12);
        const result = eval_test_vm.match(eval_test_vm.sym("x"), rhs, env);
        assert(eval_test_vm.equal(result, rhs));
        assert(eval_test_vm.equal(env.lookup(eval_test_vm.sym("x")), eval_test_vm.num(12)));

    });

    it("match() requires keyword definiends to match exactly.", () => {

        const env = eval_test_vm.make_environment();
        const result = eval_test_vm.match(eval_test_vm.kwd("x"), eval_test_vm.kwd("x"), env);
        assert.throws(() => env.lookup(eval_test_vm.sym("x")),
                      "Unbound variable: x");
        assert.throws(() => eval_test_vm.match(eval_test_vm.kwd("x"), eval_test_vm.nil()),
                      "Match error: :x vs ()");

    });

    it("match() recursively matches conses.", () => {

        const env = eval_test_vm.make_environment();
        const rhs = eval_test_vm.cons(eval_test_vm.num(12), eval_test_vm.num(33));
        const result = eval_test_vm.match(eval_test_vm.cons(eval_test_vm.sym("x"), eval_test_vm.sym("y")), rhs, env);
        assert(eval_test_vm.equal(rhs, result));
        assert(eval_test_vm.equal(env.lookup(eval_test_vm.sym("x")), eval_test_vm.num(12)));
        assert(eval_test_vm.equal(env.lookup(eval_test_vm.sym("y")), eval_test_vm.num(33)));

        assert.throws(() => eval_test_vm.match(eval_test_vm.cons(eval_test_vm.sym("x"), eval_test_vm.sym("y")), eval_test_vm.t(), env),
                      "Match error: (x . y) vs #t");

    });

    it("match() requires the operand of #NIL to be #NIL.", () => {

        const env = eval_test_vm.make_environment();
        assert(eval_test_vm.equal(eval_test_vm.match(eval_test_vm.nil(), eval_test_vm.nil(), env), eval_test_vm.nil()));

        assert.throws(() => eval_test_vm.match(eval_test_vm.nil(), eval_test_vm.t(), env),
                      "Match error: () vs #t");

    });

    it("#IGNORE ignores its operand in match().", () => {

        const env = eval_test_vm.make_environment();
        assert(eval_test_vm.equal(eval_test_vm.match(eval_test_vm.ignore(), eval_test_vm.num(1), env), eval_test_vm.num(1)));

    });

    it("Other definiends in match() are an error.", () => {

        const env = eval_test_vm.make_environment();
        assert.throws(() => eval_test_vm.match(eval_test_vm.num(1), eval_test_vm.void(), env),
                      "Type assertion failed");
        assert.throws(() => eval_test_vm.match(eval_test_vm.t(), eval_test_vm.void(), env),
                      "Type assertion failed");

    });

});

describe("%VAU", () => {

    it("%VAU constructs simple fexprs.", () => {

        const fexpr = eval_test_vm.eval_js_string("(%vau #ignore #ignore 12)");
        eval_test_vm.assert_type(fexpr, eval_test_vm.Fexpr);
        assert(eval_test_vm.equal(eval_test_vm.eval(eval_test_vm.list(fexpr)), eval_test_vm.num(12)));

    });

    it("%VAU sets fexpr properties.", () => {

        const def_env = make_child_environment();
        const fexpr = eval_test_vm.eval_js_string("(%vau x y z)", def_env);
        eval_test_vm.assert_type(fexpr, eval_test_vm.Fexpr);
        assert(eval_test_vm.equal(fexpr.param_tree, eval_test_vm.sym("x")));
        assert(eval_test_vm.equal(fexpr.env_param, eval_test_vm.sym("y")));
        assert(eval_test_vm.equal(fexpr.body_form, eval_test_vm.sym("z")));
        assert(eval_test_vm.equal(fexpr.def_env, def_env));

    });

    it("%VAU evaluates the body form and passes on errors.", () => {

        assert.throws(() => eval_test_vm.eval_js_string("((%vau #ignore #ignore z))"),
                      "Unbound variable: z");

    });

    it("%VAU receives the operands.", () => {

        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("((%vau (x y z) #ignore z) 1 2 3)"),
                        eval_test_vm.num(3)));

    });

    it("%VAU throws for illegal definiends.", () => {

        assert.throws(() => eval_test_vm.eval_js_string("(%vau 1 #ignore #ignore)"),
                      "Type assertion failed");
        assert.throws(() => eval_test_vm.eval_js_string("(%vau #ignore 1 #ignore)"),
                      "Type assertion failed");

    });

    it("%VAU binds the environment parameter.", () => {

        const dyn_env = make_child_environment();
        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("((%vau #ignore env env))", dyn_env),
                        dyn_env));

    });

});

describe("%DEF", () => {

    it("%DEF evaluates the expression and matches the definiend.", () => {

        const env = make_child_environment();
        const fexpr = eval_test_vm.eval_js_string("(%def #'some-fexpr (%vau x #ignore 12))", env);
        eval_test_vm.assert_type(fexpr, eval_test_vm.Fexpr);
        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(some-fexpr)", env), eval_test_vm.num(12)));
        assert(eval_test_vm.equal(env.lookup(eval_test_vm.fsym("some-fexpr")), fexpr));
        assert.throws(() => eval_test_vm.get_environment().lookup(eval_test_vm.fsym("some-fexpr")),
                      "Unbound function: some-fexpr");

    });

    it("%DEF throws for illegal definiends.", () => {

        assert.throws(() => eval_test_vm.eval_js_string("(%def 1 #ignore)"),
                      "Type assertion failed");
        assert.throws(() => eval_test_vm.eval_js_string("(%def #t #ignore)"),
                      "Type assertion failed");

    });

    it("%DEF passes on errors from the expression.", () => {

        eval_test_vm.progv([eval_test_vm.STANDARD_OUTPUT], [MUFFLED_STREAM], () => {
            assert.throws(() => eval_test_vm.eval_js_string("(%def #ignore x1)"),
                          "Unbound variable: x1");
        });

    });

});

describe("%PROGN", () => {

    it("%PROGN evaluates its operands and returns the result of the last.", () => {

        const env = make_child_environment();
        env.put(eval_test_vm.sym("x"), eval_test_vm.num(3));
        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(%progn 1 2 x)", env),
                        eval_test_vm.num(3)));

    });

    it("%PROGN evaluates to #VOID if there are no operands.", () => {

        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(%progn)"),
                        eval_test_vm.void()));

    });

    it("%PROGN passes on errors from the operands.", () => {

        eval_test_vm.progv([eval_test_vm.STANDARD_OUTPUT], [MUFFLED_STREAM], () => {
            assert.throws(() => eval_test_vm.eval_js_string("(%progn 1 x2 2)"),
                          "Unbound variable: x2");
        });

    });

});

describe("%IF", () => {

    it("%IF evaluates the test and sub-expressions.", () => {

        const env = make_child_environment();

        env.put(eval_test_vm.sym("x"), eval_test_vm.t());
        env.put(eval_test_vm.sym("y"), eval_test_vm.f());

        env.put(eval_test_vm.sym("a"), eval_test_vm.num(1));
        env.put(eval_test_vm.sym("b"), eval_test_vm.num(2));

        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(%if x a b)", env), eval_test_vm.num(1)));
        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(%if y a b)", env), eval_test_vm.num(2)));

    });

    it("%IF requires a boolean test.", () => {

        assert.throws(() => eval_test_vm.eval_js_string("(%if 1 2 3)"),
                      "Type assertion failed");

    });

    it("%IF passes on errors from evaluating the subexpressions.", () => {

        eval_test_vm.progv([eval_test_vm.STANDARD_OUTPUT], [MUFFLED_STREAM], () => {
            assert.throws(() => eval_test_vm.eval_js_string("(%if x4 2 3)"),
                          "Unbound variable: x4");
            assert.throws(() => eval_test_vm.eval_js_string("(%if #t y3 3)"),
                          "Unbound variable: y3");
            assert.throws(() => eval_test_vm.eval_js_string("(%if #f 2 z3)"),
                          "Unbound variable: z3");
        });

    });

    it("%IF only evaluates one of consequent and alternative, not both.", () => {

        // X is unbound.
        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(%if #t 1 x)"), eval_test_vm.num(1)));
        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(%if #f x 1)"), eval_test_vm.num(1)));

    });

});

describe("Wrapping and unwrapping.", () => {

    it("Functions can be unwrapped.", () => {

        const wrapped = eval_test_vm.alien_function(() => eval_test_vm.t());
        assert.equal(eval_test_vm.wrap(wrapped).unwrap(), wrapped);

    });

    it("Wrapping induces argument evaluation.", () => {

        const env = make_child_environment();
        env.put(eval_test_vm.sym("x"), eval_test_vm.num(1));
        env.put(eval_test_vm.sym("y"), eval_test_vm.num(2));
        env.put(eval_test_vm.fsym("f"), eval_test_vm.wrap(eval_test_vm.eval_js_string("(%vau args #ignore args)")));

        assert(eval_test_vm.equal(eval_test_vm.eval_js_string("(f x y)", env),
                        eval_test_vm.list(eval_test_vm.num(1), eval_test_vm.num(2))));

    });

    it("Wrap requires its argument to be an operator.", () => {

        assert.throws(() => eval_test_vm.wrap(1), "Type assertion failed");
        assert.throws(() => eval_test_vm.wrap(eval_test_vm.t()), "Type assertion failed");

    });

    it("Functions can be (multiply) wrapped.", () => {

        const doubly_wrapped = eval_test_vm.wrap(eval_test_vm.wrap(eval_test_vm.alien_function(() => eval_test_vm.t())));
        assert.equal(eval_test_vm.eval(eval_test_vm.list(doubly_wrapped)), eval_test_vm.t());

    });

});

describe("Alien Operators", () => {

    it("Alien operators can be defined from JS functions.", () => {

        const op = eval_test_vm.alien_operator((a, b, c) => {
            assert.equal(a, eval_test_vm.sym("x"));
            assert.equal(b, eval_test_vm.sym("y"));
            assert.equal(c, eval_test_vm.sym("z"));
            return eval_test_vm.t();
        });

        assert(op instanceof eval_test_vm.Built_in_operator);

        const result = eval_test_vm.operate(op, eval_test_vm.list(eval_test_vm.sym("x"), eval_test_vm.sym("y"), eval_test_vm.sym("z")));
        assert(eval_test_vm.equal(result, eval_test_vm.t()));

    });

    it("Alien functions can be defined from JS functions.", () => {

        const op = eval_test_vm.alien_function((a, b, c) => a + b + c);

        assert(op instanceof eval_test_vm.Function);
        assert(op.unwrap() instanceof eval_test_vm.Built_in_operator);

        const env = make_child_environment();
        env.put(eval_test_vm.sym("x"), 1);
        env.put(eval_test_vm.sym("y"), 2);
        env.put(eval_test_vm.sym("z"), 3);
        const result = eval_test_vm.operate(op, eval_test_vm.list(eval_test_vm.sym("x"), eval_test_vm.sym("y"), eval_test_vm.sym("z")), env);
        assert(eval_test_vm.equal(result, 6));

    });

});

describe("Generic Functions", () => {

    it("Test add_method() and find_method().", () => {

        // Define a method M1 on OBJECT.
        const method_name = eval_test_vm.sym("m1");
        const method = eval_test_vm.alien_function(() => eval_test_vm.void());
        eval_test_vm.lisp_class(eval_test_vm.Object).add_method(method_name, method);

        // Test that strings and numbers inherit the method.
        for (const cls of [eval_test_vm.lisp_class(eval_test_vm.Object),
                           eval_test_vm.lisp_class(eval_test_vm.String),
                           eval_test_vm.lisp_class(eval_test_vm.Number)]) {
            assert(method === cls.find_method(method_name));

            // Test an unbound method.
            assert.throws(() => cls.find_method(eval_test_vm.sym("m2")),
                          "Unbound method: m2");
        }

        // Override the method for strings.
        const str_method = eval_test_vm.alien_function(() => eval_test_vm.void());
        eval_test_vm.lisp_class(eval_test_vm.String).add_method(method_name, str_method);

        // Test that it returns the new method for strings...
        assert(str_method === eval_test_vm.lisp_class(eval_test_vm.String).find_method(method_name));
        // ...and still the old one for numbers and objects.
        assert(method === eval_test_vm.lisp_class(eval_test_vm.Number).find_method(method_name));
        assert(method === eval_test_vm.lisp_class(eval_test_vm.Object).find_method(method_name));
    });

});

describe("Panicking", () => {

    it("Panics can not be caught by condition handlers.", () => {

        assert.throws(() => eval_test_vm.eval_js_string("(panic #void)"),
                      "LISP panic!");

        const env = make_child_environment();
        env.put(eval_test_vm.sym("cause"), new Error("it happened"));

        assert.throws(() => eval_test_vm.eval_js_string("(panic cause)", env),
                      "LISP panic: it happened");

        assert.throws(() =>
            eval_test_vm.eval_js_string(`(block b
                                 (handler-bind ((object (lambda (e) (return-from b))))
                                   (panic cause)))`, env),
            "LISP panic: it happened");

    });

    it("Panics do trigger UNWIND-PROTECT.", () => {

        const env = make_child_environment();
        env.put(eval_test_vm.sym("cause"), new Error("it happened"));

        eval_test_vm.progv([eval_test_vm.STANDARD_OUTPUT], [MUFFLED_STREAM], () => {
            // Check that UW's cleanup expression runs and overrides the panic.
            assert.throws(() => eval_test_vm.eval_js_string(`(unwind-protect (panic cause)
                                                      this-var-is-unbound)`,
                                                  env),
                          "LISP panic: Unbound variable: this-var-is-unbound");
        });

    });

    it("Panics do trigger %PROGV.", () => {

        const env = make_child_environment();
        env.put(eval_test_vm.sym("cause"), new Error("it happened"));

        const old_stdout = eval_test_vm.eval_js_string(`(dynamic *standard-output*)`);

        // Temporarily bind stdout to 'foo and panic...
        assert.throws(() => eval_test_vm.eval_js_string(`(dynamic-let ((*standard-output* 'foo))
                                                 (panic cause))`, env),
                      "LISP panic: it happened");

        // Check that it has been rebound to old value even though we panicked.
        assert.strictEqual(old_stdout, eval_test_vm.eval_js_string(`(dynamic *standard-output*)`));

    });

    it("ERROR panics if there is no handler.", () => {

        const env = make_child_environment();
        env.put(eval_test_vm.sym("cause"), new Error("foo"));

        assert.throws(() => eval_test_vm.eval_js_string("(error cause)", env),
                      "LISP panic: foo");

    });

});

;// CONCATENATED MODULE: ./test/control-test.mjs




const control_test_vm = (0,external_lispx_vm_umd_min_js_.make_vm)();

function lookup_operator(name)
{
    return control_test_vm.get_environment().lookup(control_test_vm.fsym(name));
}

describe("Continuations", () => {

    it("Test %TAKE-SUBCONT suspension and resumption.", () => {

        const take_subcont = lookup_operator("%take-subcont");
        const prompt = control_test_vm.str("p");
        const handler = control_test_vm.alien_function(() => {}); // Unused here.

        const susp = control_test_vm.operate(take_subcont, control_test_vm.list(prompt, handler));

        assert.instanceOf(susp, control_test_vm.Suspension);
        assert.equal(susp.prompt, prompt);
        assert.equal(susp.handler, handler);

        const k = susp.continuation;
        assert.instanceOf(k, control_test_vm.Continuation);
        assert.isNull(k.inner);

        const resum = new control_test_vm.Resumption(k, control_test_vm.alien_function(() => control_test_vm.num(100)));
        assert(control_test_vm.equal(resum.resume(), control_test_vm.num(100)));

    });

});

describe("Dynamic Variables", () => {

    it("Can create a dynamic variable with initial value.", () => {

        const dyn = control_test_vm.make_dynamic(12);
        assert.equal(12, dyn.get_value());
        dyn.set_value(11);
        assert.equal(11, dyn.get_value());

    });

    it("Can dynamically bind variables.", () => {

        const dyn1 = control_test_vm.make_dynamic(1);
        const dyn2 = control_test_vm.make_dynamic(2);
        assert.equal(1, dyn1.get_value());
        assert.equal(2, dyn2.get_value());
        assert.equal(30, control_test_vm.progv([dyn1, dyn2], [10, 20], () => {
            return dyn1.get_value() + dyn2.get_value();
        }));
        assert.equal(1, dyn1.get_value());
        assert.equal(2, dyn2.get_value());
        assert.throws(() =>
            control_test_vm.progv([dyn1], [10],
                     () => { throw new Error("value: " + dyn1.get_value()) }),
            "value: 10");
        assert.equal(1, dyn1.get_value());
        assert.equal(2, dyn2.get_value());

    });

});

;// CONCATENATED MODULE: ./test/seq-test.mjs




const seq_test_vm = (0,external_lispx_vm_umd_min_js_.make_vm)();

/*
 * These tests can be somewhat cursory because we have additional
 * testing of the exported Lisp functions.
 */

describe("Sequence & List Utilities", () => {

    it("Test list_star().", () => {

        assert(seq_test_vm.equal(seq_test_vm.list_star(),
                        seq_test_vm.nil()));
        assert(seq_test_vm.equal(seq_test_vm.list_star(seq_test_vm.num(1)),
                        seq_test_vm.num(1)));
        assert(seq_test_vm.equal(seq_test_vm.list_star(seq_test_vm.num(1), seq_test_vm.num(2)),
                        seq_test_vm.cons(seq_test_vm.num(1), seq_test_vm.num(2))));
        assert(seq_test_vm.equal(seq_test_vm.list_star(seq_test_vm.num(1), seq_test_vm.num(2), seq_test_vm.num(3)),
                        seq_test_vm.cons(seq_test_vm.num(1), seq_test_vm.cons(seq_test_vm.num(2), seq_test_vm.num(3)))));

    });

    it("Test nthcdr().", () => {

        assert(seq_test_vm.equal(seq_test_vm.nthcdr(0, seq_test_vm.list(1, 2, 3)),
                        seq_test_vm.list(1, 2, 3)));
        assert(seq_test_vm.equal(seq_test_vm.nthcdr(1, seq_test_vm.list(1, 2, 3)),
                        seq_test_vm.list(2, 3)));
        assert(seq_test_vm.equal(seq_test_vm.nthcdr(2, seq_test_vm.list(1, 2, 3)),
                        seq_test_vm.list(3)));
        assert(seq_test_vm.equal(seq_test_vm.nthcdr(3, seq_test_vm.list(1, 2, 3)),
                        seq_test_vm.nil()));
        assert.throws(() => seq_test_vm.nthcdr(4, seq_test_vm.list(1, 2, 3)), "Out of bounds");

    });

    it("Test list_length().", () => {

        assert(seq_test_vm.equal(seq_test_vm.list_length(seq_test_vm.nil()), 0));
        assert(seq_test_vm.equal(seq_test_vm.list_length(seq_test_vm.list(1, 2, 3)), 3));

    });

    it("Test mapcar().", () => {

        assert(seq_test_vm.equal(seq_test_vm.nil(), seq_test_vm.mapcar((elt) => false, seq_test_vm.nil())));

        const list = seq_test_vm.list(seq_test_vm.num(1), seq_test_vm.num(2), seq_test_vm.num(3));
        const expected = seq_test_vm.list(seq_test_vm.num(2), seq_test_vm.num(3), seq_test_vm.num(4));
        assert.deepEqual(expected, seq_test_vm.mapcar((elt) => seq_test_vm.add(elt, seq_test_vm.one()), list));

    });

    it("Test mapc().", () => {

        assert(seq_test_vm.equal(seq_test_vm.nil(), seq_test_vm.mapc((elt) => false, seq_test_vm.nil())));

        let array = [];
        const list = seq_test_vm.list(seq_test_vm.num(1), seq_test_vm.num(2), seq_test_vm.num(3));
        assert(list === seq_test_vm.mapc((elt) => array.push(elt), list));
        assert.deepEqual(array, [seq_test_vm.num(1), seq_test_vm.num(2), seq_test_vm.num(3)]);

    });

    it("Test list_subseq().", () => {

        assert(seq_test_vm.equal(seq_test_vm.list_subseq(seq_test_vm.list(1, 2, 3), 0),
                        seq_test_vm.list(1, 2, 3)));
        assert(seq_test_vm.equal(seq_test_vm.list_subseq(seq_test_vm.list(1, 2, 3), 0, 1),
                        seq_test_vm.list(1)));
        assert(seq_test_vm.equal(seq_test_vm.list_subseq(seq_test_vm.list(1, 2, 3), 2),
                        seq_test_vm.list(3)));
        assert(seq_test_vm.equal(seq_test_vm.list_subseq(seq_test_vm.list(1, 2, 3), 1, 2),
                        seq_test_vm.list(2)));

    });

    it("Test string_subseq().", () => {

        assert(seq_test_vm.equal(seq_test_vm.string_subseq(seq_test_vm.str("123"), 0),
                        seq_test_vm.str("123")));
        assert(seq_test_vm.equal(seq_test_vm.string_subseq(seq_test_vm.str("123"), 0, 1),
                        seq_test_vm.str("1")));
        assert(seq_test_vm.equal(seq_test_vm.string_subseq(seq_test_vm.str("123"), 2),
                        seq_test_vm.str("3")));
        assert(seq_test_vm.equal(seq_test_vm.string_subseq(seq_test_vm.str("123"), 1, 2),
                        seq_test_vm.str("2")));

    });

    it("Test append().", () => {

        assert(seq_test_vm.equal(seq_test_vm.append(seq_test_vm.nil(), seq_test_vm.nil()), seq_test_vm.nil()));
        assert(seq_test_vm.equal(seq_test_vm.append(seq_test_vm.list(1), seq_test_vm.nil()), seq_test_vm.list(1)));
        assert(seq_test_vm.equal(seq_test_vm.append(seq_test_vm.nil(), seq_test_vm.list(1)), seq_test_vm.list(1)));
        assert(seq_test_vm.equal(seq_test_vm.append(seq_test_vm.nil(), seq_test_vm.list(1, 2, 3)), seq_test_vm.list(1, 2, 3)));
        assert(seq_test_vm.equal(seq_test_vm.append(seq_test_vm.list(1, 2, 3), seq_test_vm.nil()), seq_test_vm.list(1, 2, 3)));
        assert(seq_test_vm.equal(seq_test_vm.append(seq_test_vm.list(1, 2), seq_test_vm.list(3, 4, 5)), seq_test_vm.list(1, 2, 3, 4, 5)));
        assert(seq_test_vm.equal(seq_test_vm.append(seq_test_vm.list(1, 2, 3), 4), seq_test_vm.list_star(1, 2, 3, 4)));

    });

    it("Test some().", () => {

        assert(seq_test_vm.equal(seq_test_vm.some(seq_test_vm.num(12)), seq_test_vm.list(seq_test_vm.num(12))));

    });

    it("Test optional().", () => {

        assert(seq_test_vm.equal(seq_test_vm.optional(seq_test_vm.nil()), seq_test_vm.void()));
        assert(seq_test_vm.equal(seq_test_vm.optional(seq_test_vm.nil(), () => 12), 12));
        assert(seq_test_vm.equal(seq_test_vm.optional(seq_test_vm.some(10)), 10));
        assert(seq_test_vm.equal(seq_test_vm.optional(seq_test_vm.some(10), () => 12), 10));

    });

});

;// CONCATENATED MODULE: ./test/stream-test.mjs




const stream_test_vm = (0,external_lispx_vm_umd_min_js_.make_vm)();

function string_input_stream(js_string)
{
    return new stream_test_vm.String_input_stream(stream_test_vm.str(js_string));
}

function string_output_stream()
{
    return new stream_test_vm.String_output_stream();
}

describe("String Input Streams", () => {

    it("UTF-8 bytes can be read from a string.", () => {

        const st = string_input_stream("bar");

        assert.throws(() => st.unread_byte("x"), "Cannot unread");

        assert.equal(st.peek_byte(false, false, -1), "b");

        assert.equal(st.peek_byte(false, false, -1), "b");

        assert.equal(st.read_byte(false, -1), "b");

        assert.throws(() => st.unread_byte("x"), "Cannot unread");

        st.unread_byte("b");

        assert.equal(st.peek_byte(false, false, -1), "b");

        assert.equal(st.read_byte(false, -1), "b");

        assert.equal(st.peek_byte(false, false, -1), "a");

        assert.equal(st.read_byte(false, -1), "a");

        assert.equal(st.peek_byte(false, false, -1), "r");

        assert.equal(st.read_byte(false, -1), "r");

        assert.equal(st.peek_byte(false, false, -1), -1);

        assert.equal(st.peek_byte(false, false, -1), -1);

        assert.equal(st.read_byte(false, -1), -1);

        st.unread_byte("r");

        assert.equal(st.peek_byte(false, false, -1), "r");

        assert.equal(st.read_byte(false, -1), "r");

        assert.equal(st.read_byte(false, -1), -1);

        assert.throws(() => st.peek_byte(false, true), "EOF");

        assert.throws(() => st.read_byte(true), "EOF");

        assert.throws(() => st.read_byte(true), "EOF");

        assert.equal(st.peek_byte(false, false, -1), -1);

        assert.equal(st.read_byte(false, -1), -1);

        st.unread_byte("r");

        assert.equal(st.peek_byte(false, false, -1), "r");

        assert.equal(st.read_byte(false, -1), "r");

        assert.equal(st.peek_byte(false, false, -1), -1);

        assert.equal(st.read_byte(false, -1), -1);

    });

    it("read_byte() has convenient default arguments.", () => {

        const st = string_input_stream("bar");

        assert.equal(st.read_byte(), "b");
        assert.equal(st.read_byte(), "a");
        assert.equal(st.read_byte(), "r");

        assert.equal(st.read_byte(false, -1), -1);
        assert.equal(st.read_byte(false), stream_test_vm.void());
        assert.throws(() => st.read_byte(), "EOF");

    });

    it("Test skipping whitespace in peek_byte().", () => {

        const st = string_input_stream("  b a r");

        assert.equal(st.peek_byte(false), " ");
        assert.equal(st.read_byte(), " ");
        assert.equal(st.peek_byte(false), " ");
        assert.equal(st.peek_byte(true), "b");
        assert.equal(st.read_byte(), "b");
        assert.equal(st.peek_byte(false), " ");
        assert.equal(st.peek_byte(true), "a");
        assert.equal(st.read_byte(), "a");
        assert.equal(st.peek_byte(true), "r");
        assert.equal(st.read_byte(), "r");
        assert.equal(st.peek_byte(true, false, -1), -1);
        assert.equal(st.peek_byte(false, false, -1), -1);

    });

});

describe("String Output Streams", () => {

    it("UTF-8 bytes can be written to a string.", () => {

        const st = string_output_stream();

        assert(stream_test_vm.equal(st.get_string(), stream_test_vm.str("")));

        assert.equal(st.write_byte("a"), "a");

        assert(stream_test_vm.equal(st.get_string(), stream_test_vm.str("a")));

        assert.equal(st.write_byte("b"), "b");

        assert(stream_test_vm.equal(st.get_string(), stream_test_vm.str("ab")));

        assert(stream_test_vm.equal(st.write_string(stream_test_vm.str("Iñtërnâtiônàlizætiøn")),
                        stream_test_vm.str("Iñtërnâtiônàlizætiøn")));

        assert(stream_test_vm.equal(st.get_string(), stream_test_vm.str("abIñtërnâtiônàlizætiøn")));

        assert.equal(st.write_byte("c"), "c");

        assert(stream_test_vm.equal(st.get_string(), stream_test_vm.str("abIñtërnâtiônàlizætiønc")));

    });

    it("Test force_output()", () => {

        /*
         * force_output() is a no-op for string output streams,
         * just test that it's implemented.
         */

        const st = string_output_stream();

        assert(stream_test_vm.equal(st.force_output(), stream_test_vm.void()));

    });

    it("Test fresh_line()", () => {

        const st = string_output_stream();

        assert(stream_test_vm.equal(st.fresh_line(), stream_test_vm.t()));

        assert.equal(st.write_byte("a"), "a");

        assert(stream_test_vm.equal(st.fresh_line(), stream_test_vm.t()));

        assert(stream_test_vm.equal(st.get_string(), stream_test_vm.str("\na\n")));

    });

    it("Test FRESH-LINE and FORCE-OUTPUT", () => {

        class Test_stream extends stream_test_vm.Output_stream
        {
            constructor() {
                super();
                this.fresh_line_call_ct = 0;
                this.force_output_call_ct = 0;
            }
            fresh_line() { this.fresh_line_call_ct++; return stream_test_vm.f(); }
            force_output() { this.force_output_call_ct++; return stream_test_vm.void(); }
        }

        // Test that they can be called with a stream as argument
        const test_stream_1 = new Test_stream();
        assert.equal(0, test_stream_1.fresh_line_call_ct);
        assert.equal(0, test_stream_1.force_output_call_ct);
        stream_test_vm.eval_form(stream_test_vm.list(stream_test_vm.sym("fresh-line"), test_stream_1));
        assert.equal(1, test_stream_1.fresh_line_call_ct);
        assert.equal(0, test_stream_1.force_output_call_ct);
        stream_test_vm.eval_form(stream_test_vm.list(stream_test_vm.sym("force-output"), test_stream_1));
        assert.equal(1, test_stream_1.fresh_line_call_ct);
        assert.equal(1, test_stream_1.force_output_call_ct);

        // Test that they work on *STANDARD-OUTPUT* otherwise
        const test_stream_2 = new Test_stream();
        stream_test_vm.progv([stream_test_vm.STANDARD_OUTPUT], [test_stream_2], () => {
            stream_test_vm.eval_form(stream_test_vm.list(stream_test_vm.sym("fresh-line")));
            assert.equal(1, test_stream_2.fresh_line_call_ct);
            assert.equal(0, test_stream_2.force_output_call_ct);
            stream_test_vm.eval_form(stream_test_vm.list(stream_test_vm.sym("force-output")));
            assert.equal(1, test_stream_2.fresh_line_call_ct);
            assert.equal(1, test_stream_2.force_output_call_ct);
        });
    });

});

;// CONCATENATED MODULE: ./test/read-test.mjs




const read_test_vm = (0,external_lispx_vm_umd_min_js_.make_vm)();

function read_test_string_input_stream(js_string)
{
    return new read_test_vm.String_input_stream(read_test_vm.str(js_string));
}

describe("Reader", () => {

    it("Whitespace is correctly parsed.", () => {

        /*
         * This tests the situation where we there is only
         * whitespace, and no more object to read, before the EOF.
         */

        const examples = [
            "", "    ", "  \n", "\n   \t \n   ",
            ";", "   ;", "; hello", ";;;;; headline", "\t\n;foo",
            "; foo \n ; bar \n", ";;;; foo\n\t ",
            "#||#", "#||##||#", "#|#|hello|#|#\n;foo\n\n"
        ];

        for (const ex of examples) {
            assert.throws(() => read_test_vm.read(read_test_string_input_stream(ex)), "EOF");
            assert.equal(read_test_vm.read(read_test_string_input_stream(ex), false), read_test_vm.void());
            assert.equal(read_test_vm.read(read_test_string_input_stream(ex), false, -1), -1);
            /*
             * Check whether dot is allowed as eof_value. See read().
             */
            assert.equal(read_test_vm.read(read_test_string_input_stream(ex), false, read_test_vm.sym(".")),
                         read_test_vm.sym("."));
        }

        /*
         * Append an object after a newline to the example strings and test
         * that it is read.
         */

        for (const ex of examples) {
            const st = read_test_string_input_stream(ex + "\n1");
            assert(read_test_vm.equal(read_test_vm.read(st), read_test_vm.num(1)));
        }

    });

    it("Objects are correctly parsed.", () => {

        const examples = {

            // Numbers.
            "1":
            read_test_vm.num("1"),
            "-1":
            read_test_vm.num("-1"),
            "100000000000000000.1378912739821739821":
            read_test_vm.num("100000000000000000.1378912739821739821"),
            "-100000000000000000.1378912739821739821":
            read_test_vm.num("-100000000000000000.1378912739821739821"),

            // Symbols.
            "a":
            read_test_vm.sym("a"),
            "hello-world":
            read_test_vm.sym("hello-world"),
            "Iñtërnâtiônàlizætiøn":
            read_test_vm.sym("Iñtërnâtiônàlizætiøn"),

            // Escape characters in symbols.
            "a\\nb":
            read_test_vm.sym("a\nb"),
            "a\\\\b":
            read_test_vm.sym("a\\b"),
            "\\t\\n\\\\\\|\\\"":
            read_test_vm.sym("\t\n\\|\""),

            // Stuff that's almost a number but parses as a symbol.
            "1.":
            read_test_vm.sym("1."),
            ".1":
            read_test_vm.sym(".1"),
            ".1.":
            read_test_vm.sym(".1."),
            "1.a":
            read_test_vm.sym("1.a"),

            // Keyword symbols.
            ":foo":
            read_test_vm.sym("foo").to_keyword_symbol(),
            ":|foo|":
            read_test_vm.sym("foo").to_keyword_symbol(),

            // Function symbols.
            "#'foo":
            read_test_vm.sym("foo").to_function_symbol(),
            "#'|foo|":
            read_test_vm.sym("foo").to_function_symbol(),

            // Class symbols.
            "#^foo":
            read_test_vm.sym("foo").to_class_symbol(),
            "#^|foo|":
            read_test_vm.sym("foo").to_class_symbol(),

            // Strings.
            '""':
            read_test_vm.str(""),
            '"foo"':
            read_test_vm.str("foo"),
            '"foo bar"':
            read_test_vm.str("foo bar"),
            '"foo\\nbar"':
            read_test_vm.str("foo\nbar"),
            '"foo\nbar"':
            read_test_vm.str("foo\nbar"),
            '"foo\\nbar"':
            read_test_vm.str("foo\\nbar"),
            '"foo\\nbar"':
            read_test_vm.str("foo\nbar"),
            '"\\t\\n\\\\\\|\\\""':
            read_test_vm.str("\t\n\\|\""),
            '"Iñtërnâtiônàlizætiøn"':
            read_test_vm.str("Iñtërnâtiônàlizætiøn"),

            // Escaped symbols.
            '||':
            read_test_vm.sym(""),
            '|foo|':
            read_test_vm.sym("foo"),
            '|foo bar|':
            read_test_vm.sym("foo bar"),
            '|foo\nbar|':
            read_test_vm.sym("foo\nbar"),
            '|foo\\nbar|':
            read_test_vm.sym("foo\nbar"),
            '|foo\nbar|':
            read_test_vm.sym("foo\nbar"),
            '|\\t\\n\\\\\\|\\\"|':
            read_test_vm.sym("\t\n\\|\""),
            '|Iñtërnâtiônàlizætiøn|':
            read_test_vm.sym("Iñtërnâtiônàlizætiøn"),

            // Constants.
            "#t":
            read_test_vm.t(),
            "#f":
            read_test_vm.f(),
            "#nil":
            read_test_vm.nil(),
            "#void":
            read_test_vm.void(),
            "#ignore":
            read_test_vm.ignore(),

            // Lists.
            "()":
            read_test_vm.nil(),
            "( \n )":
            read_test_vm.nil(),
            "(1)":
            read_test_vm.list(read_test_vm.num(1)),
            "(1 . 2)":
            read_test_vm.cons(read_test_vm.num(1), read_test_vm.num(2)),
            "(1 2)":
            read_test_vm.list(read_test_vm.num(1), read_test_vm.num(2)),
            "(1 2 . 3)":
            read_test_vm.list_star(read_test_vm.num(1), read_test_vm.num(2), read_test_vm.num(3)),
            "(())":
            read_test_vm.list(read_test_vm.nil()),
            "( ( ) )":
            read_test_vm.list(read_test_vm.nil()),
            "( ( ) . ( ) )":
            read_test_vm.cons(read_test_vm.nil(), read_test_vm.nil()),

            // Line comments inside lists.
            "(;\n)":
            read_test_vm.nil(),
            "(1 . ;;;\n 1)":
            read_test_vm.cons(read_test_vm.num(1), read_test_vm.num(1)),
            "(;\n 1 ;\n . ;\n 1 ;\n)":
            read_test_vm.cons(read_test_vm.num(1), read_test_vm.num(1)),
            "(1 ;\n 2 ;\n 3 ;\n)":
            read_test_vm.list(read_test_vm.num(1), read_test_vm.num(2), read_test_vm.num(3)),

            // Block comments inside lists.
            "(#||#)":
            read_test_vm.nil(),
            "(1 . #||# 1)":
            read_test_vm.cons(read_test_vm.num(1), read_test_vm.num(1)),
            "(#||# 1 . #||# 1)":
            read_test_vm.cons(read_test_vm.num(1), read_test_vm.num(1)),
            "(#||# 1 . #||# 1 #||#)":
            read_test_vm.cons(read_test_vm.num(1), read_test_vm.num(1)),
            "(1 2 #||# 3)":
            read_test_vm.list(read_test_vm.num(1), read_test_vm.num(2), read_test_vm.num(3)),

            // Quote.
            "'foo":
            read_test_vm.list(read_test_vm.sym("quote"), read_test_vm.sym("foo")),
            "'()":
            read_test_vm.list(read_test_vm.sym("quote"), read_test_vm.nil()),
            "'(1 . 2)":
            read_test_vm.list(read_test_vm.sym("quote"), read_test_vm.cons(read_test_vm.num(1), read_test_vm.num(2))),
            "' foo-can-have-whitespace":
            read_test_vm.list(read_test_vm.sym("quote"), read_test_vm.sym("foo-can-have-whitespace")),
        };

        /*
         * Try parsing each object.
         *
         * Here each object gets ended by the EOF.
         */

        for (const [string, object] of Object.entries(examples)) {
            assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream(string)), object));
        }

        /*
         * Try the same but append some whitespace before the object.
         */

        for (const [string, object] of Object.entries(examples)) {
            const s = "\n;foo\n  " + string;
            assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream(s)), object));
        }

        /*
         * Try the same but append some whitespace before and after
         * the object.
         */

        for (const [string, object] of Object.entries(examples)) {
            const s = "\n;foo\n  " + string + " \n";
            assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream(s)), object));
        }

        /*
         * Try the same but append whitespace before and a terminating
         * character after the object.
         */

        for (const term_char of ["(", ")", ";", "\"", "\'", "|"]) {
            for (const [string, object] of Object.entries(examples)) {
                const s = "\n;foo\n  " + string + term_char;
                const stream = read_test_string_input_stream(s);
                assert(read_test_vm.equal(read_test_vm.read(stream), object));

                // Test that the terminating character has been unread.
                // (To be pedantic, in the case of lists and delimited
                // objects like strings, it has never been read in the
                // first place.)
                assert.equal(stream.read_byte(), term_char);
            }
        }

    });

    it("List parsing errors are reported informatively.", () => {

        const examples = {
            ".": "Consing dot not allowed here",
            "#||#.": "Consing dot not allowed here",
            "#||# .": "Consing dot not allowed here",
            "(.": "Consing dot at start of list",
            "(#||#.": "Consing dot at start of list",
            "( .": "Consing dot at start of list",
            "(#||# .": "Consing dot at start of list",
            "(1 . 1 1)": "Multiple objects after consing dot",
            // Would be better to report "consing dot not allowed here"
            // but I won't complain.
            "(1 . 1 . 1)": "Multiple objects after consing dot",
            ")": "Unbalanced parenthesis"
        };

        for (const [string, message] of Object.entries(examples)) {
            assert.throws(() => read_test_vm.read(read_test_string_input_stream(string)),
                          message);
        }

    });

    it("Incomplete objects at EOF always cause an error.", () => {

        /*
         * A stream that ends with an incomplete object.
         *
         * Test that an EOF error is thrown regardless of eof_error_p.
         */

        const examples = [
            '"foo', "bla\\", "|foo", "|foo\\", "foo\\",
            "'", "'(",
            "(", "((", "(()", "(1 .", "(1 . (",
            "#|", "#||", "(#|", "(#||", "(#||#",
            "#|#|", "#|#||", "#|#||#",
        ];

        for (const e of examples) {

            const stream1 = read_test_string_input_stream(e);
            assert.throws(() => read_test_vm.read(stream1), "EOF");

            const stream2 = read_test_string_input_stream(e);
            assert.throws(() => read_test_vm.read(stream2, false), "EOF");

            const stream3 = read_test_string_input_stream(e);
            assert.throws(() => read_test_vm.read(stream3, false, -1), "EOF");

        }

    });

    it("Invalid escape characters lose.", () => {

        const msg = "Invalid escape character x";
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("\\x")), msg);
        assert.throws(() => read_test_vm.read(read_test_string_input_stream('"\\x"')), msg);
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("|\\x|")), msg);

    });

    it("Sharpsign at EOF causes an error.", () => {

        const msg = "EOF";
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("#")), msg);
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("#'")), msg);
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("#'\n")), msg);
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("#';\n;")), msg);

    });

    it("Unknown alphabetic characters after sharpsign cause an error.", () => {

        const msg = "Illegal constant";
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("#foo")), msg);
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("#b")), msg);

    });

    it("Unknown other characters after sharpsign cause an error.", () => {

        const msg = "Illegal dispatching character";
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("#_")), msg);
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("# ")), msg);

    });

});

describe("Reader Macros", () => {

    it("Test set_macro_character() with terminating char.", () => {

        assert.isFalse(read_test_vm.is_macro_character("~"));
        assert.isFalse(read_test_vm.is_terminating_macro_character("~"));

        read_test_vm.set_macro_character("~", function(stream, b) {
            assert.equal(b, "~");
            // Note that reader macro has to return option.
            return read_test_vm.some(read_test_vm.list(read_test_vm.sym("blargh"), read_test_vm.read(stream)));
        }, false);

        assert(read_test_vm.is_macro_character("~"));
        assert(read_test_vm.is_terminating_macro_character("~"));

        assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream("(1 ~foo ~bar 2)")),
                        read_test_vm.read(read_test_string_input_stream("(1 (blargh foo) (blargh bar) 2)"))));

        assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream("(foo~bar)")),
                        read_test_vm.read(read_test_string_input_stream("(foo (blargh bar))"))));

        assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream("(|foo|~bar)")),
                        read_test_vm.read(read_test_string_input_stream("(foo (blargh bar))"))));

        assert.throws(() => read_test_vm.read(read_test_string_input_stream("~")), "EOF");
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("(foo ~")), "EOF");
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("(foo ~)")), "Unbalanced parenthesis");

        read_test_vm.unset_macro_character("~");
        assert.isFalse(read_test_vm.is_macro_character("~"));
        assert.isFalse(read_test_vm.is_terminating_macro_character("~"));

    });

    it("Test set_macro_character() with non-terminating char.", () => {

        assert.isFalse(read_test_vm.is_macro_character("~"));
        assert.isFalse(read_test_vm.is_terminating_macro_character("~"));

        read_test_vm.set_macro_character("~", function(stream, b) {
            assert.equal(b, "~");
            // Note that reader macro has to return option.
            return read_test_vm.some(read_test_vm.list(read_test_vm.sym("blargh"), read_test_vm.read(stream)));
        }, true);

        assert(read_test_vm.is_macro_character("~"));
        assert.isFalse(read_test_vm.is_terminating_macro_character("~"));

        assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream("(1 ~foo ~bar 2)")),
                        read_test_vm.read(read_test_string_input_stream("(1 (blargh foo) (blargh bar) 2)"))));

        assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream("(foo~bar)")),
                        read_test_vm.read(read_test_string_input_stream("(foo~bar)"))));

        assert(read_test_vm.equal(read_test_vm.read(read_test_string_input_stream("(|foo|~bar)")),
                        read_test_vm.read(read_test_string_input_stream("(foo (blargh bar))"))));

        assert.throws(() => read_test_vm.read(read_test_string_input_stream("~")), "EOF");
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("(foo ~")), "EOF");
        assert.throws(() => read_test_vm.read(read_test_string_input_stream("(foo ~)")), "Unbalanced parenthesis");

        read_test_vm.unset_macro_character("~");

    });

    it("Test read_delimited_list().", () => {

        assert(read_test_vm.equal(read_test_vm.read_delimited_list(read_test_string_input_stream("1 2 3 ]"), "]"),
                        read_test_vm.read(read_test_string_input_stream("(1 2 3)"))));
        assert(read_test_vm.equal(read_test_vm.read_delimited_list(read_test_string_input_stream("1 2 . 3 ]"), "]"),
                        read_test_vm.read(read_test_string_input_stream("(1 2 . 3)"))));

    });

});

describe("eval_{stream,string,js_string}", () => {

    it("Test eval_{stream,string,js_string}().", () => {

        const eval_x_funs = [
            (js_string, env) => {
                const stream = new read_test_vm.String_input_stream(read_test_vm.str(js_string));
                return read_test_vm.eval_stream(stream, env);
            },
            (js_string, env) => read_test_vm.eval_string(read_test_vm.str(js_string), env),
            (js_string, env) => read_test_vm.eval_js_string(js_string, env)
        ];

        for (const eval_x_fun of eval_x_funs) {

            /*
             * Test that it evaluates forms and returns the final result.
             */

            assert(read_test_vm.equal(eval_x_fun(""), read_test_vm.void()));
            assert(read_test_vm.equal(eval_x_fun("; Hello"), read_test_vm.void()));
            assert(read_test_vm.equal(eval_x_fun("1 2 3"), read_test_vm.num(3)));

            /*
             * Test that it uses the VM's root environment by default.
             */

            assert(eval_x_fun("#'%vau") instanceof read_test_vm.Built_in_operator);

            /*
             * Test that another environment can be supplied.
             */
            assert.throws(() => eval_x_fun("#'%vau", read_test_vm.make_environment()),
                          "Unbound function: %vau");

            /*
             * Test that suspensions are not swallowed.
             */
            assert.throws(() => eval_x_fun("1 (take-subcont 'p k k) 2"),
                          "Prompt not found");

        }

    });

});

;// CONCATENATED MODULE: ./test/print-test.mjs




const print_test_vm = (0,external_lispx_vm_umd_min_js_.make_vm)();

function print_test_string_output_stream()
{
    return new print_test_vm.String_output_stream();
}

describe("Printer", () => {

    it("Test writing objects.", () => {

        const examples = {
            // JS objects write stupidly for now
            "#<<js undefined undefined>>": undefined,
            "#<<js object null>>": null,
            "#<<js boolean true>>": true,
            "#<<js number 12>>": 12,
            "#<<js string foo>>": "foo",
            "#<<js object [object Object]>>": {},
            "#<<js object >>": [],
            // Unreadable objects
            "#<string-output-stream>": print_test_string_output_stream(),
            // Standard objects
            "#<error>":
            new print_test_vm.Error(),
            "#<unbound-symbol-error :environment #<environment> :message \"Unbound variable: x\" :symbol x>":
            new print_test_vm.Unbound_symbol_error(print_test_vm.sym("x"), print_test_vm.make_environment()),
            // Constants
            "#t": print_test_vm.t(),
            "#f": print_test_vm.f(),
            "()": print_test_vm.nil(),
            "#void": print_test_vm.void(),
            "#ignore": print_test_vm.ignore(),
            // Numbers
            "-111111111111.111111111": print_test_vm.num("-111111111111.111111111"),
            "0": print_test_vm.num("0"),
            "111111111111.111111111": print_test_vm.num("111111111111.111111111"),
            // Strings
            "\"foo-bar\"": print_test_vm.str("foo-bar"),
            // Symbols
            "foo-bar": print_test_vm.sym("foo-bar"),
            "|foo bar|": print_test_vm.sym("foo bar"),
            "Iñtërnâtiônàlizætiøn":
            print_test_vm.sym("Iñtërnâtiônàlizætiøn"),
            "|Iñtërnâtiônàlizætiøn Iñtërnâtiônàlizætiøn|":
            print_test_vm.sym("Iñtërnâtiônàlizætiøn Iñtërnâtiônàlizætiøn"),
            // Keywords
            ":foo-bar": print_test_vm.kwd("foo-bar"),
            ":|foo bar|": print_test_vm.kwd("foo bar"),
            // Function symbols
            "#'foo-bar": print_test_vm.fsym("foo-bar"),
            "#'|foo bar|": print_test_vm.fsym("foo bar"),
            // Class symbols
            "#^foo-bar": print_test_vm.csym("foo-bar"),
            "#^|foo bar|": print_test_vm.csym("foo bar"),
            // Conses
            "(1)": print_test_vm.list(print_test_vm.num(1)),
            "((1))": print_test_vm.list(print_test_vm.list(print_test_vm.num(1))),
            "(1 2 3)": print_test_vm.list(print_test_vm.num(1), print_test_vm.num(2), print_test_vm.num(3)),
            "(1 2 . 3)": print_test_vm.list_star(print_test_vm.num(1), print_test_vm.num(2), print_test_vm.num(3)),
        };

        for (const [string, object] of Object.entries(examples)) {
            assert(print_test_vm.equal(string, print_test_vm.write_to_js_string(object)));
        }

    });

    it("Test writing objects whose output depends on *PRINT-ESCAPE*.", () => {

        // Lisp object, *PRINT-ESCAPE* = #T, *PRINT-ESCAPE* = #F
        const examples = [
            // Strings
            [print_test_vm.str("foo"), "\"foo\"", "foo"],
            [print_test_vm.str("fo\"o"), "\"fo\\\"o\"", "fo\"o"],
            [print_test_vm.str("fo\no"), "\"fo\no\"", "fo\no"],
            [print_test_vm.str("fo\\o"), "\"fo\\\\o\"", "fo\\o"],
            [print_test_vm.str("foo|bar"), "\"foo|bar\"", "foo|bar"],
            // Normal symbols
            [print_test_vm.sym("foo"), "foo", "foo"],
            [print_test_vm.sym("foo-bar"), "foo-bar", "foo-bar"],
            [print_test_vm.sym("let*"), "let*", "let*"],
            // Symbols that parse as numbers
            [print_test_vm.sym("0"), "|0|", "0"],
            [print_test_vm.sym("12"), "|12|", "12"],
            [print_test_vm.sym("-12.12"), "|-12.12|", "-12.12"],
            // Symbols that contain whitespace
            [print_test_vm.sym("foo bar"), "|foo bar|", "foo bar"],
            [print_test_vm.sym("fo\no"), "|fo\no|", "fo\no"],
            // Symbols that contain terminating characters
            [print_test_vm.sym("fo|o"), "|fo\\|o|", "fo|o"],
            [print_test_vm.sym("fo(o"), "|fo(o|", "fo(o"],
            [print_test_vm.sym("fo\"o"), "|fo\"o|", "fo\"o"],
            // Symbols that contain the escape character
            [print_test_vm.sym("fo\\o"), "|fo\\\\o|", "fo\\o"],
        ];

        for (const [object, escaped, not_escaped] of examples) {
            const escaped_result = print_test_vm.progv([print_test_vm.PRINT_ESCAPE], [print_test_vm.t()],
                                            () => print_test_vm.write_to_js_string(object));
            assert.strictEqual(escaped, escaped_result);
            const not_escaped_result = print_test_vm.progv([print_test_vm.PRINT_ESCAPE], [print_test_vm.f()],
                                                () => print_test_vm.write_to_js_string(object));
            assert.strictEqual(not_escaped, not_escaped_result);
        }

    });

});

;// CONCATENATED MODULE: ./test/js-test.mjs




const js_test_vm = (0,external_lispx_vm_umd_min_js_.make_vm)();

describe("JavaScript Interface", () => {

    it("Test to_lisp_boolean().", () => {

        assert.equal(js_test_vm.to_lisp_boolean(true), js_test_vm.t());
        assert.equal(js_test_vm.to_lisp_boolean(false), js_test_vm.f());

        assert.throws(() => js_test_vm.to_lisp_boolean(12), "Type assertion failed");

    });

    it("Test apply_js_method().", () => {

        assert.equal(js_test_vm.apply_js_method(9.656, js_test_vm.str("toFixed"), js_test_vm.list(2)),
                     "9.66");

    });

    it("Test js_global().", () => {

        assert(Math === js_test_vm.js_global(js_test_vm.str("Math")));

    });

    it("Test js_new() and js_get().", () => {

        class Point
        {
            constructor(x, y)
            {
                this.x = x;
                this.y = y;
            }
        }

        const pt = js_test_vm.js_new(Point, 1, 2);

        assert.equal(pt.x, 1);
        assert.equal(pt.y, 2);

        assert.equal(js_test_vm.js_get(pt, js_test_vm.str("x")), 1);
        assert.equal(js_test_vm.js_get(pt, js_test_vm.str("y")), 2);

    });

    it("Test to_js_function().", () => {

        const js_fun = js_test_vm.to_js_function(js_test_vm.get_environment().lookup(js_test_vm.fsym("list")));
        js_test_vm.assert_type(js_fun, "function");
        const list = js_fun(js_test_vm.num(1), js_test_vm.num(2));
        js_test_vm.assert_type(list, js_test_vm.List);
        assert(js_test_vm.equal(js_test_vm.elt(list, 0), js_test_vm.num(1)));
        assert(js_test_vm.equal(js_test_vm.elt(list, 1), js_test_vm.num(2)));

    });

    it("Test to_lisp_function().", () => {

        const js_fun = () => 123;
        const lisp_fun = js_test_vm.to_lisp_function(js_fun);
        js_test_vm.assert_type(lisp_fun, js_test_vm.Operator);
        js_test_vm.get_environment().put(js_test_vm.fsym("some-fun"), lisp_fun);
        assert.equal(123, js_test_vm.eval(js_test_vm.list(js_test_vm.sym("some-fun"))));

    });

    it("Test js_elt().", () => {
        const array = [1, 2, 3];
        assert.equal(1, js_test_vm.js_elt(array, js_test_vm.num(0)));
        assert.equal(2, js_test_vm.js_elt(array, js_test_vm.num(1)));
        assert.throws(() => js_test_vm.js_elt("foo", js_test_vm.num(1)), "Assertion failed");
        assert.throws(() => js_test_vm.js_elt(array, js_test_vm.f()), "expected number got #f");
    });

    it("Test JS console output.", () => {

        function test_output(expected_result, expected_output_lines, thunk)
        {
            const output_lines = [];
            function output_function(line) { output_lines.push(line); };
            const stream = new js_test_vm.JS_console_output_stream(output_function);
            assert.deepEqual(thunk(stream), expected_result);
            assert.deepEqual(output_lines, expected_output_lines);
        }

        test_output(js_test_vm.f(), [], (stream) => {
            return stream.fresh_line()
        });
        test_output(js_test_vm.f(), [], (stream) => {
            stream.fresh_line();
            return stream.fresh_line();
        });
        test_output(js_test_vm.void(), [], (stream) => {
            return stream.force_output()
        });
        test_output(js_test_vm.void(), [], (stream) => {
            stream.fresh_line();
            return stream.force_output()
        });
        test_output(js_test_vm.str("foo"), [], (stream) => {
            return stream.write_string(js_test_vm.str("foo"));
        });
        test_output(js_test_vm.void(), ["foo"], (stream) => {
            stream.write_string(js_test_vm.str("foo"));
            return stream.force_output();
        });
        test_output(js_test_vm.void(), ["foobar"], (stream) => {
            stream.write_string(js_test_vm.str("foo"));
            stream.write_string(js_test_vm.str("bar"));
            return stream.force_output();
        });
        test_output(js_test_vm.void(), ["foobar", "foobar"], (stream) => {
            stream.write_string(js_test_vm.str("foo"));
            stream.write_string(js_test_vm.str("bar"));
            stream.force_output();
            stream.write_string(js_test_vm.str("foo"));
            stream.write_string(js_test_vm.str("bar"));
            return stream.force_output();
        });
        test_output(js_test_vm.void(), ["foo\nbar", "foobar"], (stream) => {
            stream.write_string(js_test_vm.str("foo"));
            assert.deepEqual(stream.fresh_line(), js_test_vm.t());
            assert.deepEqual(stream.fresh_line(), js_test_vm.f());
            stream.write_string(js_test_vm.str("bar"));
            stream.force_output();
            stream.write_string(js_test_vm.str("foo"));
            stream.write_string(js_test_vm.str("bar"));
            return stream.force_output();
        });
        test_output(js_test_vm.void(), ["\n"], (stream) => {
            stream.write_string(js_test_vm.str("\n"));
            return stream.force_output();
        });
        test_output(js_test_vm.void(), ["\n"], (stream) => {
            stream.write_string(js_test_vm.str("\n"));
            assert.deepEqual(stream.fresh_line(), js_test_vm.f());
            return stream.force_output();
        });
        test_output(js_test_vm.void(), ["\nfoo\nbar", "quux"], (stream) => {
            stream.write_string(js_test_vm.str("\n"));
            assert.deepEqual(stream.fresh_line(), js_test_vm.f());
            assert.deepEqual(js_test_vm.str("foo"), stream.write_string(js_test_vm.str("foo")));
            stream.write_string(js_test_vm.str("\n"));
            assert.deepEqual(stream.fresh_line(), js_test_vm.f());
            assert.deepEqual(js_test_vm.str("bar"), stream.write_string(js_test_vm.str("bar")));
            stream.force_output();
            assert.deepEqual(js_test_vm.str("quux"), stream.write_string(js_test_vm.str("quux")));
            return stream.force_output();
        });

    });

});

;// CONCATENATED MODULE: ./test/all-tests.mjs










})();

/******/ 	return __webpack_exports__;
/******/ })()
;
});