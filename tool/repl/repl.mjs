import { VM } from "../dist/lispx-vm.mjs";

const vm = new VM();

vm.eval_js_string(`
(def #'prompt (to-lisp-function (js-global "prompt")))

(defclass deno:input-stream () ())

(defmethod stream-read ((s deno:input-stream) #ignore #ignore)
  (read (make-string-input-stream (to-lisp-string (prompt (to-js-string "*"))))))

(dynamic-let ((*standard-input* (make-instance (class deno:input-stream))))
  (uprint "Warning: this REPL supports only one expression per line.")
  (loop (print (eval (read) (the-environment)))))
`);
