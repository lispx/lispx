import { VM } from "../../dist/lispx-vm.min.mjs";
import { init_repl_stream } from "./repl_stream.mjs";

const REPL_CODE = `
(defconstant repl:+root-prompt+ 'repl:root-prompt
  "The prompt used for blocking on IO.")

(defconstant repl:+environment+ (the-environment)
  "The environment in which REPL expressions are evaluated.")

(defun invoke-debugger (condition)
  (take-subcont +root-prompt+ k
    (push-delim-subcont +root-prompt+ k
      (typecase condition
        (unbound-symbol-error
         (let ((symbol (slot-value condition 'symbol))
               (env (slot-value condition 'environment)))
           (restart-case ((continue (lambda () (eval symbol env)))
                          (use-value (lambda (value) value))
                          (store-value (lambda (value) (eval (list #'def symbol value) env))))
             (repl:run-debugger-loop condition k))))
        (object
         (repl:run-debugger-loop condition k))))))

(defun repl:run-debugger-loop (condition k)
  (uprint "Debugger invoked on condition:")
  (print condition)
  (uprint "Available restarts:")
  (mapc (lambda (restart) (print (slot-value restart 'restart-name)))
        (compute-restarts condition))
  (uprint "Backtrace:")
  (%print-stacktrace k)
  (loop
    (fresh-line)
    (print (eval (read) repl:+environment+))))

(defun repl:run ()
  "Run the REPL."
  (push-prompt repl:+root-prompt+
    (loop
      (restart-case ((abort (lambda ())))
        (fresh-line)
        (print (eval (read) repl:+environment+))))))

(defmethod stream-read ((stream repl:input-buffer) . #ignore)
  "Blocking input function for the REPL input buffer.  This gets
called from 'read' when the input stream is a 'repl:input-buffer'.

Calls the built-in default implementation of 'read', which is
non-blocking, i.e. immediately signals 'end-of-file' instead of
waiting for more input.

Tries the built-in 'read' on the current contents of the input buffer.
If it successfully reads an object, return it.  If it signals
'end-of-file', save the current continuation in the input buffer's
wake-up function.  The wake-up function will be called when more input
is available, in which case we start over.

Note that the method ignores the generic function's 'eof-error-p' and
'eof-value' parameters because it always blocks on EOF.

To sum up, this method turns 'read' from non-blocking into blocking
when the input stream is a 'repl:input-buffer'."
  (block exit
    (loop
      ;; Use a trampoline to avoid stack build-up.
      ((block trampoline
         ;; Get a stream containing the current contents of the input buffer.
         (let ((stream (repl:%make-input-buffer-stream (dynamic *standard-input*))))
           (handler-case ((end-of-file
                           (lambda #ignore
                             ;; We got an end-of-file error: jump into trampoline
                             ;; and save continuation in wake-up function.
                             (return-from trampoline
                               (lambda ()
                                 (take-subcont repl:+root-prompt+ k
                                   (repl:%set-input-buffer-wake-up-function
                                    (dynamic *standard-input*)
                                    (lambda ()
                                      (push-delim-subcont repl:+root-prompt+ k)))))))))
               ;; Call built-in, non-blocking 'read' on the stream.
               ;; This calls the built-in because the stream is a
               ;; 'string-input-stream'.
               (let ((form (read stream)))
                 ;; We've successfully read an object: remove the
                 ;; input we've consumed from the input buffer.
                 (repl:%truncate-input-buffer (dynamic *standard-input*) stream)
                 (return-from exit form)))))))))
`;

$(function() {

    const vm = new VM();
    init_repl_stream(vm);

    const term = $('#terminal').terminal(input_handler, {
        greetings: vm.write_to_js_string(vm.eval_js_string(`"Welcome to Nybble Lisp!"`))
    });

    const stdout = new vm.REPL_output_stream(function (output) {
        term.echo(output.to_js_string(), { newline: false });
    });
    vm.STANDARD_OUTPUT.set_value(stdout);

    const stdin = new vm.REPL_input_buffer();
    vm.STANDARD_INPUT.set_value(stdin);

    function input_handler(line)
    {
        stdin.add_line(vm.str(line + "\n"));
        /*
         * Force any available output after each input.
         */
        stdout.force_output();
    }

    vm.eval_js_string(REPL_CODE);
    vm.eval_js_string("(repl:run)");

});
