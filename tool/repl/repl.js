$(function() {
    const vm = new window["lispx-vm"].VM();
    const term = $('#terminal').terminal({}, {
        greetings: vm.write_to_js_string(vm.eval_js_string(`"Welcome to Nybble Lisp!"`))
    });
});
