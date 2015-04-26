define('ace/mode/stackMachine_highlight_rules', ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function (require, exports, module) {

    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

    var StackMachineHighlightRules = function () {

        this.$rules = {
            "start": [
                {token: "storage.type", regex: "pushK ", next: "char"},
                {token: "storage.type", regex: "push ", next: "number"},
                {token: "storage.type", regex: "slide ", next: "slideInput"},
                {token: "storage.type", regex: /pop$/},
                {token: "keyword.operator", regex: /\+$/},
                {token: "keyword.operator", regex: /\-/},
                {token: "keyword.operator", regex: /\*/},
                {token: "keyword.operator", regex: /\//},
                {token: "storage.type", regex: /print$/},
                {token: "keyword.control", regex: /[a-zA-Z_]*\.$/},
                {token: "keyword.control", regex: "branchz ", next: "word"},
                {token: "keyword.control", regex: "jump ", next: "word"},
                {defaultToken: "invalid"}
            ],
            "word": [
                {token: "string.quoted.double.bold", regex: /[a-zA-Z_]*$/, next: "start"},
                {defaultToken: "invalid"}
            ],
            "slideInput": [
                {token: "constant.numeric.bold", regex: /[0-9]\s[0-9]+$/, next: "start"},
                {defaultToken: "invalid"}
            ],
            "number": [
                {token: "constant.numeric.bold", regex: /[0-9]+$/, next: "start"},
                {defaultToken: "invalid"}
            ],
            "char": [
                {token: "constant.numeric.bold", regex: /[0-9]*$/, next: "start"},
                {token: "string.qouted.double.bold", regex: /.{1}$/, next: "start"},
                {defaultToken: "invalid"}
            ]
        };

    };

    oop.inherits(StackMachineHighlightRules, TextHighlightRules);

    exports.StackMachineHighlightRules = StackMachineHighlightRules;
});

define('ace/mode/stackMachine', ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/mode/stackMachine_highlight_rules"], function (require, exports, module) {

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var StackMachineHighlightRules = require("./stackMachine_highlight_rules").StackMachineHighlightRules;

    var Mode = function () {
        this.HighlightRules = StackMachineHighlightRules;
    };
    oop.inherits(Mode, TextMode);

    (function () {
        this.$id = "ace/mode/stackMachine"
    }).call(Mode.prototype);

    exports.Mode = Mode;
});