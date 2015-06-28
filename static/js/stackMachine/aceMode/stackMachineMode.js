define('ace/mode/stackMachine_highlight_rules', ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function (require, exports, module) {

    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

    var StackMachineHighlightRules = function () {

        this.$rules = {
            "start": [
                {token: "support.function", regex: "pushK ", next: "char"},
                {token: "support.function", regex: "push ", next: "number"},
                {token: "support.function", regex: "slide ", next: "slideInput"},
                {token: "support.function", regex: /pop$/},
                {token: "keyword.operator", regex: /\+$/},
                {token: "keyword.operator", regex: /\-/},
                {token: "keyword.operator", regex: /\*/},
                {token: "keyword.operator", regex: /\//},
                {token: "support.type", regex: /print$/},
                {token: "string.quoted.double.bold", regex: /[a-zA-Z_]*[0-9]*\.$/},
                {token: "support.function", regex: "branchz ", next: "word"},
                {token: "support.function", regex: "jump ", next: "word"},
                {token: "comment.line.number-sign", regex: /#.*/},
                {token: "support.constant", regex: /break$/},
                {defaultToken: "invalid.illegal"}
            ],
            "word": [
                {token: "string.quoted.double.bold", regex: /[a-zA-Z_]*[0-9]*$/, next: "start"},
                {defaultToken: "invalid.illegal"}
            ],
            "slideInput": [
                {token: "constant.numeric.bold", regex: /[0-9]\s[0-9]+$/, next: "start"},
                {defaultToken: "invalid.illegal"}
            ],
            "number": [
                {token: "constant.numeric.bold", regex: /[\-]?[0-9]+$/, next: "start"},
                {defaultToken: "invalid.illegal"}
            ],
            "char": [
                {token: "constant.numeric.bold", regex: /[\-]?[0-9]*$/, next: "start"},
                {token: "string.quoted.double.bold", regex: /[a-zA-Z0-9]*$/, next: "start"},
                {defaultToken: "invalid.illegal"}
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