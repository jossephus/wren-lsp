; Keywords
[
  "class"
  "construct"
  "foreign"
  "static"
  "var"
  "import"
  "for"
  "in"
  "if"
  "else"
  "while"
  "return"
  "is"
  "as"
  "super"
] @keyword

(this) @keyword
(break_statement) @keyword
(continue_statement) @keyword

; Literals
(string) @string
(raw_string) @string
(number) @number
(boolean) @boolean
(null) @constant.builtin

; Comments
(line_comment) @comment
(block_comment) @comment

; Operators
[
  "="
  "?"
  ":"
  "+"
  "-"
  "*"
  "/"
  "%"
  ".."
  "..."
  "<<"
  ">>"
  "&"
  "^"
  "|"
  "<"
  "<="
  ">"
  ">="
  "=="
  "!="
  "&&"
  "||"
  "!"
  "~"
] @operator

; Punctuation
["(" ")"] @punctuation.bracket
["[" "]"] @punctuation.bracket
["{" "}"] @punctuation.bracket
["," "."] @punctuation.delimiter

; Types
(class_definition
  name: (identifier) @type)

; Constructors and methods
(constructor_definition
  name: (identifier) @constructor)

(named_method
  name: (identifier) @function.method)

(getter_definition
  name: (identifier) @function.method)

(setter_definition
  name: (identifier) @function.method)

(foreign_named_method
  name: (identifier) @function.method)

(foreign_getter
  name: (identifier) @function.method)

; Calls
(call_expression
  target: (identifier) @function)

(method_call_expression
  method: (identifier) @function.method)

(super_expression
  method: (identifier) @function.method)

; Variables and params
(var_statement
  name: (identifier) @variable)

(parameter_list
  (identifier) @variable.parameter)

(closure_parameter_list
  (identifier) @variable.parameter)

(subscript_parameter_list
  (identifier) @variable.parameter)

(import_variable
  name: (identifier) @variable)

(import_variable
  alias: (identifier) @variable)

; Fields
(field) @property
(static_field) @property

; Attributes
(attribute
  name: (identifier) @attribute)

(attribute_entry
  key: (identifier) @attribute)

; Imports
(import_statement
  path: (string) @string.special)

; Fallback
(identifier) @variable
