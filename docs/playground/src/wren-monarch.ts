// Wren language definition for Monaco Editor (Monarch tokenizer)

export const wrenLanguageDef = {
  keywords: [
    "break", "class", "construct", "continue", "else", "false", "for",
    "foreign", "if", "import", "in", "is", "null", "return", "static",
    "super", "this", "true", "var", "while",
  ],

  builtins: [
    "Bool", "Class", "Fiber", "Fn", "List", "Map", "Null", "Num",
    "Object", "Range", "Sequence", "String", "System",
  ],

  tokenizer: {
    root: [
      [/\/\*/, "comment", "@comment"],
      [/\/\/.*$/, "comment"],

      [/"/, "string", "@string"],

      [/0x[0-9a-fA-F]+/, "number"],
      [/\d+(\.\d+)?([eE][+-]?\d+)?/, "number"],

      [/__[a-zA-Z_]\w*/, "variable.field"],
      [/_[a-zA-Z_]\w*/, "variable.field"],

      [/[A-Z]\w*/, {
        cases: {
          "@builtins": "type",
          "@default": "type",
        },
      }],

      [/[a-z_]\w*/, {
        cases: {
          "@keywords": "keyword",
          "@default": "identifier",
        },
      }],

      [/[+\-*\/%&|^~<>=!]+/, "operator"],
      [/[{}()\[\]]/, "@brackets"],
      [/\s+/, "white"],
    ],

    comment: [
      [/\/\*/, "comment", "@push"],
      [/\*\//, "comment", "@pop"],
      [/./, "comment"],
    ],

    string: [
      [/\\./, "string.escape"],
      [/"/, "string", "@pop"],
      [/./, "string"],
    ],
  },
};

export const wrenLanguageConfig = {
  comments: {
    lineComment: "//",
    blockComment: ["/*", "*/"],
  },
  brackets: [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
  ],
  autoClosingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
    { open: '"', close: '"' },
  ],
  surroundingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
    { open: '"', close: '"' },
  ],
};
