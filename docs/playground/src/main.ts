/* eslint-disable @typescript-eslint/no-explicit-any */
import loader from "@monaco-editor/loader";
import {
  initWasm,
  isReady,
  parse,
  complete,
  definition,
  references,
  hover,
  documentSymbols,
  setVirtualFile,
} from "./wasm-bridge";
import { wrenLanguageDef, wrenLanguageConfig } from "./wren-monarch";

let monaco: any;

interface File {
  id: string;
  uri: string;
  language: string;
  content: string;
}

const defaultFileIds = new Set(["main.wren", "dome.wren", "wren-lsp.json", "resolver.js"]);

const files: File[] = [
  {
    id: "main.wren",
    uri: "file:///playground/main.wren",
    language: "wren",
    content: ` 
System.print("Hello World")
// a = 1 // un comment this to see lsp error

`,
  },
  {
    id: "dome.wren",
    uri: "file:///playground/dome.wren",
    language: "wren",
    content: `/// The wren-lsp.json configuration file and resolver.js enable custom
/// import resolution mechanisms. The resolver.js below uses GitHub links to resolve
/// dome modules. This works because wren-lsp configuration is host-agnostic.
///
/// for this to work on wasm, i am doing a little bit too much work, but its in order to
/// demonstrate what's possible.

import "input" for Keyboard
import "graphics" for Canvas, Color

class Main {
  construct new() {}

  init() {
    _x = 10
    _y = 10
    _w = 5
    _h = 5
  }

  update() {
    if (Keyboard.isKeyDown("left")) {
      _x = _x - 1
    }
    if (Keyboard.isKeyDown("right")) {
      _x = _x+ 1
    }
    if (Keyboard.isKeyDown("up")) {
      _y = _y - 1
    }
    if (Keyboard.isKeyDown("down")) {
      _y = _y + 1
    }
  }

  draw(alpha) {
    Canvas.cls()
    var color = Color.rgb(171, 82, 54)
    Canvas.rectfill(_x, _y, _w, _h, color)
  }
}

var Game = Main.new()
`,
  },
  {
    id: "wren-lsp.json",
    uri: "file:///playground/wren-lsp.json",
    language: "json",
    content: `{
  "version": 1,
  "resolvers": [
    {
      "type": "host",
      "script": "./resolver.js",
      "fallbackToBuiltin": true
    },
    {
      "type": "path",
      "roots": ["./"],
      "delimiter": "/"
    }
  ]
}`,
  },
  {
    id: "resolver.js",
    uri: "file:///playground/resolver.js",
    language: "javascript",
    content: `const REPO_OWNER = "domeengine"
const REPO_NAME = "dome"
const REPO_REF = "main"
const REPO_API_BASE = "https://api.github.com/repos/" + REPO_OWNER + "/" + REPO_NAME
const REPO_RAW_BASE = "https://raw.githubusercontent.com/" + REPO_OWNER + "/" + REPO_NAME + "/" + REPO_REF
const MODULE_ROOTS = ["src/modules", "src/bindings", "src"]

const DOME_SCHEME = "dome://"
const domeModules = new Map()

const stripWrenExtension = (path) => path.endsWith(".wren") ? path.slice(0, -5) : path

const normalizePath = (path) => {
  const out = []
  for (const part of path.split("/")) {
    if (!part || part == ".") continue
    if (part == "..") {
      if (out.length > 0) out.pop()
      continue
    }
    out.push(part)
  }
  return out.join("/")
}

const candidateKeys = (importString) => {
  const raw = stripWrenExtension(importString)
  const slash = raw.replaceAll(".", "/")
  const keys = new Set([raw, slash])
  for (const root of MODULE_ROOTS) {
    keys.add(root + "/" + raw)
    keys.add(root + "/" + slash)
  }
  if (raw.startsWith("dome/")) keys.add(raw.slice("dome/".length))
  if (slash.startsWith("dome/")) keys.add(slash.slice("dome/".length))
  return [...keys].map(normalizePath).filter((k) => k.length > 0)
}

export async function preloadDomeModules() {
  if (domeModules.size > 0) return
  const treeUrl = REPO_API_BASE + "/git/trees/" + REPO_REF + "?recursive=1"
  const treeResp = await fetch(treeUrl)
  if (!treeResp.ok) return
  const tree = await treeResp.json()

  const files = (tree.tree || [])
    .filter((entry) => entry.type == "blob" && entry.path.endsWith(".wren"))
    .map((entry) => entry.path)

  await Promise.all(files.map(async (path) => {
    const rawUrl = REPO_RAW_BASE + "/" + path
    const resp = await fetch(rawUrl)
    if (!resp.ok) return
    const source = await resp.text()
    const key = stripWrenExtension(path)
    const uri = DOME_SCHEME + key
    domeModules.set(key, {
      canonical_id: uri,
      uri,
      source,
      kind: "virtual",
    })
  }))
}

export function resolveHostImport(importerUri, importString) {
  if ((importString.startsWith("./") || importString.startsWith("../")) && importerUri.startsWith(DOME_SCHEME)) {
    const importerPath = importerUri.slice(DOME_SCHEME.length)
    const slash = importerPath.lastIndexOf("/")
    const baseDir = slash >= 0 ? importerPath.slice(0, slash) : ""
    const relative = normalizePath(baseDir + "/" + stripWrenExtension(importString))
    return domeModules.get(relative) || null
  }

  for (const key of candidateKeys(importString)) {
    const module = domeModules.get(key)
    if (module) return module
  }
  return null
}`,
  },
];

let active_file_id = files[0].id;
let editor: any = null;
const models = new Map<string, any>();

// --- Run worker ---
let runWorker: Worker | null = null;
let runTokenCounter = 0;
const runPending = new Map<number, { resolve: (value: { output: string; errors: string; result: number }) => void }>();

function initRunWorker() {
  runWorker = new Worker(new URL("./run-worker.ts", import.meta.url), { type: "module" });
  runWorker.onmessage = (event) => {
    const { token, output, errors, result } = event.data;
    const pending = runPending.get(token);
    if (!pending) return;
    runPending.delete(token);
    pending.resolve({ output, errors, result });
  };
}

function runCode(source: string): Promise<{ output: string; errors: string; result: number }> {
  if (!runWorker) initRunWorker();
  return new Promise((resolve) => {
    const token = ++runTokenCounter;
    runPending.set(token, { resolve });
    runWorker!.postMessage({ token, source });
  });
}

const runBtn = document.getElementById("run-btn") as HTMLButtonElement;
const outputEl = document.getElementById("output") as HTMLDivElement;

function renderOutput(output: string, errors: string) {
  let html = "";
  if (output) {
    html += '<span class="output-text">' + escapeHtml(output) + "</span>";
  }
  if (errors) {
    html += '<span class="output-error">' + escapeHtml(errors) + "</span>";
  }
  if (!output && !errors) {
    html = '<span class="output-hint">No output</span>';
  }
  outputEl.innerHTML = html;
}

async function handleRun() {
  if (!editor || !isActiveWren()) return;

  runBtn.disabled = true;
  outputEl.innerHTML = '<span class="output-running">Running…</span>';

  try {
    const { output, errors } = await runCode(editor.getValue());
    renderOutput(output, errors);
  } catch (e) {
    outputEl.innerHTML = '<span class="output-error">' + escapeHtml((e as Error).message) + "</span>";
  } finally {
    runBtn.disabled = false;
  }
}

runBtn.addEventListener("click", handleRun);

function activeFile(): File {
  return files.find((f) => f.id === active_file_id) || files[0];
}

function isActiveWren(): boolean {
  return activeFile().language === "wren";
}

// --- Monaco theme ---
function defineTheme() {
  monaco.editor.defineTheme("wren-dark", {
    base: "vs-dark",
    inherit: true,
    rules: [
      { token: "keyword", foreground: "c586c0" },
      { token: "type", foreground: "4ec9b0" },
      { token: "identifier", foreground: "9cdcfe" },
      { token: "string", foreground: "ce9178" },
      { token: "string.escape", foreground: "d7ba7d" },
      { token: "number", foreground: "b5cea8" },
      { token: "comment", foreground: "6a9955" },
      { token: "operator", foreground: "d4d4d4" },
      { token: "variable.field", foreground: "9cdcfe", fontStyle: "italic" },
    ],
    colors: {
      "editor.background": "#111111",
      "editor.foreground": "#e0e0e0",
      "editorGutter.background": "#0d0d0d",
      "editorLineNumber.foreground": "#404040",
      "editorLineNumber.activeForeground": "#808080",
      "editor.lineHighlightBackground": "#1a1a1a",
      "editor.selectionBackground": "#264f78",
      "editorCursor.foreground": "#e0e0e0",
      "editorWidget.background": "#1a1a1a",
      "editorWidget.border": "#333333",
      "editorSuggestWidget.background": "#1a1a1a",
      "editorSuggestWidget.border": "#333333",
      "editorSuggestWidget.selectedBackground": "#264f78",
      "editorHoverWidget.background": "#1a1a1a",
      "editorHoverWidget.border": "#333333",
    },
  });
}

// --- Diagnostics panel ---
const diagnosticsEl = document.getElementById("diagnostics") as HTMLDivElement;
const statusEl = document.getElementById("status") as HTMLDivElement;

function updateStatus(text: string, ok: boolean) {
  statusEl.textContent = text;
  statusEl.className = ok ? "status status-ok" : "status";
}

function escapeHtml(s: string): string {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

interface Diagnostic {
  line: number;
  colStart: number;
  colEnd: number;
  severity: string;
  message: string;
}

function renderDiagnosticsPanel(diags: Diagnostic[]) {
  if (!isActiveWren()) {
    diagnosticsEl.innerHTML = '<span class="diag-hint">Diagnostics available for .wren files only</span>';
    updateStatus("Viewing non-Wren file", true);
    return;
  }

  if (!diags || diags.length === 0) {
    diagnosticsEl.innerHTML = '<span class="diag-hint">No issues found ✓</span>';
    updateStatus("No issues", true);
    return;
  }

  updateStatus(diags.length + " issue" + (diags.length !== 1 ? "s" : ""), false);

  diagnosticsEl.innerHTML = diags.map((d) => {
    const cls = "diag-" + d.severity;
    const loc = (d.line + 1) + ":" + d.colStart;
    return (
      '<div class="diag">' +
      '<span class="diag-location">' + loc + "</span> " +
      '<span class="' + cls + '">' +
      escapeHtml(d.severity) + ": " + escapeHtml(d.message) +
      "</span></div>"
    );
  }).join("");
}

async function updateDiagnostics() {
  if (!isReady() || !editor) return;

  if (!isActiveWren()) {
    monaco.editor.setModelMarkers(editor.getModel(), "wren-lsp", []);
    renderDiagnosticsPanel([]);
    return;
  }

  const source = editor.getValue();
  const result = await parse(source, activeFile().uri);
  const diags = result.diagnostics;

  const markers = diags.map((d: Diagnostic) => ({
    severity: d.severity === "error" ? monaco.MarkerSeverity.Error :
      d.severity === "warning" ? monaco.MarkerSeverity.Warning :
        d.severity === "info" ? monaco.MarkerSeverity.Info :
          monaco.MarkerSeverity.Hint,
    message: d.message,
    startLineNumber: d.line + 1,
    startColumn: d.colStart + 1,
    endLineNumber: d.line + 1,
    endColumn: d.colEnd + 1,
  }));

  monaco.editor.setModelMarkers(editor.getModel(), "wren-lsp", markers);
  renderDiagnosticsPanel(diags);
}

// --- Helpers ---
function hoverContentsToText(contents: any): string {
  const unwrapUnion = (value: any) => {
    if (!value || typeof value !== "object" || Array.isArray(value)) return value;
    const keys = Object.keys(value);
    if (keys.length !== 1) return value;
    return value[keys[0]];
  };

  const toText = (value: any): string => {
    const unwrapped = unwrapUnion(value);
    if (!unwrapped) return "";
    if (typeof unwrapped === "string") return unwrapped;
    if (Array.isArray(unwrapped)) {
      return unwrapped.map(toText).filter(Boolean).join("\n");
    }
    if (typeof unwrapped === "object") {
      if (typeof unwrapped.value === "string") return unwrapped.value;
      if (typeof unwrapped.kind === "string" && typeof unwrapped.value === "string") return unwrapped.value;
      return toText(unwrapUnion(unwrapped));
    }
    return "";
  };

  if (!contents) return "";
  return toText(contents);
}

interface Location {
  uri: string;
  range: {
    start: { line: number; character: number };
    end: { line: number; character: number };
  };
}

function firstLocation(result: any): Location | null {
  if (!result) return null;
  if (Array.isArray(result)) return result.length > 0 ? result[0] : null;
  if (result.uri && result.range) return result;
  if (Array.isArray(result.targets) && result.targets.length > 0) {
    return {
      uri: result.targets[0].uri,
      range: result.targets[0].targetSelectionRange || result.targets[0].targetRange,
    };
  }
  return null;
}

// --- LSP providers ---
function registerProviders() {
  monaco.languages.registerCompletionItemProvider("wren", {
    triggerCharacters: ["."],
    provideCompletionItems: async (model: any, position: any) => {
      if (!isReady()) return { suggestions: [] };

      const source = model.getValue();
      const offset = model.getOffsetAt(position);

      const wordInfo = model.getWordUntilPosition(position);
      const lineContent = model.getLineContent(position.lineNumber);
      const charBeforeWord = lineContent[wordInfo.startColumn - 2];
      const isDot = charBeforeWord === ".";
      const triggerChar = isDot ? "." : undefined;

      const items = await complete(source, offset, triggerChar, activeFile().uri);

      const filtered = items.filter((item: any) => {
        if (isDot) return item.kind === "method" || item.kind === "function";
        return item.kind !== "method" && item.kind !== "function";
      });

      const range = {
        startLineNumber: position.lineNumber,
        startColumn: wordInfo.startColumn,
        endLineNumber: position.lineNumber,
        endColumn: wordInfo.endColumn,
      };

      const kindMap: Record<string, any> = {
        class: monaco.languages.CompletionItemKind.Class,
        method: monaco.languages.CompletionItemKind.Method,
        function: monaco.languages.CompletionItemKind.Function,
        variable: monaco.languages.CompletionItemKind.Variable,
        field: monaco.languages.CompletionItemKind.Field,
        keyword: monaco.languages.CompletionItemKind.Keyword,
        text: monaco.languages.CompletionItemKind.Text,
        constructor: monaco.languages.CompletionItemKind.Constructor,
        module: monaco.languages.CompletionItemKind.Module,
        interface: monaco.languages.CompletionItemKind.Interface,
      };

      return {
        suggestions: filtered.map((item: any) => ({
          label: item.label,
          kind: kindMap[item.kind] || monaco.languages.CompletionItemKind.Text,
          insertText: item.label,
          range,
        })),
      };
    },
  });

  monaco.languages.registerHoverProvider("wren", {
    provideHover: async (model: any, position: any) => {
      if (!isReady()) return null;

      const source = model.getValue();
      const offset = model.getOffsetAt(position);
      const result = await hover(source, offset, activeFile().uri);

      const text = hoverContentsToText((result as any)?.contents);
      if (!text) return null;

      const word = model.getWordAtPosition(position);
      const range = word ? {
        startLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endLineNumber: position.lineNumber,
        endColumn: word.endColumn,
      } : undefined;

      return {
        range,
        contents: [{ value: "```\n" + text + "\n```" }],
      };
    },
  });

  monaco.languages.registerDefinitionProvider("wren", {
    provideDefinition: async (model: any, position: any) => {
      if (!isReady()) return null;

      const source = model.getValue();
      const offset = model.getOffsetAt(position);
      const result = await definition(source, offset, activeFile().uri);

      const loc = firstLocation(result);
      if (!loc || loc.uri !== activeFile().uri) return null;

      return {
        uri: model.uri,
        range: {
          startLineNumber: loc.range.start.line + 1,
          startColumn: loc.range.start.character + 1,
          endLineNumber: loc.range.end.line + 1,
          endColumn: loc.range.end.character + 1,
        },
      };
    },
  });

  monaco.languages.registerReferenceProvider("wren", {
    provideReferences: async (model: any, position: any) => {
      if (!isReady()) return [];

      const source = model.getValue();
      const offset = model.getOffsetAt(position);
      const result = await references(source, offset, activeFile().uri);

      const refs = Array.isArray(result) ? result : [];
      return refs
        .filter((r: any) => r.uri === activeFile().uri)
        .map((r: any) => ({
          uri: model.uri,
          range: {
            startLineNumber: r.range.start.line + 1,
            startColumn: r.range.start.character + 1,
            endLineNumber: r.range.end.line + 1,
            endColumn: r.range.end.character + 1,
          },
        }));
    },
  });

  monaco.languages.registerDocumentSymbolProvider("wren", {
    provideDocumentSymbols: async (model: any): Promise<any[]> => {
      if (!isReady()) return [];

      const source = model.getValue();
      const result = await documentSymbols(source, activeFile().uri);

      const symbols = Array.isArray(result) ? result : [];
      const kindMap: Record<number, any> = {
        5: monaco.languages.SymbolKind.Class,
        6: monaco.languages.SymbolKind.Method,
        12: monaco.languages.SymbolKind.Function,
        13: monaco.languages.SymbolKind.Variable,
      };

      return symbols.map((s: any) => ({
        name: s.name,
        kind: kindMap[s.kind] || monaco.languages.SymbolKind.Variable,
        range: {
          startLineNumber: (s.range?.start?.line ?? 0) + 1,
          startColumn: (s.range?.start?.character ?? 0) + 1,
          endLineNumber: (s.range?.end?.line ?? 0) + 1,
          endColumn: (s.range?.end?.character ?? 0) + 1,
        },
        selectionRange: {
          startLineNumber: (s.selectionRange?.start?.line ?? s.range?.start?.line ?? 0) + 1,
          startColumn: (s.selectionRange?.start?.character ?? s.range?.start?.character ?? 0) + 1,
          endLineNumber: (s.selectionRange?.end?.line ?? s.range?.end?.line ?? 0) + 1,
          endColumn: (s.selectionRange?.end?.character ?? s.range?.end?.character ?? 0) + 1,
        },
      }));
    },
  });
}

// --- File switching ---
function createModels() {
  for (const file of files) {
    const uri = monaco.Uri.parse(file.uri);
    const model = monaco.editor.createModel(file.content, file.language, uri);
    models.set(file.id, model);

    model.onDidChangeContent(() => {
      file.content = model.getValue();
      void setVirtualFile(file.uri, file.content);

      if (file.id === active_file_id) {
        if (debounce) clearTimeout(debounce);
        debounce = setTimeout(() => updateDiagnostics(), 300);
      }
    });
  }
}

let debounce: ReturnType<typeof setTimeout> | null = null;

async function addFile(name: string) {
  if (!name.endsWith(".wren")) name += ".wren";
  if (files.some((f) => f.id === name)) return;

  const file: File = {
    id: name,
    uri: "file:///playground/" + name,
    language: "wren",
    content: "",
  };
  files.push(file);

  await setVirtualFile(file.uri, file.content);

  const uri = monaco.Uri.parse(file.uri);
  const model = monaco.editor.createModel(file.content, file.language, uri);
  models.set(file.id, model);

  model.onDidChangeContent(() => {
    file.content = model.getValue();
    void setVirtualFile(file.uri, file.content);
    if (file.id === active_file_id) {
      if (debounce) clearTimeout(debounce);
      debounce = setTimeout(() => updateDiagnostics(), 300);
    }
  });

  switchFile(file.id);
}

function deleteFile(fileId: string) {
  if (defaultFileIds.has(fileId)) return;

  const idx = files.findIndex((f) => f.id === fileId);
  if (idx < 0) return;

  files.splice(idx, 1);
  const model = models.get(fileId);
  if (model) {
    model.dispose();
    models.delete(fileId);
  }

  if (active_file_id === fileId) {
    switchFile(files[0].id);
  } else {
    renderFileList();
  }
}

function promptNewFile() {
  const name = prompt("File name (e.g. utils.wren):");
  if (!name || !name.trim()) return;
  void addFile(name.trim());
}

const configFileIds = new Set(["wren-lsp.json", "resolver.js"]);

function renderFileList() {
  const tabsBar = document.getElementById("tabs-bar") as HTMLDivElement;
  const configBar = document.getElementById("config-tabs-bar") as HTMLDivElement;
  const addBtn = document.getElementById("tab-add-btn") as HTMLButtonElement;

  // Remove existing tabs (keep add button)
  const existingTabs = tabsBar.querySelectorAll(".tab");
  for (let i = 0; i < existingTabs.length; i++) {
    existingTabs[i].remove();
  }
  configBar.innerHTML = "";

  for (const file of files) {
    const isConfig = configFileIds.has(file.id);
    const tab = document.createElement("button");
    tab.className = "tab" + (file.id === active_file_id ? " active" : "");
    tab.textContent = file.id;
    tab.addEventListener("click", () => switchFile(file.id));

    if (!defaultFileIds.has(file.id)) {
      const close = document.createElement("span");
      close.className = "tab-close";
      close.textContent = "×";
      close.title = "Delete " + file.id;
      close.addEventListener("click", (e) => {
        e.stopPropagation();
        deleteFile(file.id);
      });
      tab.appendChild(close);
    }

    if (isConfig) {
      configBar.appendChild(tab);
    } else {
      tabsBar.insertBefore(tab, addBtn);
    }
  }
}

async function switchFile(fileId: string) {
  if (!editor || fileId === active_file_id) return;
  const next = files.find((f) => f.id === fileId);
  if (!next) return;

  active_file_id = fileId;
  editor.setModel(models.get(fileId));
  runBtn.disabled = !isActiveWren();
  renderFileList();
  await updateDiagnostics();
}

// --- Initialize ---
async function init() {
  monaco = await loader.init();

  // Register Wren language
  monaco.languages.register({ id: "wren" });
  monaco.languages.setMonarchTokensProvider("wren", wrenLanguageDef as any);
  monaco.languages.setLanguageConfiguration("wren", wrenLanguageConfig as any);

  defineTheme();
  createModels();
  registerProviders();

  editor = monaco.editor.create(document.getElementById("editor") as HTMLElement, {
    model: models.get(active_file_id),
    theme: "wren-dark",
    minimap: { enabled: false },
    scrollBeyondLastLine: false,
    fontSize: 13,
    fontFamily: 'ui-monospace, "SF Mono", Menlo, monospace',
    lineHeight: 20,
    automaticLayout: true,
    tabSize: 2,
    renderLineHighlight: "line",
    scrollbar: {
      verticalScrollbarSize: 8,
      horizontalScrollbarSize: 8,
    },
  });

  editor.addAction({
    id: "wren-run",
    label: "Run Wren Code",
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
    run: handleRun,
  });

  // Setup add tab button
  const addBtn = document.getElementById("tab-add-btn") as HTMLButtonElement;
  addBtn.addEventListener("click", promptNewFile);

  try {
    await initWasm();

    for (const file of files) {
      await setVirtualFile(file.uri, file.content);
    }

    runBtn.disabled = !isActiveWren();
    updateStatus("Ready", true);
  } catch (e) {
    updateStatus("Failed to load WASM: " + (e as Error).message, false);
    console.error(e);
  }

  renderFileList();
  await updateDiagnostics();
}

init();
