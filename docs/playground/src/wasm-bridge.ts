let worker: Worker | null = null;
let initialized = false;
const documents = new Map<string, { source: string; version: number; opened: boolean }>();
const diagnostics_by_uri = new Map<string, Array<{
  line: number;
  colStart: number;
  colEnd: number;
  severity: string;
  message: string;
}>>();

let next_token = 1;
let next_id = 1;

const pending_tokens = new Map<number, { resolve: (value: string[]) => void; reject: (reason: Error) => void }>();
const default_document_uri = "file:///playground/main.wren";

function toDiagSeverity(severity: number): string {
  if (severity === 1) return "error";
  if (severity === 2) return "warning";
  if (severity === 3) return "info";
  return "hint";
}

function convertDiagnostics(params: { uri: string; diagnostics: Array<{ range: { start: { line: number; character: number }; end: { line: number; character: number } }; severity?: number; message?: string }> }) {
  if (!params || typeof params.uri !== "string" || !Array.isArray(params.diagnostics)) {
    return;
  }

  diagnostics_by_uri.set(params.uri, params.diagnostics.map((d) => ({
    line: d.range.start.line,
    colStart: d.range.start.character,
    colEnd: d.range.end.character,
    severity: toDiagSeverity(d.severity ?? 3),
    message: d.message ?? "",
  })));
}

function normalizeMessages(outputs: string[]) {
  const messages: object[] = [];
  for (const raw of outputs) {
    try {
      messages.push(JSON.parse(raw));
    } catch {
      // Ignore malformed output.
    }
  }
  return messages;
}

function consumeServerMessages(messages: object[]) {
  for (const msg of messages) {
    const m = msg as { method?: string; params?: { uri?: string; diagnostics?: Array<{ range: { start: { line: number; character: number }; end: { line: number; character: number } }; severity?: number; message?: string }> } };
    if (m.method === "textDocument/publishDiagnostics") {
      convertDiagnostics(m.params as { uri: string; diagnostics: Array<{ range: { start: { line: number; character: number }; end: { line: number; character: number } }; severity?: number; message?: string }> });
    }
  }
}

function callWorker(message: { payload?: string; control?: { type: string; uri: string; content: string } }): Promise<string[]> {
  return new Promise((resolve, reject) => {
    const token = next_token++;
    pending_tokens.set(token, { resolve, reject });
    worker!.postMessage({ token, ...message });
  });
}

async function sendMessage(message: { jsonrpc: string; id?: number; method?: string; params?: object }) {
  const outputs = await callWorker({ payload: JSON.stringify(message) });
  const parsed = normalizeMessages(outputs);
  consumeServerMessages(parsed);
  return parsed;
}

export async function setVirtualFile(uri: string, content: string) {
  if (!worker) return;
  await callWorker({
    control: {
      type: "setVirtualFile",
      uri,
      content,
    },
  });
}

async function request(method: string, params: object) {
  const id = next_id++;
  const messages = await sendMessage({
    jsonrpc: "2.0",
    id,
    method,
    params,
  });

  for (const msg of messages) {
    const m = msg as { id?: number; error?: { message?: string }; result?: unknown };
    if (m.id !== id) continue;
    if (m.error) throw new Error(m.error.message || "LSP request failed");
    return m.result;
  }

  return null;
}

async function notify(method: string, params: object) {
  await sendMessage({
    jsonrpc: "2.0",
    method,
    params,
  });
}

async function requestAtOffset(source: string, cursorOffset: number, method: string, buildParams: (position: { line: number; character: number }) => object, uri = default_document_uri) {
  if (!initialized) return null;
  await syncDocument(source, uri);
  const position = offsetToPosition(source, cursorOffset);
  return await request(method, buildParams(position));
}

function offsetToPosition(source: string, offset: number): { line: number; character: number } {
  let line = 0;
  let character = 0;

  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === "\n") {
      line++;
      character = 0;
    } else {
      character++;
    }
  }

  return { line, character };
}

function completionKindToString(kind: number): string {
  if (kind === 7) return "class";
  if (kind === 2) return "method";
  if (kind === 3) return "function";
  if (kind === 6) return "variable";
  if (kind === 5) return "field";
  if (kind === 14) return "keyword";
  if (kind === 1) return "text";
  if (kind === 4) return "constructor";
  if (kind === 9) return "module";
  if (kind === 8) return "interface";
  return "variable";
}

async function syncDocument(source: string, uri = default_document_uri) {
  const doc = documents.get(uri);
  if (!doc) {
    const next_doc = { source, version: 1, opened: true };
    documents.set(uri, next_doc);
    await notify("textDocument/didOpen", {
      textDocument: {
        uri,
        languageId: "wren",
        version: next_doc.version,
        text: source,
      },
    });
    return;
  }

  if (source === doc.source) return;

  doc.source = source;
  doc.version += 1;
  await notify("textDocument/didChange", {
    textDocument: {
      uri,
      version: doc.version,
    },
    contentChanges: [{ text: source }],
  });
}

export async function initWasm() {
  worker = new Worker(new URL("./lsp-worker.ts", import.meta.url), { type: "module" });

  worker.onmessage = (event: MessageEvent) => {
    const { token, outputs, error } = event.data;
    const pending = pending_tokens.get(token);
    if (!pending) return;

    pending_tokens.delete(token);
    if (error) {
      pending.reject(new Error(error));
      return;
    }
    pending.resolve(outputs || []);
  };

  await request("initialize", {
    processId: null,
    rootUri: null,
    initializationOptions: null,
    capabilities: {},
    workspaceFolders: null,
  });
  await notify("initialized", {});

  initialized = true;
}

export function isReady() {
  return initialized;
}

export async function parse(source: string, uri = default_document_uri) {
  if (!initialized) return { diagnostics: [] };
  await syncDocument(source, uri);
  return { diagnostics: diagnostics_by_uri.get(uri) || [] };
}

export async function complete(source: string, cursorOffset: number, triggerCharacter: string | undefined, uri = default_document_uri) {
  if (!initialized) return [];

  await syncDocument(source, uri);
  const position = offsetToPosition(source, cursorOffset);

  const is_trigger = typeof triggerCharacter === "string" && triggerCharacter.length > 0;
  const result = await request("textDocument/completion", {
    textDocument: { uri },
    position,
    context: {
      triggerKind: is_trigger ? 2 : 1,
      triggerCharacter: is_trigger ? triggerCharacter : undefined,
    },
  });

  if (!result) return [];
  const items = Array.isArray(result) ? result : (result as { items?: Array<{ label: string; kind: number }> }).items || [];
  return items.map((item) => ({
    label: item.label,
    kind: completionKindToString(item.kind),
  }));
}

export async function definition(source: string, cursorOffset: number, uri = default_document_uri) {
  return await requestAtOffset(source, cursorOffset, "textDocument/definition", (position) => ({
    textDocument: { uri },
    position,
  }), uri);
}

export async function references(source: string, cursorOffset: number, uri = default_document_uri) {
  return await requestAtOffset(source, cursorOffset, "textDocument/references", (position) => ({
    textDocument: { uri },
    position,
    context: { includeDeclaration: true },
  }), uri);
}

export async function hover(source: string, cursorOffset: number, uri = default_document_uri) {
  const res = await requestAtOffset(source, cursorOffset, "textDocument/hover", (position) => ({
    textDocument: { uri },
    position,
  }), uri);
  console.log("[playground] hover response", res);
  return res;
}

export async function documentHighlights(source: string, cursorOffset: number, uri = default_document_uri) {
  return await requestAtOffset(source, cursorOffset, "textDocument/documentHighlight", (position) => ({
    textDocument: { uri },
    position,
  }), uri);
}

export async function rename(source: string, cursorOffset: number, newName: string) {
  if (!initialized) return null;
  await syncDocument(source, default_document_uri);
  const position = offsetToPosition(source, cursorOffset);
  return await request("textDocument/rename", {
    textDocument: { uri: default_document_uri },
    position,
    newName,
  });
}

export async function documentSymbols(source: string, uri = default_document_uri) {
  if (!initialized) return null;
  await syncDocument(source, uri);
  return await request("textDocument/documentSymbol", {
    textDocument: { uri },
  });
}
