/* eslint-disable @typescript-eslint/no-explicit-any */
const encoder = new TextEncoder();
const decoder = new TextDecoder();

let wasm: any = null;
let wasi: any = null;
const virtual_files = new Map<string, string>();
let host_resolve: ((importerUri: string, importString: string) => { canonical_id: string; uri: string; source: string; kind: string } | null) | null = null;

async function ensureWasm() {
  if (wasm) return;

  const response = await fetch(new URL("../wren-playground.wasm", import.meta.url));
  const bytes = await response.arrayBuffer();

  const WASI = (await import("@bjorn3/browser_wasi_shim")).WASI;

  const stdio = [
    { fd_write: () => { throw new Error("stdio write is not supported"); }, fd_read: () => { throw new Error("stdio read is not supported"); } } as any,
    { fd_write: () => { throw new Error("stdio write is not supported"); }, fd_read: () => { throw new Error("stdio read is not supported"); } } as any,
    { fd_write: () => { throw new Error("stdio write is not supported"); }, fd_read: () => { throw new Error("stdio read is not supported"); } } as any,
  ];

  wasi = new WASI(["wren-playground.wasm"], [], stdio, {
    debug: false,
  });

  const { instance } = await WebAssembly.instantiate(bytes, {
    wasi_snapshot_preview1: wasi.wasiImport,
    env: {
      wren_lsp_host_resolve: hostResolve,
    },
  });

  wasi.inst = instance;

  wasm = instance.exports;
  wasm.createServer();
}

function dirnameOfUri(uri: string): string {
  const idx = uri.lastIndexOf("/");
  if (idx < 0) return uri;
  return uri.slice(0, idx + 1);
}

function resolveScriptUri(config_uri: string, script_path: string): string {
  if (script_path.startsWith("file://")) return script_path;
  if (script_path.startsWith("/")) return `file://${script_path}`;
  const base = dirnameOfUri(config_uri);
  return new URL(script_path, base).toString();
}

async function refreshHostResolver() {
  host_resolve = null;

  const config_uri = "file:///playground/wren-lsp.json";
  const config_text = virtual_files.get(config_uri);
  if (!config_text) return;

  let config: { resolvers?: Array<{ type?: string; script?: string }> } | null = null;
  try {
    config = JSON.parse(config_text);
  } catch {
    return;
  }

  const resolvers = Array.isArray(config?.resolvers) ? config.resolvers : [];
  const host_cfg = resolvers.find((r) => r && r.type === "host");
  const script_path = typeof host_cfg?.script === "string" ? host_cfg.script : "";
  if (!script_path) return;

  const script_uri = resolveScriptUri(config_uri, script_path);
  const script_source = virtual_files.get(script_uri);
  if (!script_source) return;

  try {
    const blob = new Blob([script_source], { type: "text/javascript" });
    const url = URL.createObjectURL(blob);
    const mod = await import(/* @vite-ignore */ `${url}#${Date.now()}`);
    URL.revokeObjectURL(url);

    if (typeof mod.preloadDomeModules === "function") {
      try {
        await mod.preloadDomeModules();
      } catch (err) {
        console.warn("[playground] preloadDomeModules failed", err);
      }
    }

    if (typeof mod.resolveHostImport === "function") {
      host_resolve = mod.resolveHostImport;
    }
  } catch (err) {
    console.warn("[playground] failed loading resolver script", err);
  }
}

async function applyControlMessage(control: { type?: string; uri?: string; content?: string }) {
  if (!control || control.type !== "setVirtualFile") return;
  if (typeof control.uri !== "string" || typeof control.content !== "string") return;
  virtual_files.set(control.uri, control.content);
  // Only refresh resolver if config or resolver script changed
  if (control.uri.includes("wren-lsp.json") || control.uri.includes("resolver.js")) {
    await refreshHostResolver();
  }
}

function readWasmBytes(ptr: number, len: number): Uint8Array {
  return new Uint8Array(wasm.memory.buffer, ptr, len);
}

function readWasmString(ptr: number, len: number): string {
  return decoder.decode(readWasmBytes(ptr, len));
}

function resolvePlaygroundImport(importerUri: string, importString: string): { canonical_id: string; uri: string; source: string; kind: string } | null {
  if (!importString.startsWith("./") && !importString.startsWith("../")) return null;

  let importPath = importString;
  if (!importPath.endsWith(".wren")) importPath += ".wren";

  const resolvedUri = new URL(importPath, importerUri).href;
  const source = virtual_files.get(resolvedUri);
  if (source !== undefined) {
    return { canonical_id: resolvedUri, uri: resolvedUri, source, kind: "virtual" };
  }
  return null;
}

function hostResolve(importer_ptr: number, importer_len: number, import_ptr: number, import_len: number, _project_ptr: number, _project_len: number, out_ptr: number, out_cap: number): number {
  if (!wasm) return 0;

  const importer_uri = readWasmString(importer_ptr, importer_len);
  const import_string = readWasmString(import_ptr, import_len);

  const resolved = resolvePlaygroundImport(importer_uri, import_string)
    || (typeof host_resolve === "function" ? host_resolve(importer_uri, import_string) : null);
  if (!resolved) return 0;

  const json = JSON.stringify(resolved);
  const encoded = encoder.encode(json);
  if (encoded.length <= out_cap) {
    readWasmBytes(out_ptr, encoded.length).set(encoded);
  }
  return encoded.length;
}

function runRpc(payload: string): string[] {
  const input = encoder.encode(payload);
  const ptr = wasm.allocMessage(input.length);
  const mem = new Uint8Array(wasm.memory.buffer);
  mem.set(input, ptr);

  wasm.call();

  const count = wasm.outputMessageCount();
  const outputs: string[] = [];
  for (let i = 0; i < count; i++) {
    const start = wasm.outputMessagePtr(i);
    const len = wasm.outputMessageLen(i);
    const msg = new Uint8Array(wasm.memory.buffer).slice(start, start + len);
    outputs.push(decoder.decode(msg));
  }
  return outputs;
}

self.addEventListener("message", async (event: MessageEvent) => {
  const { token, payload, control } = event.data as { token: number; payload?: string; control?: { type: string; uri: string; content: string } };

  try {
    if (control) {
      await applyControlMessage(control);
      postMessage({ token, outputs: [] });
      return;
    }

    await ensureWasm();
    const outputs = runRpc(payload!);
    postMessage({ token, outputs });
  } catch (err) {
    postMessage({ token, error: err instanceof Error ? err.message : String(err) });
  }
});
