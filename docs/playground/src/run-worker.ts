/* eslint-disable @typescript-eslint/no-explicit-any */
const encoder = new TextEncoder();
const decoder = new TextDecoder();

let wasm: any = null;
let wasi: any = null;

async function ensureWasm() {
  if (wasm) return;

  const response = await fetch(new URL("../wren-runner.wasm", import.meta.url));
  const bytes = await response.arrayBuffer();

  const WASI = (await import("@bjorn3/browser_wasi_shim")).WASI;

  const stdio = [
    { fd_write: () => { throw new Error("stdio write is not supported"); }, fd_read: () => { throw new Error("stdio read is not supported"); } } as any,
    { fd_write: () => { throw new Error("stdio write is not supported"); }, fd_read: () => { throw new Error("stdio read is not supported"); } } as any,
    { fd_write: () => { throw new Error("stdio write is not supported"); }, fd_read: () => { throw new Error("stdio read is not supported"); } } as any,
  ];

  wasi = new WASI(["wren-runner.wasm"], [], stdio, { debug: false });

  const { instance } = await WebAssembly.instantiate(bytes, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  wasi.inst = instance;
  wasm = instance.exports;
}

self.addEventListener("message", async (event: MessageEvent) => {
  const { token, source } = event.data as { token: number; source: string };

  try {
    await ensureWasm();

    const encoded = encoder.encode(source);
    const ptr = wasm.allocSource(encoded.length);
    new Uint8Array(wasm.memory.buffer).set(encoded, ptr);

    const result = wasm.runCode();

    const outputPtr = wasm.getOutputPtr();
    const outputLen = wasm.getOutputLen();
    const output = outputLen > 0
      ? decoder.decode(new Uint8Array(wasm.memory.buffer, outputPtr, outputLen))
      : "";

    const errorPtr = wasm.getErrorPtr();
    const errorLen = wasm.getErrorLen();
    const errors = errorLen > 0
      ? decoder.decode(new Uint8Array(wasm.memory.buffer, errorPtr, errorLen))
      : "";

    postMessage({ token, output, errors, result });
  } catch (err) {
    postMessage({ token, output: "", errors: err instanceof Error ? err.message : String(err), result: 2 });
  }
});
