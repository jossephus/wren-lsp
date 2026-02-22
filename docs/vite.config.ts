import { defineConfig } from "vite";

export default defineConfig({
  base: "/wren-lsp/",
  root: ".",
  publicDir: "public",
  build: {
    target: "esnext",
    outDir: "dist",
    rollupOptions: {
      input: {
        main: "index.html",
        playground: "playground/index.html",
      },
    },
  },
  worker: {
    format: "es",
  },
});
