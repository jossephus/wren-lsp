#include <stddef.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    const char* severity;
    const char* message;
} WrenLspDiagnostic;

typedef struct {
    const char* canonical_id;
    const char* uri;
    const char* source;
    const char* kind;
    WrenLspDiagnostic* diagnostics;
    size_t diagnostics_len;
} WrenLspResolveResult;

static const char* dup_string(const char* value) {
    size_t len = strlen(value) + 1;
    char* out = (char*)malloc(len);
    if (!out) {
        return NULL;
    }
    memcpy(out, value, len);
    return out;
}

static const char* kMathSource =
    "class Vector {\n"
    "  construct new(x, y, z) {\n"
    "    _x = x\n"
    "    _y = y\n"
    "    _z = z\n"
    "  }\n"
    "\n"
    "  length {\n"
    "    return Math.sqrt(_x * _x + _y * _y + _z * _z)\n"
    "  }\n"
    "}\n";

WrenLspResolveResult wren_lsp_resolve_module(
    const char* importer_uri,
    const char* import_string,
    const char* project_root
) {
    (void)importer_uri;
    (void)project_root;

    WrenLspResolveResult result = {0};
    if (import_string == NULL) {
        return result;
    }

    if (strcmp(import_string, "bad") == 0) {
        result.canonical_id = dup_string("wren://plugin/bad");
        result.kind = dup_string("virtual");
        result.source = dup_string("// intentionally empty\n");
        result.diagnostics_len = 1;
        result.diagnostics = (WrenLspDiagnostic*)malloc(sizeof(WrenLspDiagnostic));
        if (result.diagnostics != NULL) {
            result.diagnostics[0].severity = dup_string("error");
            result.diagnostics[0].message = dup_string("Plugin blocked import \"bad\".");
        } else {
            result.diagnostics_len = 0;
        }
        return result;
    }

    if (strcmp(import_string, "math") != 0) {
        return result;
    }

    result.canonical_id = dup_string("wren://plugin/math");
    result.kind = dup_string("virtual");
    result.source = dup_string(kMathSource);
    return result;
}

void wren_lsp_free_result(WrenLspResolveResult result) {
    free((void*)result.canonical_id);
    free((void*)result.uri);
    free((void*)result.source);
    free((void*)result.kind);
    if (result.diagnostics != NULL && result.diagnostics_len > 0) {
        for (size_t i = 0; i < result.diagnostics_len; ++i) {
            free((void*)result.diagnostics[i].severity);
            free((void*)result.diagnostics[i].message);
        }
        free(result.diagnostics);
    }
}
