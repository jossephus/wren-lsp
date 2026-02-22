const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const skip_run = b.option(bool, "skip-run", "Skip running test executables") orelse false;

    const lsp = buildLsp(b, target, optimize);

    b.installArtifact(lsp);

    const exe_mod = b.addModule("wrenalyzer", .{
        .root_source_file = b.path("src/wrenalyzer/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const testWrenAnalyzer = b.addExecutable(.{ .name = "test-wrenalyzer", .root_module = exe_mod });

    const test_step = b.step("test", "Run test wrenalyzer");
    test_step.dependOn(&testWrenAnalyzer.step);
    b.installArtifact(testWrenAnalyzer);

    const run_cmd = b.addRunArtifact(testWrenAnalyzer);
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_runner_mod = b.createModule(.{
        .root_source_file = b.path("src/wrenalyzer/test_runner.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_runner = b.addExecutable(.{
        .name = "test-runner",
        .root_module = test_runner_mod,
    });

    b.installArtifact(test_runner);

    const run_tests_cmd = b.addRunArtifact(test_runner);
    run_tests_cmd.step.dependOn(b.getInstallStep());

    const run_tests_step = b.step("test-wren", "Run parser tests against wren test suite");
    run_tests_step.dependOn(&run_tests_cmd.step);

    // LSP feature tests
    const lsp_dep = b.dependency("lsp_kit", .{});
    const wrenalyzer_mod = build_wren_analyzer(b, target, optimize);

    const lsp_test_mod = b.createModule(.{
        .root_source_file = b.path("src/lsp_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    lsp_test_mod.addImport("wrenalyzer", wrenalyzer_mod);
    lsp_test_mod.addImport("lsp", lsp_dep.module("lsp"));

    const lsp_tests = b.addTest(.{
        .root_module = lsp_test_mod,
    });

    const lsp_test_step = b.step("test-lsp", "Run LSP feature tests");
    if (skip_run) {
        lsp_test_step.dependOn(&lsp_tests.step);
    } else {
        const run_lsp_tests = b.addRunArtifact(lsp_tests);
        lsp_test_step.dependOn(&run_lsp_tests.step);
    }

    // WASM playground
    const wasm_step = b.step("wasm", "Build WASM module for the playground");
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });
    const wasm_wrenalyzer = b.createModule(.{
        .root_source_file = b.path("src/wrenalyzer/root.zig"),
        .target = wasm_target,
        .optimize = .ReleaseSmall,
    });
    const wasm_mod = b.createModule(.{
        .root_source_file = b.path("src/wasm.zig"),
        .target = wasm_target,
        .optimize = .ReleaseSmall,
    });
    wasm_mod.addImport("wrenalyzer", wasm_wrenalyzer);
    wasm_mod.addImport("lsp", lsp_dep.module("lsp"));
    const wasm_lib = b.addExecutable(.{
        .name = "wren-playground",
        .root_module = wasm_mod,
    });
    wasm_lib.entry = .disabled;
    wasm_lib.rdynamic = true;
    const wasm_install = b.addInstallArtifact(wasm_lib, .{});

    const sync_wasm_to_docs = b.addSystemCommand(&.{ "cp", "-f" });
    sync_wasm_to_docs.addFileArg(wasm_lib.getEmittedBin());
    sync_wasm_to_docs.addArg("docs/playground/wren-playground.wasm");
    sync_wasm_to_docs.step.dependOn(&wasm_install.step);

    wasm_step.dependOn(&sync_wasm_to_docs.step);

    const fmt_step = b.step("fmt", "Format Zig files");
    const fmt_zig = b.addFmt(.{
        .paths = &.{ "src", "build.zig" },
        .exclude_paths = &.{"src/wrenalyzer/test.wren"},
        .check = false,
    });
    fmt_step.dependOn(&fmt_zig.step);
}

fn build_wren_analyzer(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) *std.Build.Module {
    _ = .{ b, target, optimize };
    const exe_mod = b.addModule("wrenalyzer", .{
        .root_source_file = b.path("src/wrenalyzer/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    //const exe = b.addExecutable(.{
    //.name = "wren-lsp",
    //.root_module = exe_mod,
    //});

    return exe_mod;
}

fn buildLsp(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) *std.Build.Step.Compile {
    const lsp = b.dependency("lsp_kit", .{});
    const wrenalyzer = build_wren_analyzer(b, target, optimize);

    //const module = b.addModule(
    //"lsp",
    //.{
    //.root_source_file = b.path("src/root.zig"),
    //.target = target,
    //.optimize = optimize,
    //.imports = &.{
    //.{ .name = "diffz", .module = diffz.module("diffz") },
    //},
    //},
    //);
    //module.addImport("lsp", lsp_codegen.module("lsp"));

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addImport("wrenalyzer", wrenalyzer);

    exe_mod.addImport("lsp", lsp.module("lsp"));

    const exe = b.addExecutable(.{
        .name = "wren-lsp",
        .root_module = exe_mod,
    });
    return exe;
}
