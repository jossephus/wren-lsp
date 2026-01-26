const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

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
