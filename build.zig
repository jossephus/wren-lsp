const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lsp = buildLsp(b, target, optimize);

    const wrenalyzer = build_wren_analyzer(b, target, optimize);

    b.installArtifact(wrenalyzer);

    const run_cmd = b.addRunArtifact(lsp);
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}

fn build_wren_analyzer(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.Mode) *std.Build.Step.Compile {
    _ = .{ b, target, optimize };
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/wrenalyzer/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "wren-lsp",
        .root_module = exe_mod,
    });

    return exe;
}

fn buildLsp(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.Mode) *std.Build.Step.Compile {
    const lsp = b.dependency("lsp_kit", .{});

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

    exe_mod.addImport("lsp", lsp.module("lsp"));

    const exe = b.addExecutable(.{
        .name = "wren-lsp",
        .root_module = exe_mod,
    });
    return exe;
}
