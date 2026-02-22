const std = @import("std");

pub fn build(b: *std.Build) void {
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });

    const wren_dep = b.dependency("wren", .{});

    const wren_lib = b.addLibrary(.{
        .name = "wren",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = wasm_target,
            .optimize = .ReleaseSmall,
            .link_libc = true,
        }),
    });

    wren_lib.addIncludePath(wren_dep.path("src/include"));
    wren_lib.addIncludePath(wren_dep.path("src/vm"));
    wren_lib.addIncludePath(wren_dep.path("src/optional"));

    wren_lib.addCSourceFiles(.{
        .root = wren_dep.path("src/vm"),
        .files = &.{
            "wren_compiler.c",
            "wren_core.c",
            "wren_debug.c",
            "wren_primitive.c",
            "wren_utils.c",
            "wren_value.c",
            "wren_vm.c",
        },
    });
    wren_lib.addCSourceFiles(.{
        .root = wren_dep.path("src/optional"),
        .files = &.{
            "wren_opt_meta.c",
            "wren_opt_random.c",
        },
    });

    wren_lib.installHeadersDirectory(wren_dep.path("src/include"), "", .{
        .include_extensions = &.{"wren.h"},
    });

    const runner_mod = b.createModule(.{
        .root_source_file = b.path("src/wasm_runner.zig"),
        .target = wasm_target,
        .optimize = .ReleaseSmall,
    });

    const runner_exe = b.addExecutable(.{
        .name = "wren-runner",
        .root_module = runner_mod,
    });
    runner_exe.entry = .disabled;
    runner_exe.rdynamic = true;
    runner_exe.linkLibrary(wren_lib);

    const install = b.addInstallArtifact(runner_exe, .{});

    const copy = b.addSystemCommand(&.{ "cp", "-f" });
    copy.addFileArg(runner_exe.getEmittedBin());
    copy.addArg("wren-runner.wasm");
    copy.step.dependOn(&install.step);

    b.getInstallStep().dependOn(&copy.step);
}
