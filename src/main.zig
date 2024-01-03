const std = @import("std");
const debug = std.debug;
const io = std.io;

const native_endianness = @import("builtin").target.cpu.arch.endian();

const clap = @import("clap");

const decode = @import("decode");

const MAX_FILE_SIZE: usize = 33554432; // 32 MiB

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    // First we specify what parameters our program can take.
    // We can use `parseParamsComptime` to parse a string into an array of `Param(Help)`
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\<str>
    );

    // Initialize our diagnostics, which can be used for reporting useful errors.
    // This is optional. You can also pass `.{}` to `clap.parse` if you don't
    // care about the extra information `Diagnostics` provides.
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = gpa.allocator(),
    }) catch |err| {
        // Report useful error and exit
        diag.report(io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        debug.print("--help\n", .{});
    }

    if (res.positionals.len == 0 or res.positionals[0].len == 0) {
        return clap.usage(std.io.getStdErr().writer(), clap.Help, &params);
    }

    var file: std.fs.File = try std.fs.cwd().openFile(res.positionals[0], std.fs.File.OpenFlags{});
    defer file.close();

    const code: []u8 = try file.readToEndAlloc(gpa.allocator(), MAX_FILE_SIZE);
    defer gpa.allocator().free(code);

    if (native_endianness == std.builtin.Endian.big) {
        @byteSwap(code);
    }

    if (code.len % 8 != 0) {
        @panic("Unaligned code");
    }

    const instructions: std.ArrayList(decode.Instruction) = try decode.decode(@alignCast(std.mem.bytesAsSlice(u64, code)), gpa.allocator());
    defer instructions.deinit();

    debug.print("{?}\n", .{instructions.items[0]});
}
