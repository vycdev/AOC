const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    // open input.txt
    var input = try std.fs.cwd().openFile("input.txt", .{ });
    defer input.close();

    var buf_reader = std.io.bufferedReader(input.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    var sum: i32 = 0;

    // iterate over the lines
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var firstNumber: u8 = '0';
        var lastNumber: u8 = '0';
        for (line) |c| {
            // if c is a digit then we save it to firstNumber and then break;
            if (c >= '0' and c <= '9') {
                firstNumber = c;
                break;
            }
        }
        // iterate over the line from the end to the start
        for (line) |c| {
            // if c is a digit then we save it to lastNumber and then break;
            if (c >= '0' and c <= '9') {
                lastNumber = c;
            }
        }

        // concatenate firstNumber and lastNumber together
        var number: [2]u8 = .{firstNumber, lastNumber};

        // print number
        // try stdout.print("Number: {c}{c}\n", .{number[0], number[1]});

        // convert the string to an integer
        var integer = try std.fmt.parseInt(i32, &number, 10);

        // add the integer to sum
        sum += integer;
    }

    // print the sum
    try stdout.print("Sum: {}\n", .{sum});
}