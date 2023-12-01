const std = @import("std");

const ParseResult = struct {
    success: bool, 
    value: u8,
};

fn stringComparer(a: []const u8, b: []const u8) bool {
    for (b, 0..) |a_char, i| {
        if(a_char != a[i]) {
            return false;
        }
    }

    return true;
}

fn parseNumber(index: usize, line: []const u8) !ParseResult {
    var i = index;
    var j: u8 = 0;
    var potentialNumber: [1024]u8 = undefined;

    while (i < line.len) : (i += 1) {
        var c = line[i];
        potentialNumber[j] = c;
        potentialNumber[j + 1] = 0;
        j += 1;
        
        if(stringComparer(&potentialNumber, "one"))
            return ParseResult{.success = true, .value = '1'};
        if(stringComparer(&potentialNumber, "two"))
            return ParseResult{.success = true, .value = '2'};
        if(stringComparer(&potentialNumber, "three"))
            return ParseResult{.success = true, .value = '3'};
        if(stringComparer(&potentialNumber, "four"))
            return ParseResult{.success = true, .value = '4'};
        if(stringComparer(&potentialNumber, "five"))
            return ParseResult{.success = true, .value = '5'};
        if(stringComparer(&potentialNumber, "six"))
            return ParseResult{.success = true, .value = '6'};
        if(stringComparer(&potentialNumber, "seven"))
            return ParseResult{.success = true, .value = '7'};
        if(stringComparer(&potentialNumber, "eight"))
            return ParseResult{.success = true, .value = '8'};
        if(stringComparer(&potentialNumber, "nine"))
            return ParseResult{.success = true, .value = '9'};
        if(stringComparer(&potentialNumber, "zero"))
            return ParseResult{.success = true, .value = '0'};
    }

    return ParseResult{.success = false, .value = '0'};
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    // open input.txt
    var input = try std.fs.cwd().openFile("input.txt", .{ });
    defer input.close();

    var buf_reader = std.io.bufferedReader(input.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    var sum: u32 = 0;

    // iterate over the lines
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var firstNumber: u8 = '0';
        var lastNumber: u8 = '0';
        for (line, 0..) |c, index| {
            // if c is a digit then we save it to firstNumber and then break;
            if (c >= '0' and c <= '9') {
                firstNumber = c;
                break;
            } else {
                var result = try parseNumber(index, line);
                if (result.success) {
                    firstNumber = result.value;
                    break;
                }
            }
        }
        // iterate over the line from the end to the start
        for (line, 0..) |c, index| {
            // if c is a digit then we save it to lastNumber and then break;
            if (c >= '0' and c <= '9') {
                lastNumber = c;
            } else {
                var result = try parseNumber(index, line);
                if (result.success) {
                    lastNumber = result.value;
                }
            } 
        }

        // concatenate firstNumber and lastNumber together
        var number: [2]u8 = .{firstNumber, lastNumber};

        // print number
        // try stdout.print("Number: {c}{c}\n", .{number[0], number[1]});

        // convert the string to an integer
        var integer = try std.fmt.parseInt(u32, &number, 10);

        // add the integer to sum
        sum += integer;
    }

    // print the sum
    try stdout.print("Sum: {}\n", .{sum});
}