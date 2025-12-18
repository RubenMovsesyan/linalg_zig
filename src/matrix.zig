const std = @import("std");

pub fn Matrix(comptime T: type, comptime rows_: usize, comptime cols_: usize) type {
    // Make sure the type is numeric
    const type_info = @typeInfo(T);
    switch (type_info) {
        .int => |info| {
            if (info.signedness) {} else {
                @compileError("Matrix type must be signed");
            }
        },
        .comptime_int,
        .float,
        => {},
        else => @compileError("Matrix type must be numeric"),
    }

    // Make sure the size is not 0
    if (rows_ == 0 or cols_ == 0) {
        @compileError("Matrix size cannot be zero");
    }

    return struct {
        const Self = @This();

        data: [rows_][cols_]T,

        pub const rows = rows_;
        pub const cols = cols_;

        // Printing
        pub fn format(self: Self, writer: *std.io.Writer) !void {
            try writer.print("Matrix({}x{}) [\n", .{ rows_, cols_ });

            for (0..rows_) |i| {
                try writer.print("  [", .{});
                for (0..cols_) |j| {
                    if (j > 0) try writer.print(", ", .{});

                    switch (@typeInfo(T)) {
                        .int, .comptime_int => try writer.print("{}", .{self.data[i][j]}),
                        .float, .comptime_float => try writer.print("{d:.3}", .{self.data[i][j]}),
                        else => @compileError("Matrix type must be numeric"),
                    }
                }

                try writer.print("]", .{});
                if (i < rows_ - 1) try writer.print(",", .{});
                try writer.print("\n", .{});
            }

            try writer.print("]\n", .{});
        }

        // Equating
        pub fn eql(self: *const Self, other: *const Self) bool {
            return std.meta.eql(self.data, other.data);
        }

        // Initializers
        pub fn init(data: [rows_][cols_]T) Self {
            return Self{ .data = data };
        }

        pub fn zero() Self {
            var data: [rows_][cols_]T = undefined;
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    if (type_info == .int) {
                        data[i][j] = @as(T, @intCast(0));
                    } else {
                        data[i][j] = @as(T, @floatCast(0.0));
                    }
                }
            }

            return Self{ .data = data };
        }

        pub fn eye() Self {
            if (comptime rows_ != cols_) {
                @compileError("Cannot create an identity matrix with different row and column counts");
            }

            var data: [rows_][cols_]T = undefined;
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    if (i == j) {
                        if (type_info == .int) {
                            data[i][j] = @as(T, @intCast(1));
                        } else {
                            data[i][j] = @as(T, @floatCast(1.0));
                        }
                    } else {
                        if (type_info == .int) {
                            data[i][j] = @as(T, @intCast(0));
                        } else {
                            data[i][j] = @as(T, @floatCast(0.0));
                        }
                    }
                }
            }

            return Self{ .data = data };
        }

        // Matrix operation functions

        // Element wise operations
        pub fn elem_add(self: *const Self, other: *const Self) Self {
            var data_out: [rows_][cols_]T = undefined;

            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    data_out[i][j] = self.data[i][j] + other.data[i][j];
                }
            }

            return Self{ .data = data_out };
        }

        pub fn elem_add_to(self: *Self, other: *const Self) void {
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    self.data[i][j] += other.data[i][j];
                }
            }
        }

        pub fn elem_sub(self: *const Self, other: *const Self) Self {
            var data_out: [rows_][cols_]T = undefined;

            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    data_out[i][j] = self.data[i][j] - other.data[i][j];
                }
            }

            return Self{ .data = data_out };
        }

        pub fn elem_sub_to(self: *Self, other: *const Self) void {
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    self.data[i][j] -= other.data[i][j];
                }
            }
        }

        pub fn elem_mul(self: *const Self, other: *const Self) Self {
            var data_out: [rows_][cols_]T = undefined;

            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    data_out[i][j] = self.data[i][j] * other.data[i][j];
                }
            }

            return Self{ .data = data_out };
        }

        pub fn elem_mul_to(self: *Self, other: *const Self) void {
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    self.data[i][j] *= other.data[i][j];
                }
            }
        }

        pub fn elem_div(self: *const Self, other: *const Self) Self {
            var data_out: [rows_][cols_]T = undefined;

            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    data_out[i][j] = self.data[i][j] / other.data[i][j];
                }
            }

            return Self{ .data = data_out };
        }

        pub fn elem_div_to(self: *Self, other: *const Self) void {
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    self.data[i][j] /= other.data[i][j];
                }
            }
        }

        // Matrix operations
        pub fn mult(self: *const Self, other: *const Self) Matrix(T, rows, @TypeOf(other.*).cols) {
            const OtherMat = @TypeOf(other.*);
            if (comptime cols != OtherMat.rows) {
                @compileError("Matrix Dimensions do not match for muliplication");
            }

            var data: [rows][OtherMat.cols]T = undefined;
            inline for (0..rows) |row| {
                inline for (0..OtherMat.cols) |col| {
                    inline for (0..cols) |k| {
                        data[row][col] += self.data[row][k] * other.data[k][col];
                    }
                }
            }

            return Matrix(T, rows, OtherMat.cols).init(data);
        }

        pub fn transposed(self: *const Self) Matrix(T, cols, rows) {
            var data: [cols][rows]T = undefined;
            inline for (0..rows) |row| {
                inline for (0..cols) |col| {
                    data[row][col] = self.data[col][row];
                }
            }

            return Matrix(T, cols, rows).init(data);
        }

        fn is_matrix_pointer(comptime PtrType: type) bool {
            const ptr_type_info = @typeInfo(PtrType);

            if (ptr_type_info != .pointer) {
                return false;
            }

            const child_type = ptr_type_info.pointer.child;

            return @hasField(child_type, "data") and @hasDecl(child_type, "rows") and @hasDecl(child_type, "cols");
        }

        // Vector operations
        pub fn dot(self: *const Self, other: anytype) T {
            const OtherMatType = @TypeOf(other);

            if (comptime !is_matrix_pointer(OtherMatType)) {
                @compileError("Expected a pointer to a Matrix struct, got " ++ @typeName(OtherMatType));
            }

            const OtherMat = @TypeOf(other.*);

            if (comptime rows != 1 and OtherMat.rows != 1) {
                @compileError("Dot product is only defined for vectors");
            }

            if (comptime cols != OtherMat.cols) {
                @compileError("Dot product is only defined for vectors of the same dimension");
            }

            var output: T = 0;
            inline for (0..cols) |i| {
                output += self.data[0][i] * other.data[0][i];
            }

            return output;
        }

        pub fn cross(self: *const Self, other: anytype) Vector(T, cols) {
            const OtherMatType = @TypeOf(other);

            if (comptime !is_matrix_pointer(OtherMatType)) {
                @compileError("Expected a pointer to a Matrix struct, got " ++ @typeName(OtherMatType));
            }

            const OtherMat = @TypeOf(other.*);

            if (comptime rows != 1 and OtherMat.rows != 1) {
                @compileError("Cross product is only defined for vectors");
            }

            if (comptime cols != OtherMat.cols) {
                @compileError("Cross product is only defined for vectors of the same dimension");
            }

            // TODO: Implement for vector with dimenison of 7
            if (comptime cols != 3) {
                @compileError("Cross product is only defined for vectors of dimension 3");
            }

            const x = self.data[0][1] * other.data[0][2] - self.data[0][2] * other.data[0][1];
            const y = self.data[0][2] * other.data[0][0] - self.data[0][0] * other.data[0][2];
            const z = self.data[0][0] * other.data[0][1] - self.data[0][1] * other.data[0][0];

            return Vector(T, cols).init(.{.{ x, y, z }});
        }

        pub fn mag(self: *const Self) T {
            if (comptime rows_ != 1) {
                @compileError("Magnitude is only defined for vectors");
            }

            var sum = @as(T, 0);
            inline for (self.data[0]) |elem| {
                sum += elem * elem;
            }
            sum = std.math.sqrt(sum);

            return sum;
        }

        pub fn norm(self: *const Self) Self {
            if (comptime rows_ != 1) {
                @compileError("Normalization is only defined for vectors");
            }

            const magnitude = self.mag();

            var output = Self.init(self.data);
            inline for (&output.data[0]) |*elem| {
                elem.* /= magnitude;
            }

            return output;
        }

        pub fn normalize(self: *Self) void {
            if (comptime rows_ != 1) {
                @compileError("Normalization is only defined for vectors");
            }

            const magnitude = self.mag();

            inline for (&self.data[0]) |*elem| {
                elem.* /= magnitude;
            }
        }
    };
}

pub fn Vector(comptime T: type, comptime dim: usize) type {
    return Matrix(T, 1, dim);
}

pub const Mat2i = Matrix(i32, 2, 2);
pub const Mat2f = Matrix(f32, 2, 2);
pub const Mat2d = Matrix(f64, 2, 2);

pub const Mat3i = Matrix(i32, 3, 3);
pub const Mat3f = Matrix(f32, 3, 3);
pub const Mat3d = Matrix(f64, 3, 3);

pub const Mat4i = Matrix(i32, 4, 4);
pub const Mat4f = Matrix(f32, 4, 4);
pub const Mat4d = Matrix(f64, 4, 4);

pub const Vec2i = Vector(i32, 2);
pub const Vec2f = Vector(f32, 2);
pub const Vec2d = Vector(f64, 2);

pub const Vec3i = Vector(i32, 3);
pub const Vec3f = Vector(f32, 3);
pub const Vec3d = Vector(f64, 3);

pub const Vec4i = Vector(i32, 4);
pub const Vec4f = Vector(f32, 4);
pub const Vec4d = Vector(f64, 4);

test "Matrix Creation" {
    const zero_mat = Mat4f.zero();
    std.debug.print("{f}", .{zero_mat});

    const eye_mat = Mat4f.eye();
    std.debug.print("{f}", .{eye_mat});
}

test "Matrix Adding" {
    var eye_mat = Mat4f.eye();
    const adding_mat = Mat4f.init(.{
        .{ 1.0, 2.0, 3.0, 4.0 },
        .{ 5.0, 6.0, 7.0, 8.0 },
        .{ 9.0, 10.0, 11.0, 12.0 },
        .{ 13.0, 14.0, 15.0, 16.0 },
    });

    const expected_mat = Mat4f.init(.{
        .{ 2.0, 2.0, 3.0, 4.0 },
        .{ 5.0, 7.0, 7.0, 8.0 },
        .{ 9.0, 10.0, 12.0, 12.0 },
        .{ 13.0, 14.0, 15.0, 17.0 },
    });

    const added_mat = eye_mat.elem_add(&adding_mat);
    std.log.debug("Original: {f} Adding: {f}", .{ eye_mat, adding_mat });
    std.log.debug("Result: {f}", .{added_mat});

    try std.testing.expect(added_mat.eql(&expected_mat));

    eye_mat.elem_add_to(&adding_mat);

    try std.testing.expect(eye_mat.eql(&expected_mat));
}

test "Matrix Multiplication" {
    const mat2_f_a = Mat2f.init(.{
        .{ 1.0, 2.0 },
        .{ 3.0, 4.0 },
    });

    const mat2_f_b = Mat2f.init(.{
        .{ 5.0, 6.0 },
        .{ 7.0, 8.0 },
    });

    const output = mat2_f_a.mult(&mat2_f_b);

    const expected = Mat2f.init(.{
        .{ 19.0, 22.0 },
        .{ 43.0, 50.0 },
    });

    try std.testing.expect(output.eql(&expected));
}

test "Matrix Transpose" {
    const mat2_f_a = Mat2f.init(.{
        .{ 1.0, 2.0 },
        .{ 3.0, 4.0 },
    });

    const output = mat2_f_a.transposed();

    const expected = Mat2f.init(.{
        .{ 1.0, 3.0 },
        .{ 2.0, 4.0 },
    });

    try std.testing.expect(output.eql(&expected));
}

test "Vector Dot" {
    const vec_1 = Vec3f.init(.{.{ 1, 2, 3 }});

    const vec_2 = Vec3f.init(.{.{ 1, 2, 3 }});

    const output = vec_1.dot(&vec_2);
    std.debug.print("Output: {}\n", .{output});

    try std.testing.expect(output == 14);
}

test "Vector Cross" {
    const vec_1 = Vec3f.init(.{.{ 1, 2, 3 }});
    const vec_2 = Vec3f.init(.{.{ 4, 5, 6 }});

    const expected = Vec3f.init(.{.{ -3, 6, -3 }});

    const output = vec_1.cross(&vec_2);
    std.debug.print("Output: {f}\n", .{output});

    try std.testing.expect(output.eql(&expected));
}

test "Vector Normalization" {
    const ERROR = 1e-6;

    const vec = Vec3f.init(.{.{ 1, 2, 3 }});
    const normalized = vec.norm();

    std.debug.print("Input {f}\n", .{vec});
    std.debug.print("Output {f}\n", .{normalized});

    const mag = normalized.mag();
    std.debug.print("Magnitude: {}\n", .{mag});
    try std.testing.expect(mag >= 1.0 - ERROR and mag <= 1.0 + ERROR);
}

test "Vector Normalization In Place" {
    const ERROR = 1e-6;

    var vec = Vec3f.init(.{.{ 1, 2, 3 }});

    std.debug.print("Input {f}\n", .{vec});
    vec.normalize();
    std.debug.print("Output {f}\n", .{vec});

    const mag = vec.mag();
    std.debug.print("Magnitude: {}\n", .{mag});
    try std.testing.expect(mag >= 1.0 - ERROR and mag <= 1.0 + ERROR);
}
