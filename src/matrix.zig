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

        data: [rows_ * cols_]T,

        pub const rows = rows_;
        pub const cols = cols_;

        pub fn isQuaternion() bool {
            return rows_ == 1 and cols_ == 4;
        }

        // Printing
        pub fn format(self: Self, writer: *std.io.Writer) !void {
            try writer.print("Matrix({}x{}) [\n", .{ rows_, cols_ });

            for (0..rows_) |i| {
                try writer.print("  [", .{});
                for (0..cols_) |j| {
                    if (j > 0) try writer.print(", ", .{});

                    const index = i * cols + j;
                    switch (@typeInfo(T)) {
                        .int, .comptime_int => try writer.print("{}", .{self.data[index]}),
                        .float, .comptime_float => try writer.print("{d:.3}", .{self.data[index]}),
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
        pub fn init(data: [rows_ * cols_]T) Self {
            return Self{ .data = data };
        }

        pub fn zero() Self {
            var data: [rows_ * cols_]T = undefined;
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    if (type_info == .int) {
                        data[index] = @as(T, @intCast(0));
                    } else {
                        data[index] = @as(T, @floatCast(0.0));
                    }
                }
            }

            return Self{ .data = data };
        }

        pub fn eye() Self {
            if (comptime Self.isQuaternion()) {
                var data: [rows_ * cols_]T = undefined;
                if (type_info == .int) {
                    data[0] = @as(T, @intCast(0));
                    data[1] = @as(T, @intCast(0));
                    data[2] = @as(T, @intCast(0));
                    data[3] = @as(T, @intCast(1));
                } else {
                    data[0] = @as(T, @floatCast(0.0));
                    data[1] = @as(T, @floatCast(0.0));
                    data[2] = @as(T, @floatCast(0.0));
                    data[3] = @as(T, @floatCast(1.0));
                }
                return Self{ .data = data };
            }

            if (comptime rows_ != cols_) {
                @compileError("Cannot create an identity matrix with different row and column counts");
            }

            var data: [rows_ * cols_]T = undefined;
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    if (i == j) {
                        if (type_info == .int) {
                            data[index] = @as(T, @intCast(1));
                        } else {
                            data[index] = @as(T, @floatCast(1.0));
                        }
                    } else {
                        if (type_info == .int) {
                            data[index] = @as(T, @intCast(0));
                        } else {
                            data[index] = @as(T, @floatCast(0.0));
                        }
                    }
                }
            }

            return Self{ .data = data };
        }

        // Matrix operation functions

        // Element wise operations
        pub fn elemAdd(self: *const Self, other: *const Self) Self {
            var data_out: [rows_ * cols_]T = undefined;

            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    data_out[index] = self.data[index] + other.data[index];
                }
            }

            return Self{ .data = data_out };
        }

        pub fn elemAddTo(self: *Self, other: *const Self) void {
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    self.data[index] += other.data[index];
                }
            }
        }

        pub fn elemSub(self: *const Self, other: *const Self) Self {
            var data_out: [rows_ * cols_]T = undefined;

            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    data_out[index] = self.data[index] - other.data[index];
                }
            }

            return Self{ .data = data_out };
        }

        pub fn elemSubTo(self: *Self, other: *const Self) void {
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    self.data[index] -= other.data[index];
                }
            }
        }

        pub fn elemMul(self: *const Self, other: *const Self) Self {
            var data_out: [rows_ * cols_]T = undefined;

            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    data_out[index] = self.data[index] * other.data[index];
                }
            }

            return Self{ .data = data_out };
        }

        pub fn elemMulTo(self: *Self, other: *const Self) void {
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    self.data[index] *= other.data[index];
                }
            }
        }

        pub fn elemDiv(self: *const Self, other: *const Self) Self {
            var data_out: [rows_ * cols_]T = undefined;

            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    data_out[index] = self.data[index] / other.data[index];
                }
            }

            return Self{ .data = data_out };
        }

        pub fn elemDivTo(self: *Self, other: *const Self) void {
            inline for (0..rows_) |i| {
                inline for (0..cols_) |j| {
                    const index = i * cols_ + j;
                    self.data[index] /= other.data[index];
                }
            }
        }

        // Matrix operations
        pub fn mult(self: *const Self, other: anytype) Matrix(T, rows, @TypeOf(other.*).cols) {
            const OtherMat = @TypeOf(other.*);

            // Ensure other is a Matrix type
            if (comptime !@hasField(OtherMat, "data") or !@hasDecl(OtherMat, "rows") or !@hasDecl(OtherMat, "cols")) {
                @compileError("Expected a Matrix type for multiplication");
            }

            if (comptime cols != OtherMat.rows) {
                @compileError("Matrix Dimensions do not match for muliplication");
            }

            var data: [rows * OtherMat.cols]T = undefined;
            // Initialize the data first
            inline for (0..rows) |row| {
                inline for (0..OtherMat.cols) |col| {
                    const index = row * OtherMat.cols + col;

                    if (type_info == .int) {
                        data[index] = @as(T, @intCast(0));
                    } else {
                        data[index] = @as(T, @floatCast(0.0));
                    }
                }
            }

            inline for (0..rows) |row| {
                inline for (0..OtherMat.cols) |col| {
                    inline for (0..cols) |k| {
                        const self_index = row * cols + col;
                        const self_iter_index = row * cols + k;
                        const other_iter_index = k * OtherMat.cols + col;
                        data[self_index] += self.data[self_iter_index] * other.data[other_iter_index];
                    }
                }
            }

            return Matrix(T, rows, OtherMat.cols).init(data);
        }

        pub fn scalarMult(self: *const Self, scalar: T) Matrix(T, rows, cols) {
            var data: [rows * cols]T = self.data;
            inline for (0..rows) |row| {
                inline for (0..cols) |col| {
                    const index = row * cols + col;
                    data[index] *= scalar;
                }
            }

            return Matrix(T, rows, cols).init(data);
        }

        pub fn scalarMultTo(self: *Self, scalar: T) void {
            inline for (0..rows) |row| {
                inline for (0..cols) |col| {
                    const index = row * cols + col;
                    self.data[index] *= scalar;
                }
            }
        }

        pub fn transposed(self: *const Self) Matrix(T, cols, rows) {
            var data: [cols * rows]T = undefined;
            inline for (0..rows) |row| {
                inline for (0..cols) |col| {
                    const index = row * cols + col;
                    const index_inv = col * cols + row;
                    data[index] = self.data[index_inv];
                }
            }

            return Matrix(T, cols, rows).init(data);
        }

        fn isMatrixPointer(comptime PtrType: type) bool {
            const ptr_type_info = @typeInfo(PtrType);

            if (ptr_type_info != .pointer) {
                return false;
            }

            const child_type = ptr_type_info.pointer.child;
            const child_type_info = @typeInfo(child_type);

            if (child_type_info != .@"struct") {
                return false;
            }

            return @hasField(child_type, "data") and @hasDecl(child_type, "rows") and @hasDecl(child_type, "cols");
        }

        // Vector operations
        pub fn dot(self: *const Self, other: anytype) T {
            const OtherMatType = @TypeOf(other);

            if (comptime !isMatrixPointer(OtherMatType)) {
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
                const index = i;
                output += self.data[index] * other.data[index];
            }

            return output;
        }

        pub fn cross(self: *const Self, other: anytype) Vector(T, cols) {
            const OtherMatType = @TypeOf(other);

            if (comptime !isMatrixPointer(OtherMatType)) {
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

            const _x = self.data[1] * other.data[2] - self.data[2] * other.data[1];
            const _y = self.data[2] * other.data[0] - self.data[0] * other.data[2];
            const _z = self.data[0] * other.data[1] - self.data[1] * other.data[0];

            return Vector(T, cols).init(.{ _x, _y, _z });
        }

        pub fn mag(self: *const Self) T {
            if (comptime rows_ != 1) {
                @compileError("Magnitude is only defined for vectors");
            }

            var sum = @as(T, 0);
            inline for (self.data) |elem| {
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
            inline for (&output.data) |*elem| {
                elem.* /= magnitude;
            }

            return output;
        }

        pub fn normalize(self: *Self) void {
            if (comptime rows_ != 1) {
                @compileError("Normalization is only defined for vectors");
            }

            const magnitude = self.mag();

            inline for (&self.data) |*elem| {
                elem.* /= magnitude;
            }
        }

        // NOTE: Only implemented for 4x4 matrix
        pub fn inverse(self: *Self) Self {
            if (comptime rows_ != 4 or cols_ != 4 or rows_ != cols_) {
                @compileError("Inverse is only defined for 4x4 square matrices");
            }

            const m00 = self.data[0 * 4 + 0];
            const m01 = self.data[0 * 4 + 1];
            const m02 = self.data[0 * 4 + 2];
            const m03 = self.data[0 * 4 + 3];
            const m10 = self.data[1 * 4 + 0];
            const m11 = self.data[1 * 4 + 1];
            const m12 = self.data[1 * 4 + 2];
            const m13 = self.data[1 * 4 + 3];
            const m20 = self.data[2 * 4 + 0];
            const m21 = self.data[2 * 4 + 1];
            const m22 = self.data[2 * 4 + 2];
            const m23 = self.data[2 * 4 + 3];
            const m30 = self.data[3 * 4 + 0];
            const m31 = self.data[3 * 4 + 1];
            const m32 = self.data[3 * 4 + 2];
            const m33 = self.data[3 * 4 + 3];

            const A2323 = m22 * m33 - m23 * m32;
            const A1323 = m21 * m33 - m23 * m31;
            const A1223 = m21 * m32 - m22 * m31;
            const A0323 = m20 * m33 - m23 * m30;
            const A0223 = m20 * m32 - m22 * m30;
            const A0123 = m20 * m31 - m21 * m30;
            const A2313 = m12 * m33 - m13 * m32;
            const A1313 = m11 * m33 - m13 * m31;
            const A1213 = m11 * m32 - m12 * m31;
            const A2312 = m12 * m23 - m13 * m22;
            const A1312 = m11 * m23 - m13 * m21;
            const A1212 = m11 * m22 - m12 * m21;
            const A0313 = m10 * m33 - m13 * m30;
            const A0213 = m10 * m32 - m12 * m30;
            const A0312 = m10 * m23 - m13 * m20;
            const A0212 = m10 * m22 - m12 * m20;
            const A0113 = m10 * m31 - m11 * m30;
            const A0112 = m10 * m21 - m11 * m20;

            var det = m00 * (m11 * A2323 - m12 * A1323 + m13 * A1223) -
                m01 * (m10 * A2323 - m12 * A0323 + m13 * A0223) +
                m02 * (m10 * A1323 - m11 * A0323 + m13 * A0123) -
                m03 * (m10 * A1223 - m11 * A0223 + m12 * A0123);
            det = 1 / det;

            return Self.init(.{
                det * (m11 * A2323 - m12 * A1323 + m13 * A1223),
                det * -(m01 * A2323 - m02 * A1323 + m03 * A1223),
                det * (m01 * A2313 - m02 * A1313 + m03 * A1213),
                det * -(m01 * A2312 - m02 * A1312 + m03 * A1212),
                det * -(m10 * A2323 - m12 * A0323 + m13 * A0223),
                det * (m00 * A2323 - m02 * A0323 + m03 * A0223),
                det * -(m00 * A2313 - m02 * A0313 + m03 * A0213),
                det * (m00 * A2312 - m02 * A0312 + m03 * A0212),
                det * (m10 * A1323 - m11 * A0323 + m13 * A0123),
                det * -(m00 * A1323 - m01 * A0323 + m03 * A0123),
                det * (m00 * A1313 - m01 * A0313 + m03 * A0113),
                det * -(m00 * A1312 - m01 * A0312 + m03 * A0112),
                det * -(m10 * A1223 - m11 * A0223 + m12 * A0123),
                det * (m00 * A1223 - m01 * A0223 + m02 * A0123),
                det * -(m00 * A1213 - m01 * A0213 + m02 * A0113),
                det * (m00 * A1212 - m01 * A0212 + m02 * A0112),
            });
        }

        // Vector specific accessors
        pub fn x(self: *const Self) T {
            if (comptime rows_ != 1) {
                @compileError("x is only defined for vectors");
            }

            if (comptime cols_ < 1) {
                @compileError("x is only defined for vectors of >= 1 dimension");
            }

            // If is Quaternion
            if (comptime Self.isQuaternion()) {
                return self.data[1];
            } else {
                return self.data[0];
            }
        }

        pub fn y(self: *const Self) T {
            if (comptime rows_ != 1) {
                @compileError("y is only defined for vectors");
            }

            if (comptime cols_ < 2) {
                @compileError("y is only defined for vectors of >= 2 dimensions");
            }

            if (comptime Self.isQuaternion()) {
                return self.data[2];
            } else {
                return self.data[1];
            }
        }

        pub fn z(self: *const Self) T {
            if (comptime rows_ != 1) {
                @compileError("z is only defined for vectors");
            }

            if (comptime cols_ < 3) {
                @compileError("z is only defined for vectors of >= 3 dimensions");
            }

            if (comptime Self.isQuaternion()) {
                return self.data[3];
            } else {
                return self.data[2];
            }
        }

        pub fn w(self: *const Self) T {
            if (comptime rows_ != 1) {
                @compileError("w is only defined for vectors");
            }

            if (comptime cols_ < 4) {
                @compileError("w is only defined for vectors of >= 4 dimensions");
            }

            if (comptime Self.isQuaternion()) {
                return self.data[0];
            } else {
                return self.data[3];
            }
        }

        pub fn homogeneous(self: *const Self) Vector(T, 4) {
            if (comptime rows_ != 1 and cols_ != 3) {
                @compileError("homogeneous is only defined for vectors of 1x3 dimensions");
            }

            return Vector(T, 4).init(.{ self.x(), self.y(), self.z(), @as(T, 1) });
        }

        pub fn toQuat(self: *const Self) Vector(T, 4) {
            if (comptime rows_ != 1 and cols_ != 3) {
                @compileError("homogeneous is only defined for vectors of 1x3 dimensions");
            }

            return Vector(T, 4).init(.{ @as(T, 0), self.x(), self.y(), self.z() });
        }

        // Quaternion Operations

        pub fn conjugate(self: *const Self) Vector(T, 4) {
            if (comptime !Self.isQuaternion()) {
                @compileError("conjugate is only defined for quaternions");
            }

            return Vector(T, 4).init(.{ self.w(), -self.x(), -self.y(), -self.z() });
        }

        // fn hamilton_prod(a: vec4<f32>, b: vec4<f32>) -> vec4<f32> {
        //     return vec4<f32>(
        //         a.w * b.x + a.x * b.w + a.y * b.z - a.z * b.y,
        //         a.w * b.y - a.x * b.z + a.y * b.w + a.z * b.x,
        //         a.w * b.z + a.x * b.y - a.y * b.x + a.z * b.w,
        //         a.w * b.w - a.x * b.x - a.y * b.y - a.z * b.z,
        //     );
        // }

        // pub fn hamiltonProd(self: *const Self, vector: *const Vector(T, 4)) Vector(T, 4) {
        //     return Vector(T, 4).init(.{
        //         self.w() * vector.w() - self.x() * vector.x() - self.y() * vector.y() - self.z() * vector.z(),
        //         self.w() * vector.x() + self.x() * vector.w() + self.y() * vector.z() - self.z() * vector.y(),
        //         self.w() * vector.y() - self.x() * vector.z() + self.y() * vector.w() + self.z() * vector.x(),
        //         self.w() * vector.z() + self.x() * vector.y() - self.y() * vector.x() + self.z() * vector.w(),
        //     });
        // }

        pub fn hamiltonProd(self: *const Self, vector: *const Vector(T, 4)) Vector(T, 4) {
            return Vector(T, 4).init(.{
                self.w() * vector.x() + self.x() * vector.w() + self.y() * vector.z() - self.z() * vector.y(),
                self.w() * vector.y() - self.x() * vector.z() + self.y() * vector.w() + self.z() * vector.x(),
                self.w() * vector.z() + self.x() * vector.y() - self.y() * vector.x() + self.z() * vector.w(),
                self.w() * vector.w() - self.x() * vector.x() - self.y() * vector.y() - self.z() * vector.z(),
            });
        }

        pub fn rotateVec3(self: *const Self, vector: *const Vector(T, 3)) Vector(T, 3) {
            if (comptime !Self.isQuaternion()) {
                @compileError("rotateVec3 is only defined for quaternions");
            }

            const conj = self.conjugate();

            const temp = self.hamiltonProd(&vector.toQuat());
            const output = temp.hamiltonProd(&conj);
            return Vector(T, 3).init(.{ output.x(), output.y(), output.z() });
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

pub const Quati = Vec4i;
pub const Quatf = Vec4f;
pub const Quatd = Vec4d;

test "Matrix Creation" {
    const zero_mat = Mat4f.zero();
    std.debug.print("{f}", .{zero_mat});

    const eye_mat = Mat4f.eye();
    std.debug.print("{f}", .{eye_mat});
}

test "Matrix Adding" {
    var eye_mat = Mat4f.eye();
    const adding_mat = Mat4f.init(.{
        1.0,  2.0,  3.0,  4.0,
        5.0,  6.0,  7.0,  8.0,
        9.0,  10.0, 11.0, 12.0,
        13.0, 14.0, 15.0, 16.0,
    });

    const expected_mat = Mat4f.init(.{
        2.0,  2.0,  3.0,  4.0,
        5.0,  7.0,  7.0,  8.0,
        9.0,  10.0, 12.0, 12.0,
        13.0, 14.0, 15.0, 17.0,
    });

    const added_mat = eye_mat.elemAdd(&adding_mat);
    std.log.debug("Original: {f} Adding: {f}", .{ eye_mat, adding_mat });
    std.log.debug("Result: {f}", .{added_mat});

    try std.testing.expect(added_mat.eql(&expected_mat));

    eye_mat.elemAddTo(&adding_mat);

    try std.testing.expect(eye_mat.eql(&expected_mat));
}

test "Matrix Multiplication" {
    const mat2_f_a = Mat2f.init(.{
        1.0, 2.0,
        3.0, 4.0,
    });

    const mat2_f_b = Mat2f.init(.{
        5.0, 6.0,
        7.0, 8.0,
    });

    const output = mat2_f_a.mult(&mat2_f_b);

    const expected = Mat2f.init(.{
        19.0, 22.0,
        43.0, 50.0,
    });

    try std.testing.expect(output.eql(&expected));
}

test "Matrix Transpose" {
    const mat2_f_a = Mat2f.init(.{
        1.0, 2.0,
        3.0, 4.0,
    });

    const output = mat2_f_a.transposed();

    const expected = Mat2f.init(.{
        1.0, 3.0,
        2.0, 4.0,
    });

    try std.testing.expect(output.eql(&expected));
}

test "Vector Dot" {
    const vec_1 = Vec3f.init(.{ 1, 2, 3 });

    const vec_2 = Vec3f.init(.{ 1, 2, 3 });

    const output = vec_1.dot(&vec_2);
    std.debug.print("Output: {}\n", .{output});

    try std.testing.expect(output == 14);
}

test "Vector Cross" {
    const vec_1 = Vec3f.init(.{ 1, 2, 3 });
    const vec_2 = Vec3f.init(.{ 4, 5, 6 });

    const expected = Vec3f.init(.{ -3, 6, -3 });

    const output = vec_1.cross(&vec_2);
    std.debug.print("Output: {f}\n", .{output});

    try std.testing.expect(output.eql(&expected));
}

test "Vector Normalization" {
    const ERROR = 1e-6;

    const vec = Vec3f.init(.{ 1, 2, 3 });
    const normalized = vec.norm();

    std.debug.print("Input {f}\n", .{vec});
    std.debug.print("Output {f}\n", .{normalized});

    const mag = normalized.mag();
    std.debug.print("Magnitude: {}\n", .{mag});
    try std.testing.expect(mag >= 1.0 - ERROR and mag <= 1.0 + ERROR);
}

test "Vector Normalization In Place" {
    const ERROR = 1e-6;

    var vec = Vec3f.init(.{ 1, 2, 3 });

    std.debug.print("Input {f}\n", .{vec});
    vec.normalize();
    std.debug.print("Output {f}\n", .{vec});

    const mag = vec.mag();
    std.debug.print("Magnitude: {}\n", .{mag});
    try std.testing.expect(mag >= 1.0 - ERROR and mag <= 1.0 + ERROR);
}

test "Scalar Multiplication" {
    var vec = Vec3f.init(.{ 1, 2, 3 });

    std.debug.print("Input {f}\n", .{vec});
    const multed = vec.scalarMult(2);
    vec.scalarMultTo(2);
    std.debug.print("Output {f}\n", .{vec});
    std.debug.print("Output {f}\n", .{multed});

    const expected = Vec3f.init(.{ 2, 4, 6 });

    try std.testing.expect(multed.eql(&expected));
    try std.testing.expect(vec.eql(&expected));
}

test "isQuaternion" {
    try std.testing.expect(Quatf.isQuaternion() == true);
    try std.testing.expect(Vec3f.isQuaternion() == false);
}

test "Quat Accessors" {
    const vec = Vec3f.init(.{ 0.0, 1.0, 2.0 });
    const quat = Quatf.init(.{ 0.0, 1.0, 2.0, 3.0 });

    std.debug.print("Vec: {} {} {}\n", .{ vec.x(), vec.y(), vec.z() });
    try std.testing.expect(vec.x() == 0.0);
    try std.testing.expect(vec.y() == 1.0);
    try std.testing.expect(vec.z() == 2.0);

    std.debug.print("Quat: {} {} {} {}\n", .{ quat.w(), quat.x(), quat.y(), quat.z() });
    try std.testing.expect(quat.w() == 0.0);
    try std.testing.expect(quat.x() == 1.0);
    try std.testing.expect(quat.y() == 2.0);
    try std.testing.expect(quat.z() == 3.0);
}

test "Vector Rotation" {
    const vec = Vec3f.init(.{ 1.0, 0.0, 0.0 });
    const quat = Quatf.init(.{
        @cos(std.math.pi / 4.0),
        0.0,
        0.0,
        @sin(std.math.pi / 4.0),
    });

    const expected = Vec3f.init(.{ 0.0, 1.0, 0.0 });

    const rotated = quat.rotateVec3(&vec);
    std.debug.print("Expected: {f}\n", .{expected});
    std.debug.print("Actual: {f}\n", .{rotated});

    try std.testing.expect(rotated.eql(&expected));
}

test "Inverse Matrix" {
    var eye_mat = Mat4f.eye();
    const inv = eye_mat.inverse();

    std.debug.print("Eye Mat: {f}\n", .{eye_mat});
    std.debug.print("Inverse Mat: {f}\n", .{inv});

    try std.testing.expect(eye_mat.eql(&inv));

    var new_mat = Mat4f.init(.{ 1.0, 3.0, 5.0, 9.0, 1.0, 3.0, 1.0, 7.0, 4.0, 3.0, 9.0, 7.0, 5.0, 2.0, 0.0, 9.0 });
    const new_inv = new_mat.inverse();

    std.debug.print("New Mat: {f}\n", .{new_mat});
    std.debug.print("New Inve Mat: {f}\n", .{new_inv});
}
