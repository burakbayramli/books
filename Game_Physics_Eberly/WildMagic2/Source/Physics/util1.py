import math

# --- Placeholder Classes for Wml (Vector3, Matrix3, Quaternion) ---
# In a real-world Python application, you would typically use a library
# like NumPy for these mathematical operations for performance and functionality.

class Vector3:
    """Simplified placeholder for Wml::Vector3."""
    def __init__(self, x=0.0, y=0.0, z=0.0):
        self.x = float(x)
        self.y = float(y)
        self.z = float(z)

    def __add__(self, other):
        return Vector3(self.x + other.x, self.y + other.y, self.z + other.z)

    def __mul__(self, scalar):
        return Vector3(self.x * scalar, self.y * scalar, self.z * scalar)

    def __rmul__(self, scalar):
        return self.__mul__(scalar) # Allow scalar * Vector3

    def __str__(self):
        return f"Vector3({self.x}, {self.y}, {self.z})"

    def __repr__(self):
        return self.__str__()

    def dot(self, other):
        return self.x * other.x + self.y * other.y + self.z * other.z

    def cross(self, other):
        return Vector3(
            self.y * other.z - self.z * other.y,
            self.z * other.x - self.x * other.z,
            self.x * other.y - self.y * other.x
        )

    @staticmethod
    def zero():
        return Vector3(0.0, 0.0, 0.0)

class Matrix3:
    """Simplified placeholder for Wml::Matrix3."""
    def __init__(self, m=None):
        if m is None:
            self.m = [[0.0 for _ in range(3)] for _ in range(3)]
        else:
            # Expect a 3x3 list of lists or similar structure
            self.m = [[float(val) for val in row] for row in m]

    def __mul__(self, other):
        if isinstance(other, Vector3):
            # Matrix-Vector multiplication
            x = self.m[0][0] * other.x + self.m[0][1] * other.y + self.m[0][2] * other.z
            y = self.m[1][0] * other.x + self.m[1][1] * other.y + self.m[1][2] * other.z
            z = self.m[2][0] * other.x + self.m[2][1] * other.y + self.m[2][2] * other.z
            return Vector3(x, y, z)
        elif isinstance(other, Matrix3):
            # Matrix-Matrix multiplication
            result = Matrix3()
            for i in range(3):
                for j in range(3):
                    for k in range(3):
                        result.m[i][j] += self.m[i][k] * other.m[k][j]
            return result
        else:
            raise TypeError("Unsupported operand type for Matrix3 multiplication")

    def transpose(self):
        result = Matrix3()
        for i in range(3):
            for j in range(3):
                result.m[i][j] = self.m[j][i]
        return result

    def inverse(self):
        # Very simplified inverse for demonstration.
        # A robust inverse would use more advanced linear algebra.
        det = (self.m[0][0] * (self.m[1][1] * self.m[2][2] - self.m[1][2] * self.m[2][1]) -
               self.m[0][1] * (self.m[1][0] * self.m[2][2] - self.m[1][2] * self.m[2][0]) +
               self.m[0][2] * (self.m[1][0] * self.m[2][1] - self.m[1][1] * self.m[2][0]))
        
        if det == 0:
            # Handle singular matrix, return a zero matrix or raise error
            return Matrix3.zero()
        
        inv_det = 1.0 / det
        
        result = Matrix3()
        result.m[0][0] = (self.m[1][1] * self.m[2][2] - self.m[1][2] * self.m[2][1]) * inv_det
        result.m[0][1] = (self.m[0][2] * self.m[2][1] - self.m[0][1] * self.m[2][2]) * inv_det
        result.m[0][2] = (self.m[0][1] * self.m[1][2] - self.m[0][2] * self.m[1][1]) * inv_det
        result.m[1][0] = (self.m[1][2] * self.m[2][0] - self.m[1][0] * self.m[2][2]) * inv_det
        result.m[1][1] = (self.m[0][0] * self.m[2][2] - self.m[0][2] * self.m[2][0]) * inv_det
        result.m[1][2] = (self.m[0][2] * self.m[1][0] - self.m[0][0] * self.m[1][2]) * inv_det
        result.m[2][0] = (self.m[1][0] * self.m[2][1] - self.m[1][1] * self.m[2][0]) * inv_det
        result.m[2][1] = (self.m[0][1] * self.m[2][0] - self.m[0][0] * self.m[2][1]) * inv_det
        result.m[2][2] = (self.m[0][0] * self.m[1][1] - self.m[0][1] * self.m[1][0]) * inv_det
        return result

    @staticmethod
    def identity():
        return Matrix3([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]])

    @staticmethod
    def zero():
        return Matrix3([[0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0]])

    def __str__(self):
        return f"Matrix3(\n{self.m[0]}\n{self.m[1]}\n{self.m[2]})"

    def __repr__(self):
        return self.__str__()


class Quaternion:
    """Simplified placeholder for Wml::Quaternion."""
    def __init__(self, w=1.0, x=0.0, y=0.0, z=0.0):
        self.w = float(w)
        self.x = float(x)
        self.y = float(y)
        self.z = float(z)
        self._normalize() # Normalize on initialization

    def _normalize(self):
        magnitude = math.sqrt(self.w**2 + self.x**2 + self.y**2 + self.z**2)
        if magnitude > 0:
            self.w /= magnitude
            self.x /= magnitude
            self.y /= magnitude
            self.z /= magnitude

    def __mul__(self, other):
        if isinstance(other, Quaternion):
            # Quaternion multiplication (Hamilton product)
            w = self.w * other.w - self.x * other.x - self.y * other.y - self.z * other.z
            x = self.w * other.x + self.x * other.w + self.y * other.z - self.z * other.y
            y = self.w * other.y - self.x * other.z + self.y * other.w + self.z * other.x
            z = self.w * other.z + self.x * other.y - self.y * other.x + self.z * other.w
            return Quaternion(w, x, y, z)
        elif isinstance(other, (int, float)):
            return Quaternion(self.w * other, self.x * other, self.y * other, self.z * other)
        else:
            raise TypeError("Unsupported operand type for Quaternion multiplication")

    def __rmul__(self, scalar):
        return self.__mul__(scalar)

    def __add__(self, other):
        return Quaternion(self.w + other.w, self.x + other.x, self.y + other.y, self.z + other.z)

    def to_rotation_matrix(self, matrix_out):
        # Convert quaternion to rotation matrix
        # Assumes matrix_out is a pre-allocated Matrix3 object
        xx = self.x * self.x
        xy = self.x * self.y
        xz = self.x * self.z
        xw = self.x * self.w
        yy = self.y * self.y
        yz = self.y * self.z
        yw = self.y * self.w
        zz = self.z * self.z
        zw = self.z * self.w

        matrix_out.m[0][0] = 1.0 - 2.0 * (yy + zz)
        matrix_out.m[0][1] = 2.0 * (xy - zw)
        matrix_out.m[0][2] = 2.0 * (xz + yw)
        matrix_out.m[1][0] = 2.0 * (xy + zw)
        matrix_out.m[1][1] = 1.0 - 2.0 * (xx + zz)
        matrix_out.m[1][2] = 2.0 * (yz - xw)
        matrix_out.m[2][0] = 2.0 * (xz - yw)
        matrix_out.m[2][1] = 2.0 * (yz + xw)
        matrix_out.m[2][2] = 1.0 - 2.0 * (xx + yy)

    def from_rotation_matrix(self, matrix_in):
        # Convert rotation matrix to quaternion
        m = matrix_in.m
        trace = m[0][0] + m[1][1] + m[2][2]
        
        if trace > 0:
            s = 0.5 / math.sqrt(trace + 1.0)
            self.w = 0.25 / s
            self.x = (m[2][1] - m[1][2]) * s
            self.y = (m[0][2] - m[2][0]) * s
            self.z = (m[1][0] - m[0][1]) * s
        elif (m[0][0] > m[1][1]) and (m[0][0] > m[2][2]):
            s = 2.0 * math.sqrt(1.0 + m[0][0] - m[1][1] - m[2][2])
            self.w = (m[2][1] - m[1][2]) / s
            self.x = 0.25 * s
            self.y = (m[0][1] + m[1][0]) / s
            self.z = (m[0][2] + m[2][0]) / s
        elif m[1][1] > m[2][2]:
            s = 2.0 * math.sqrt(1.0 + m[1][1] - m[0][0] - m[2][2])
            self.w = (m[0][2] - m[2][0]) / s
            self.x = (m[0][1] + m[1][0]) / s
            self.y = 0.25 * s
            self.z = (m[1][2] + m[2][1]) / s
        else:
            s = 2.0 * math.sqrt(1.0 + m[2][2] - m[0][0] - m[1][1])
            self.w = (m[1][0] - m[0][1]) / s
            self.x = (m[0][2] + m[2][0]) / s
            self.y = (m[1][2] + m[2][1]) / s
            self.z = 0.25 * s
        self._normalize() # Ensure normalization after conversion

    @staticmethod
    def identity():
        return Quaternion(1.0, 0.0, 0.0, 0.0)

    @staticmethod
    def zero():
        return Quaternion(0.0, 0.0, 0.0, 0.0)

    def __str__(self):
        return f"Quaternion(w={self.w}, x={self.x}, y={self.y}, z={self.z})"

    def __repr__(self):
        return self.__str__()
    
