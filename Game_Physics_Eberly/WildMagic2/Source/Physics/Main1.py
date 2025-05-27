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


# --- RigidBody Class Conversion ---

class RigidBody:
    """
    Python equivalent of the Wml::RigidBody class.
    Uses placeholder Vector3, Matrix3, and Quaternion classes.
    """

    def __init__(self):
        # Constant quantities (matrices in body coordinates)
        self.m_fMass = float('inf')
        self.m_fInvMass = 0.0
        self.m_kInertia = Matrix3.identity()
        self.m_kInvInertia = Matrix3.zero()

        # State variables
        self.m_kPos = Vector3.zero()          # position
        self.m_kQOrient = Quaternion.identity()  # orientation
        self.m_kLinMom = Vector3.zero()        # linear momentum
        self.m_kAngMom = Vector3.zero()        # angular momentum

        # Derived state variables
        self.m_kROrient = Matrix3.identity()   # orientation matrix
        self.m_kLinVel = Vector3.zero()        # linear velocity
        self.m_kAngVel = Vector3.zero()        # angular velocity

        self.m_bMoved = False

        # External force/torque at current time of simulation
        self.m_kExternalForce = Vector3.zero()
        self.m_kExternalTorque = Vector3.zero()

        # Resting contact force/torque.
        self.m_kInternalForce = Vector3.zero()
        self.m_kInternalTorque = Vector3.zero()

        # Force and Torque functions
        self.force_func = self._default_force
        self.torque_func = self._default_torque

    def _default_force(self, t, mass, pos, q_orient, lin_mom, ang_mom, r_orient, lin_vel, ang_vel):
        """Default force function (returns zero force)."""
        return Vector3.zero()

    def _default_torque(self, t, mass, pos, q_orient, lin_mom, ang_mom, r_orient, lin_vel, ang_vel):
        """Default torque function (returns zero torque)."""
        return Vector3.zero()

    def set_force_function(self, func):
        self.force_func = func

    def set_torque_function(self, func):
        self.torque_func = func

    def position(self):
        return self.m_kPos

    def set_mass(self, f_mass):
        if 0.0 < f_mass < float('inf'):
            self.m_fMass = f_mass
            self.m_fInvMass = 1.0 / f_mass
        else:
            # Assume the body is immovable
            self.m_fMass = float('inf')
            self.m_fInvMass = 0.0
            self.m_kInertia = Matrix3.identity()
            self.m_kInvInertia = Matrix3.zero()
            self.m_kQOrient = Quaternion.identity()
            self.m_kLinMom = Vector3.zero()
            self.m_kAngMom = Vector3.zero()
            self.m_kROrient = Matrix3.identity()
            self.m_kLinVel = Vector3.zero()
            self.m_kAngVel = Vector3.zero()

    def set_body_inertia(self, rk_inertia):
        self.m_kInertia = rk_inertia
        self.m_kInvInertia = self.m_kInertia.inverse()

    def set_position(self, rk_pos):
        self.m_kPos = rk_pos

    def set_q_orientation(self, rk_q_orient):
        self.m_kQOrient = rk_q_orient
        self.m_kQOrient.to_rotation_matrix(self.m_kROrient)

    def set_linear_momentum(self, rk_lin_mom):
        self.m_kLinMom = rk_lin_mom
        self.m_kLinVel = self.m_fInvMass * self.m_kLinMom

    def set_angular_momentum(self, rk_ang_mom):
        self.m_kAngMom = rk_ang_mom
        self.m_kAngVel = self.m_kROrient * self.m_kInvInertia * self.m_kROrient.transpose() * self.m_kAngMom

    def set_r_orientation(self, rk_r_orient):
        self.m_kROrient = rk_r_orient
        self.m_kQOrient.from_rotation_matrix(self.m_kROrient)

    def set_linear_velocity(self, rk_lin_vel):
        self.m_kLinVel = rk_lin_vel
        self.m_kLinMom = self.m_fMass * self.m_kLinVel

    def set_angular_velocity(self, rk_ang_vel):
        self.m_kAngVel = rk_ang_vel
        self.m_kAngMom = self.m_kROrient * self.m_kInertia * self.m_kROrient.transpose() * self.m_kAngVel

    def get_mass(self):
        return self.m_fMass

    def get_inverse_mass(self):
        return self.m_fInvMass

    def get_body_inertia(self):
        return self.m_kInertia

    def get_body_inverse_inertia(self):
        return self.m_kInvInertia

    def get_world_inertia(self):
        return self.m_kROrient * self.m_kInertia * self.m_kROrient.transpose()

    def get_world_inverse_inertia(self):
        return self.m_kROrient * self.m_kInvInertia * self.m_kROrient.transpose()

    def get_position(self):
        return self.m_kPos

    def get_q_orientation(self):
        return self.m_kQOrient

    def get_linear_momentum(self):
        return self.m_kLinMom

    def get_angular_momentum(self):
        return self.m_kAngMom

    def get_r_orientation(self):
        return self.m_kROrient

    def get_linear_velocity(self):
        return self.m_kLinVel

    def get_angular_velocity(self):
        return self.m_kAngVel

    def set_internal_force(self, rk_force):
        self.m_kInternalForce = rk_force

    def set_internal_torque(self, rk_torque):
        self.m_kInternalTorque = rk_torque

    def set_external_force(self, rk_force):
        self.m_kExternalForce = rk_force

    def set_external_torque(self, rk_torque):
        self.m_kExternalTorque = rk_torque

    def append_internal_force(self, rk_force):
        self.m_kInternalForce += rk_force

    def append_internal_torque(self, rk_torque):
        self.m_kInternalTorque += rk_torque

    def update(self, f_t, f_dt):
        f_half_dt = 0.5 * f_dt
        f_sixth_dt = f_dt / 6.0
        f_tp_half_dt = f_t + f_half_dt
        f_tp_dt = f_t + f_dt

        # A1 = G(T,S0), B1 = S0 + (DT/2)*A1
        kA1DXDT = self.m_kLinVel
        kW = Quaternion(0.0, self.m_kAngVel.x, self.m_kAngVel.y, self.m_kAngVel.z)
        kA1DQDT = 0.5 * kW * self.m_kQOrient
        kA1DPDT = self.m_kExternalForce + self.m_kInternalForce
        kA1DLDT = self.m_kExternalTorque + self.m_kInternalTorque
        
        self.m_kInternalForce = Vector3.zero()
        self.m_kInternalTorque = Vector3.zero()

        kNewPos_B1 = self.m_kPos + f_half_dt * kA1DXDT
        kNewQOrient_B1 = self.m_kQOrient + f_half_dt * kA1DQDT
        kNewLinMom_B1 = self.m_kLinMom + f_half_dt * kA1DPDT
        kNewAngMom_B1 = self.m_kAngMom + f_half_dt * kA1DLDT
        
        kNewROrient_B1 = Matrix3.identity() # Create a new matrix for this step
        kNewQOrient_B1.to_rotation_matrix(kNewROrient_B1)
        
        kNewLinVel_B1 = self.m_fInvMass * kNewLinMom_B1
        kNewAngVel_B1 = kNewROrient_B1 * self.m_kInvInertia * kNewROrient_B1.transpose() * kNewAngMom_B1

        # A2 = G(T+DT/2,B1), B2 = S0 + (DT/2)*A2
        kA2DXDT = kNewLinVel_B1
        kW = Quaternion(0.0, kNewAngVel_B1.x, kNewAngVel_B1.y, kNewAngVel_B1.z)
        kA2DQDT = 0.5 * kW * kNewQOrient_B1
        kA2DPDT = self.force_func(f_tp_half_dt, self.m_fMass, kNewPos_B1, kNewQOrient_B1,
                                  kNewLinMom_B1, kNewAngMom_B1, kNewROrient_B1,
                                  kNewLinVel_B1, kNewAngVel_B1)
        kA2DLDT = self.torque_func(f_tp_half_dt, self.m_fMass, kNewPos_B1, kNewQOrient_B1,
                                   kNewLinMom_B1, kNewAngMom_B1, kNewROrient_B1,
                                   kNewLinVel_B1, kNewAngVel_B1)
        
        kNewPos_B2 = self.m_kPos + f_half_dt * kA2DXDT
        kNewQOrient_B2 = self.m_kQOrient + f_half_dt * kA2DQDT
        kNewLinMom_B2 = self.m_kLinMom + f_half_dt * kA2DPDT
        kNewAngMom_B2 = self.m_kAngMom + f_half_dt * kA2DLDT
        
        kNewROrient_B2 = Matrix3.identity()
        kNewQOrient_B2.to_rotation_matrix(kNewROrient_B2)
        
        kNewLinVel_B2 = self.m_fInvMass * kNewLinMom_B2
        kNewAngVel_B2 = kNewROrient_B2 * self.m_kInvInertia * kNewROrient_B2.transpose() * kNewAngMom_B2

        # A3 = G(T+DT/2,B2), B3 = S0 + DT*A3
        kA3DXDT = kNewLinVel_B2
        kW = Quaternion(0.0, kNewAngVel_B2.x, kNewAngVel_B2.y, kNewAngVel_B2.z)
        kA3DQDT = 0.5 * kW * kNewQOrient_B2
        kA3DPDT = self.force_func(f_tp_half_dt, self.m_fMass, kNewPos_B2, kNewQOrient_B2,
                                  kNewLinMom_B2, kNewAngMom_B2, kNewROrient_B2,
                                  kNewLinVel_B2, kNewAngVel_B2)
        kA3DLDT = self.torque_func(f_tp_half_dt, self.m_fMass, kNewPos_B2, kNewQOrient_B2,
                                   kNewLinMom_B2, kNewAngMom_B2, kNewROrient_B2,
                                   kNewLinVel_B2, kNewAngVel_B2)
        
        kNewPos_B3 = self.m_kPos + f_dt * kA3DXDT
        kNewQOrient_B3 = self.m_kQOrient + f_dt * kA3DQDT
        kNewLinMom_B3 = self.m_kLinMom + f_dt * kA3DPDT
        kNewAngMom_B3 = self.m_kAngMom + f_dt * kA3DLDT
        
        kNewROrient_B3 = Matrix3.identity()
        kNewQOrient_B3.to_rotation_matrix(kNewROrient_B3)
        
        kNewLinVel_B3 = self.m_fInvMass * kNewLinMom_B3
        kNewAngVel_B3 = kNewROrient_B3 * self.m_kInvInertia * kNewROrient_B3.transpose() * kNewAngMom_B3

        # A4 = G(T+DT,B3), S1 = S0 + (DT/6)*(A1+2*(A2+A3)+A4)
        kA4DXDT = kNewLinVel_B3
        kW = Quaternion(0.0, kNewAngVel_B3.x, kNewAngVel_B3.y, kNewAngVel_B3.z)
        kA4DQDT = 0.5 * kW * kNewQOrient_B3
        kA4DPDT = self.force_func(f_tp_dt, self.m_fMass, kNewPos_B3, kNewQOrient_B3,
                                  kNewLinMom_B3, kNewAngMom_B3, kNewROrient_B3,
                                  kNewLinVel_B3, kNewAngVel_B3)
        kA4DLDT = self.torque_func(f_tp_dt, self.m_fMass, kNewPos_B3, kNewQOrient_B3,
                                   kNewLinMom_B3, kNewAngMom_B3, kNewROrient_B3,
                                   kNewLinVel_B3, kNewAngVel_B3)
        
        self.m_kPos += f_sixth_dt * (kA1DXDT + 2.0 * (kA2DXDT + kA3DXDT) + kA4DXDT)
        self.m_kQOrient += f_sixth_dt * (kA1DQDT + 2.0 * (kA2DQDT + kA3DQDT) + kA4DQDT)
        self.m_kLinMom += f_sixth_dt * (kA1DPDT + 2.0 * (kA2DPDT + kA3DPDT) + kA4DPDT)
        self.m_kAngMom += f_sixth_dt * (kA1DLDT + 2.0 * (kA2DLDT + kA3DLDT) + kA4DLDT)
        
        self.m_kQOrient.to_rotation_matrix(self.m_kROrient)
        self.m_kLinVel = self.m_fInvMass * self.m_kLinMom
        self.m_kAngVel = self.m_kROrient * self.m_kInvInertia * self.m_kROrient.transpose() * self.m_kAngMom

        # Make force and torque correspond to new time T+DT
        self.m_kExternalForce = self.force_func(f_tp_dt, self.m_fMass, self.m_kPos,
                                                self.m_kQOrient, self.m_kLinMom,
                                                self.m_kAngMom, self.m_kROrient,
                                                self.m_kLinVel, self.m_kAngVel)
        self.m_kExternalTorque = self.torque_func(f_tp_dt, self.m_fMass, self.m_kPos,
                                                   self.m_kQOrient, self.m_kLinMom,
                                                   self.m_kAngMom, self.m_kROrient,
                                                   self.m_kLinVel, self.m_kAngVel)

    def moved(self):
        return self.m_bMoved

# --- Example Usage (equivalent to main.cpp) ---

if __name__ == "__main__":
    print("Hello World!")

    t = 0.0
    dt = 0.01

    # Using the placeholder classes
    x0 = Vector3.zero()
    q0 = Quaternion(4.0, 3.0, 2.0, 1.0) # Quaternion(w, x, y, z)
    lin_mom0 = Vector3.zero()
    ang_mom0 = Vector3.zero()
    ext_force0 = Vector3(3.0, 3.0, 3.0)

    rb = RigidBody()

    # Set initial state
    rb.set_position(x0)
    rb.set_q_orientation(q0)
    rb.set_linear_momentum(lin_mom0)
    rb.set_angular_momentum(ang_mom0)
    rb.set_external_force(ext_force0)
    rb.set_mass(1.0) # Set a non-infinite mass for movement

    # Define a custom force function for the example
    # This function will be called by the update method
    def my_force_func(t_val, mass, pos, q_orient, lin_mom, ang_mom, r_orient, lin_vel, ang_vel):
        # Example: Constant force as defined by ext_force0 in main.
        # In a real simulation, this would calculate forces based on physics.
        return ext_force0
    
    rb.set_force_function(my_force_func)

    print("\n--- Simulation Start ---")
    for i in range(10):
        # The C++ code had a redundant SetExternalForce(extForce0) inside the loop for i==0.
        # In this Python version, it's set once, and the force_func will use it.
        rb.update(t, dt)
        t += dt
        print(f"Time: {t:.2f}, Position: {rb.get_position()}, Linear Velocity: {rb.get_linear_velocity()}")
    print("--- Simulation End ---")
