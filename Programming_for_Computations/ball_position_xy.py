def y(v0y, t):
    g = 9.81                  # Acceleration of gravity
    return v0y*t - 0.5*g*t**2

def x(v0x, t):
    return v0x*t

initial_velocity_x = 2.0
initial_velocity_y = 5.0

time = 0.6       # Just pick one point in time
print x(initial_velocity_x, time), y(initial_velocity_y, time)
time = 0.9       # ... Pick another point in time
print x(initial_velocity_x, time), y(initial_velocity_y, time)
