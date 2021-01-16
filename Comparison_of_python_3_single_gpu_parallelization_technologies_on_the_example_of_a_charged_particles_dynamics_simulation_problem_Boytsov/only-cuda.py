# only CUDA code
import pycuda.driver as cuda
from jinja2 import Template
from enum import Enum, auto
import numpy as np, pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import rayleigh, norm, kstest
from time import perf_counter
import math

cuda.init()

from pycuda.tools import make_default_context

from pycuda.compiler import SourceModule

from collections import namedtuple

Data = namedtuple('Data', 'charges masses pos vel')

SplitData = namedtuple('SplitData', 'charges masses p0 p1 v0 v1')

def run_simulation(generator, solver, compound_step=True, scale=16,
         time_steps=500, save_step=100):
  dt, n, size, data = generator.gen_on_scale(scale)
  frame = pd.DataFrame(solver.simulate(dt, n, size, time_steps, save_step, compound_step, data), columns=["t", "time", "perf", "data"])
  return frame

def gen_pos(n, size):
  steps = math.ceil(math.sqrt(n))
  full_grid = np.mgrid[0:steps, 0:steps].reshape(2, steps*steps).transpose()
  n_grid_points = np.ascontiguousarray(full_grid[:n])
  return size/steps * (n_grid_points+0.5)

def gen_vel(n, max_vel):
  return (np.random.rand(n, 2) - 0.5)*max_vel

class TaskGenerator:
  def __init__(self, pos_t=np.float32, vel_t=np.float32, time_t=np.float32,
               mass_t=np.float32, charge_t=np.float32, scalar=False,
               dist=10, max_vel=1, mass=2, charge=1, dt = 0.1):
    self.dist = pos_t(dist)
    self.max_vel = vel_t(max_vel)
    self.mass = mass_t(mass)
    self.charge = charge_t(charge)
    self.dt = time_t(dt)
    self.scalar = scalar
    
  def __repr__(self):
    typed = lambda x: f"{type(x)}({x})"
    params = ', '.join(f'{s}={typed(getattr(self, s))}' for s in ('dist', 'max_vel', 'mass', 'charge', 'dt'))
    return f"{self.__class__}({params}, {'scalar' if self.scalar else 'vector'})"

  def __str__(self):
    typed = lambda x: f"{type(x).__name__}({x:.3f})"
    params = ', '.join(f'{s}={typed(getattr(self, s))}' for s in ('dist', 'max_vel', 'mass', 'charge', 'dt'))
    return f"{self.__class__.__name__}({params}, {'scalar' if self.scalar else 'vector'})"
  
  @staticmethod
  def gen_data(n, size, max_vel, charge, mass, scalar):
    pos = gen_pos(n, size).astype(type(size))
    vel = gen_vel(n, max_vel).astype(type(max_vel))
    charges = charge if scalar else np.full(n, charge)
    masses = mass if scalar else np.full(n, mass)
    return Data(charges, masses, pos, vel)

  def gen_on_scale(self, scale):
    n = scale*scale
    size = type(self.dist)(self.dist * scale)
    return self.dt, n, size, self.gen_data(n, size, self.max_vel, self.charge, self.mass, self.scalar)


class CoordFormat(Enum):
  nx = auto()
  xn = auto()
  split = auto()

f=np.float32

class Solver:
  no_blocks = False
  perf_good_batch_crits = (24, 64)
  tpl = Template("")
  
  def __init__(self, coord_format=CoordFormat.xn, scalar=False, threadsperblock=64, types={}):
    self.format = coord_format
    self.scalar = scalar
    self.threadsperblock = threadsperblock
    f = np.float32
    self.types = {s: f for s in ("pos_t", "vel_t", "time_t", "charge_t", "mass_t")}
    self.types.update(types)
    self.source = self.tpl.render(format=coord_format.name, scalar=scalar,
                                  **{k: v.__name__ for k, v in self.types.items()})
   
  def __repr__(self):
    return f"{self.__class__.__name__}({self.format.name}, {self.threadsperblock})"
  
  def _to_device(self, host_array):
    return host_array.copy()

  def _to_host(self, dev_array, host_array):
    return dev_array.copy()
    
  def call(self, foo, *args, **kwargs):
    return foo(*args, **kwargs)
  
  @staticmethod
  def update(self, *args, **kwargs):
    raise NotImplementedError()

  @staticmethod
  def update_v(self, *args, **kwargs):
    raise NotImplementedError()
  
  @staticmethod
  def update_p(self, *args, **kwargs):
    raise NotImplementedError()
  
  @staticmethod
  def rebound(self, *args, **kwargs):
    raise NotImplementedError()
        
  def to_device(self, host_value):
    return self._to_device(np.ascontiguousarray(host_value)) if isinstance(host_value, np.ndarray) else host_value

  def to_host(self, dev_value, host_value):
    return self._to_host(dev_value, host_value) if isinstance(host_value, np.ndarray) else dev_value

  def to_device_all(self, host_data):
    return type(host_data)(*(self.to_device(x) for x in host_data))
    
  def to_host_all(self, dev_data, host_data):
    return type(host_data)(*(self.to_host(a, b) for a, b in zip(dev_data, host_data)))
  
  def to_device_layout(self, data):
    if self.format == CoordFormat.nx:
      return Data(*data)
    elif self.format == CoordFormat.xn:
      return Data(data.charges, data.masses, data.pos.transpose(), data.vel.transpose())
    elif self.format == CoordFormat.split:
      return SplitData(data.charges, data.masses, data.pos[:, 0], data.pos[:, 1], data.vel[:, 0], data.vel[:, 1])
    
  def to_host_layout(self, data):
    if self.format == CoordFormat.nx:
      return Data(*data)
    elif self.format == CoordFormat.xn:
      return Data(data.charges, data.masses, data.pos.transpose(), data.vel.transpose())
    elif self.format == CoordFormat.split:
      return Data(data.charges, data.masses, np.stack((data.p0, data.p1), axis=1), np.stack((data.v0, data.v1), axis=1))
    
  def step(self, dt, size, n, dd, compound_step):
    if self.format == CoordFormat.split:
      if compound_step:
        self.call(self.update_v, dd.p0, dd.p1, dd.v0, dd.v1, dt, dd.charges, dd.masses, n)
        self.call(self.update_p, dd.p0, dd.p1, dd.v0, dd.v1, dt, n)
        self.call(self.rebound, dd.p0, dd.p1, dd.v0, dd.v1, size, n)
      else:
        self.call(self.update, dd.p0, dd.p1, dd.v0, dd.v1, dt, size, dd.charges, dd.masses, n)
    else:
      if compound_step:
        self.call(self.update_v, dd.pos, dd.vel, dt, dd.charges, dd.masses, n)
        self.call(self.update_p, dd.pos, dd.vel, dt, n)
        self.call(self.rebound, dd.pos, dd.vel, size, n)
      else:
        self.call(self.update, dd.pos, dd.vel, dt, size, dd.charges, dd.masses, n) 
  
  def simulate(self, dt, n, size, time_steps, save_step, compound_step, data):
    self.blockspergrid = math.ceil(n / self.threadsperblock)
    batches = [save_step]*(time_steps//save_step) + [time_steps%save_step]
    ti = 0
    host_data = self.to_device_layout(data)
    dev_data = self.to_device_all(host_data)
    host_data = self.to_host_all(dev_data, host_data)
    yield (ti, ti*dt, perf_counter(), self.to_host_layout(host_data))
    for batch_size in batches:
      if batch_size==0: # skip last (remainder) batch if it's not needed
        continue 
      for i in range(batch_size):
        self.step(dt, size, np.int32(n), dev_data, compound_step)
      host_data = self.to_host_all(dev_data, host_data)
      ti += batch_size;
      yield (ti, ti*dt, perf_counter(), self.to_host_layout(host_data))


class Solver:
  no_blocks = False
  perf_good_batch_crits = (24, 64)
  tpl = Template("")
  
  def __init__(self, coord_format=CoordFormat.xn, scalar=False, threadsperblock=64, types={}):
    self.format = coord_format
    self.scalar = scalar
    self.threadsperblock = threadsperblock
    f = np.float32
    self.types = {s: f for s in ("pos_t", "vel_t", "time_t", "charge_t", "mass_t")}
    self.types.update(types)
    self.source = self.tpl.render(format=coord_format.name, scalar=scalar,
                                  **{k: v.__name__ for k, v in self.types.items()})
   
  def __repr__(self):
    return f"{self.__class__.__name__}({self.format.name}, {self.threadsperblock})"
  
  def _to_device(self, host_array):
    return host_array.copy()

  def _to_host(self, dev_array, host_array):
    return dev_array.copy()
    
  def call(self, foo, *args, **kwargs):
    return foo(*args, **kwargs)
  
  @staticmethod
  def update(self, *args, **kwargs):
    raise NotImplementedError()

  @staticmethod
  def update_v(self, *args, **kwargs):
    raise NotImplementedError()
  
  @staticmethod
  def update_p(self, *args, **kwargs):
    raise NotImplementedError()
  
  @staticmethod
  def rebound(self, *args, **kwargs):
    raise NotImplementedError()
        
  def to_device(self, host_value):
    return self._to_device(np.ascontiguousarray(host_value)) if isinstance(host_value, np.ndarray) else host_value

  def to_host(self, dev_value, host_value):
    return self._to_host(dev_value, host_value) if isinstance(host_value, np.ndarray) else dev_value

  def to_device_all(self, host_data):
    return type(host_data)(*(self.to_device(x) for x in host_data))
    
  def to_host_all(self, dev_data, host_data):
    return type(host_data)(*(self.to_host(a, b) for a, b in zip(dev_data, host_data)))
  
  def to_device_layout(self, data):
    if self.format == CoordFormat.nx:
      return Data(*data)
    elif self.format == CoordFormat.xn:
      return Data(data.charges, data.masses, data.pos.transpose(), data.vel.transpose())
    elif self.format == CoordFormat.split:
      return SplitData(data.charges, data.masses, data.pos[:, 0], data.pos[:, 1], data.vel[:, 0], data.vel[:, 1])
    
  def to_host_layout(self, data):
    if self.format == CoordFormat.nx:
      return Data(*data)
    elif self.format == CoordFormat.xn:
      return Data(data.charges, data.masses, data.pos.transpose(), data.vel.transpose())
    elif self.format == CoordFormat.split:
      return Data(data.charges, data.masses, np.stack((data.p0, data.p1), axis=1), np.stack((data.v0, data.v1), axis=1))
    
  def step(self, dt, size, n, dd, compound_step):
    if self.format == CoordFormat.split:
      if compound_step:
        self.call(self.update_v, dd.p0, dd.p1, dd.v0, dd.v1, dt, dd.charges, dd.masses, n)
        self.call(self.update_p, dd.p0, dd.p1, dd.v0, dd.v1, dt, n)
        self.call(self.rebound, dd.p0, dd.p1, dd.v0, dd.v1, size, n)
      else:
        self.call(self.update, dd.p0, dd.p1, dd.v0, dd.v1, dt, size, dd.charges, dd.masses, n)
    else:
      if compound_step:
        self.call(self.update_v, dd.pos, dd.vel, dt, dd.charges, dd.masses, n)
        self.call(self.update_p, dd.pos, dd.vel, dt, n)
        self.call(self.rebound, dd.pos, dd.vel, size, n)
      else:
        self.call(self.update, dd.pos, dd.vel, dt, size, dd.charges, dd.masses, n) 
  
  def simulate(self, dt, n, size, time_steps, save_step, compound_step, data):
    self.blockspergrid = math.ceil(n / self.threadsperblock)
    batches = [save_step]*(time_steps//save_step) + [time_steps%save_step]
    ti = 0
    host_data = self.to_device_layout(data)
    dev_data = self.to_device_all(host_data)
    host_data = self.to_host_all(dev_data, host_data)
    yield (ti, ti*dt, perf_counter(), self.to_host_layout(host_data))
    for batch_size in batches:
      if batch_size==0: # skip last (remainder) batch if it's not needed
        continue 
      for i in range(batch_size):
        self.step(dt, size, np.int32(n), dev_data, compound_step)
      host_data = self.to_host_all(dev_data, host_data)
      ti += batch_size;
      yield (ti, ti*dt, perf_counter(), self.to_host_layout(host_data))

class CudaSolver(Solver):
  tpl = Template("""
typedef float float32;
typedef double float64;
typedef int int32;

{% set i_thread -%}
  int i = threadIdx.x + blockIdx.x*blockDim.x;
{% endset -%}

{%- if format == "split" %}
  {% set xi, yi, vxi, vyi = 'x[i]', 'y[i]', 'vx[i]', 'vy[i]' %}
  {% set xj, yj = 'x[j]', 'y[j]' %}
  {% set pos = pos_t + ' *x, ' + pos_t + ' *y' %}
  {% set vel = vel_t + ' *vx, ' + vel_t + ' *vy' %}
{% elif format == "nx" %}
  {% set xi, yi, vxi, vyi = 'pos[2*i]', 'pos[2*i+1]', 'vel[2*i]', 'vel[2*i+1]' %}  
  {% set xj, yj = 'pos[2*j]', 'pos[2*j+1]' %}
  {% set pos, vel = pos_t + ' *pos', vel_t + ' *vel' %}
{% elif format == "xn" %}
  {% set xi, yi, vxi, vyi = 'pos[i]', 'pos[n+i]', 'vel[i]', 'vel[n+i]' %}
  {% set xj, yj = 'pos[j]', 'pos[n+j]' %}
  {% set pos, vel = pos_t + ' *pos', vel_t + ' *vel' %}
{% endif -%}

{%- set update_v %}
  {{pos_t}} epsilon = 0.0001;
  {{pos_t}} diff0;
  {{pos_t}} diff1;
  {{pos_t}} dist;
  for(int j = 0; j<n; j++){
    if(i != j){
      diff0 = {{xj}} - {{xi}};
      diff1 = {{yj}} - {{yi}};
      dist = sqrt( diff0*diff0 + diff1*diff1 );
      diff0 = diff0 / dist;
      diff1 = diff1 / dist;
      dist = max(epsilon, dist);
      {{vxi}} += - q{% if not scalar %}[i]{% endif %} * q{% if not scalar %}[j]{% endif %} * diff0 / dist / dist / m{% if not scalar %}[i]{% endif %} * dt;
      {{vyi}} += - q{% if not scalar %}[i]{% endif %} * q{% if not scalar %}[j]{% endif %} * diff1 / dist / dist / m{% if not scalar %}[i]{% endif %} * dt;
    }   
  }
{% endset -%}

{%- set update_p %}
  {{xi}} += {{vxi}} * dt;
  {{yi}} += {{vyi}} * dt;
{% endset -%}

{%- set rebound %}
  if({{xi}} < 0){
    {{xi}} = - {{xi}};
    {{vxi}} = - {{vxi}};
  } else if ({{xi}} > size) {
    {{xi}} = 2*size - {{xi}};
    {{vxi}} = - {{vxi}};
  }
  if({{yi}} < 0){
    {{yi}} = - {{yi}};
    {{vyi}} = - {{vyi}};
  } else if ({{yi}} > size) {
    {{yi}} = 2*size - {{yi}};
    {{vyi}} = - {{vyi}};
  }
{% endset -%}

__global__ void update_v({{pos}}, {{vel}}, {{time_t}} dt,
                         {{charge_t}} {% if not scalar %}*{% endif %}q, 
                         {{mass_t}} {% if not scalar %}*{% endif %}m, int n){
  {{ i_thread}}
  if(i < n){
    {{ update_v }}
  }
}

__global__ void update_p({{pos}}, {{vel}}, {{time_t}} dt, int n){
  {{ i_thread }}
  if(i < n){
    {{ update_p }}
  }
}

__global__ void rebound({{pos}}, {{vel}}, {{pos_t}} size, int n){
  {{ i_thread }}
  if(i < n){
    {{ rebound }}
  }
}

__global__ void update({{pos}}, {{vel}}, {{time_t}} dt, {{pos_t}} size, 
                       {{charge_t}} {% if not scalar %}*{% endif %}q, 
                       {{mass_t}} {% if not scalar %}*{% endif %}m, int n){
  {{ i_thread }}
  if(i < n){
    {{ update_v }}
    {{ update_p }}
    {{ rebound }}
  }
}

""")

  def _to_device(self, host_array):
    return cuda.to_device(host_array)
  
  def _to_host(self, dev_array, host_array):
    return cuda.from_device_like(dev_array, host_array)
  
  def __init__(self, coord_format=CoordFormat.nx, scalar=False, threadsperblock=64, types={}):
    super().__init__(coord_format, scalar, threadsperblock, types)
    self._context = make_default_context()
    self._device = self._context.get_device()
    mod = SourceModule(self.source)
    self.update_v = mod.get_function("update_v")
    self.update_p = mod.get_function("update_p")
    self.rebound = mod.get_function("rebound")
    self.update = mod.get_function("update")

  def __del__(self):
    self._context.detach()

  def call(self, foo, *args, **kwargs):
    grid = (self.blockspergrid, 1)
    block = (self.threadsperblock, 1, 1)
    return foo(block=block, grid=grid, shared=0, *args, **kwargs)

result = run_simulation(TaskGenerator(dt=0.1), CudaSolver(), True, 16, 1000, 20)
