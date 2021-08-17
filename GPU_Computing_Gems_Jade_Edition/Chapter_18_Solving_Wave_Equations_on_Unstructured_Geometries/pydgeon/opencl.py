# Pydgeon - the Python DG Environment
# (C) 2010 Tim Warburton, Andreas Kloeckner
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.




from __future__ import division

import numpy as np
from pydgeon import Discretization2D
import pyopencl as cl
import pyopencl.array as cl_array




CL_OPTIONS = ("-cl-mad-enable -cl-fast-relaxed-math "
        "-cl-no-signed-zeros -cl-strict-aliasing")




class CLDiscretization2D(Discretization2D):
    def __init__(self, ldis, Nv, VX, VY, K, EToV, profile=0):
        Discretization2D.__init__(self, ldis, Nv, VX, VY, K, EToV)

        self.ctx = cl.create_some_context()
        if profile:
            cq_properties = cl.command_queue_properties.PROFILING_ENABLE
        else:
            cq_properties = 0

        self.queue = cl.CommandQueue(self.ctx,
                properties=cq_properties)

        self.block_size = 16*((ldis.Np+15)//16)

        self.prepare_dev_data()

        self.profile = profile

        if self.profile:
            self.total_flops = 0

    def prepare_dev_data(self):
        ldis = self.ldis

        # differentiation matrix
        drds_dev = np.empty((ldis.Np, ldis.Np, 2), dtype=np.float32)
        drds_dev[:,:,0] = ldis.Dr.T
        drds_dev[:,:,1] = ldis.Ds.T
        mf = cl.mem_flags
        self.diffmatrices_img = cl.Image(self.ctx, mf.READ_ONLY | mf.COPY_HOST_PTR,
                cl.ImageFormat(cl.channel_order.RG, cl.channel_type.FLOAT),
                shape=drds_dev.shape[:2], hostbuf=drds_dev)

        # geometric coefficients
        drdx_dev = np.empty((self.K, self.dimensions**2), dtype=np.float32)
        drdx_dev[:,0] = self.rx[:, 0]
        drdx_dev[:,1] = self.ry[:, 0]
        drdx_dev[:,2] = self.sx[:, 0]
        drdx_dev[:,3] = self.sy[:, 0]
        self.drdx_dev = cl_array.to_device(self.ctx, self.queue, drdx_dev)

        # lift matrix
        lift_dev = np.zeros((ldis.Np, ldis.Nfp, 4), dtype=np.float32)
        partitioned_lift = ldis.LIFT.reshape(ldis.Np, -1, ldis.Nfaces)

        lift_dev[:, :, :ldis.Nfaces] = partitioned_lift

        self.lift_img = cl.Image(self.ctx, mf.READ_ONLY | mf.COPY_HOST_PTR,
                cl.ImageFormat(cl.channel_order.RGBA, cl.channel_type.FLOAT),
                shape=(ldis.Nfp, ldis.Np), hostbuf=lift_dev)

        # surface info
        surfinfo_dev = np.empty((self.K, 6, ldis.Nafp), dtype=np.float32)

        el_p, face_i_p = divmod(self.vmapP.reshape(-1, ldis.Nafp), ldis.Np)
        el_m, face_i_m = divmod(self.vmapM.reshape(-1, ldis.Nafp), ldis.Np)

        ind_p = el_p * self.block_size + face_i_p
        ind_m = el_m * self.block_size + face_i_m

        surfinfo_dev[:, 0, :] = ind_m
        surfinfo_dev[:, 1, :] = ind_p
        surfinfo_dev[:, 2, :] = self.Fscale
        surfinfo_dev[:, 3, :] = np.where(ind_m==ind_p, -1, 1)
        surfinfo_dev[:, 4, :] = self.nx
        surfinfo_dev[:, 5, :] = self.ny

        self.surfinfo_dev = cl_array.to_device(self.ctx, self.queue, surfinfo_dev)

    def to_dev(self, vec):
        dev_vec = np.empty((self.K, self.block_size), dtype=np.float32)
        dev_vec[:, :self.ldis.Np] = vec
        return cl_array.to_device(self.ctx, self.queue, dev_vec)

    def from_dev(self, vec):
        return vec.get()[:, :self.ldis.Np]

    def volume_empty(self):
        return cl_array.Array(
                self.ctx, queue=self.queue,
                shape=(self.K, self.block_size),
                dtype=np.float32)

# vim: foldmethod=marker
