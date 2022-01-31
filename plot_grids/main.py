#!/usr/bin/env python3
import numpy as np
import struct as st
import plot3d as p3d

ng, dims, x, y, z = p3d.read_grid_file("cylinder.sp.x")

ni = dims[0]; nj = dims[1]; nk = dims[2];
print(dims.shape, dims.dtype)
print(x.shape,x.dtype)
print(y.shape,y.dtype)
print(z.shape,z.dtype)


