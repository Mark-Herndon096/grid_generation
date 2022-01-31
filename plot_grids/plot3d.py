#!/usr/bin/env python3
import numpy as np
import struct as st

def read_grid_file(filename) :
	def fopen(filename) :
		fid = open(filename,"rb")
		return fid
	
	def fread(fid,n,typ) :
		data_val = []
		if typ == 'int' :
			typ_val = 4
			for i in range(n) :
				byte_val = fid.read(typ_val)
				byte_data = st.unpack('i',byte_val)
				data_val.append(byte_data[0])
		if typ == 'single' :
			typ_val = 4
			for i in range(n) :
				byte_val = fid.read(typ_val)
				byte_data = st.unpack('f',byte_val)
				data_val.append(byte_data[0])
		if typ == 'double' :
			typ_val = 8
			for i in range(n) :
				byte_val = fid.read(typ_val)
				byte_data = st.unpack('d',byte_val)
				data_val.append(byte_data[0])
		return data_val
	
	def read_ng(fid) :
		ng = fread(fid,1,'int')
		return ng
	
	def read_dims(fid,ng) :
		dims = fread(fid,3,'int')
		dims = np.asarray(dims,dtype='int')
		return dims
	
	def read_xyz(fid,ng,dims) :
		ni = dims[0]; nj = dims[1]; nk = dims[2];
		nn = ni*nj*nk
		x  = fread(fid,nn,'double')
		x = np.asarray(x)
		x = np.reshape(x,(ni,nj,nk), order='F')
		y  = fread(fid,nn,'double')
		y = np.asarray(y)
		y = np.reshape(y,(ni,nj,nk), order='F')
		z  = fread(fid,nn,'double')
		z = np.asarray(z)
		z = np.reshape(z,(ni,nj,nk), order='F')
		return x, y, z

	fid = fopen(filename)
	ng  = read_ng(fid)
	dims = read_dims(fid,ng)
	x, y, z = read_xyz(fid,ng,dims)
	return ng, dims, x, y, z


