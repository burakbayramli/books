#!/usr/bin/env python
import xdrlib
p = xdrlib.Packer()
p.pack_double(3.2)
p.pack_int(5)
# pack list; 2nd arg is the function used to pack each element
p.pack_array([1.0, 0.1, 0.001], p.pack_double)
f=open('tmp.dat','w'); f.write(p.get_buffer()); f.close()

f=open('tmp.dat','r'); 
u = xdrlib.Unpacker(f.read())
f.close()
some_double = u.unpack_double()
some_int = u.unpack_int()
some_list = u.unpack_array(u.unpack_double)
print some_double, some_int, some_list

