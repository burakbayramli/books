# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************

class SimpleGMSH(object):
    def __init__(self):
        self.ID_POINT = 0
        self.ID_LINE = 0
        self.ID_CIRCLE = 10000
        self.ID_LINE_LOOP = 0
        self.ID_PLANE_SURFACE = 0
        self.GMSH_CODE = []
        
    def add_point(self,coords,esize=0.1):
        n = esize
        self.ID_POINT += 1
        x,y = coords[0], coords[1]
        name = "{0}".format(self.ID_POINT)
        self.GMSH_CODE.append("Point({0}) = {{ {1},{2},{3},{4} }};".format(name,x,y,0,n))
        return name
        
    def add_line(self,p0,p1):
        self.ID_LINE += 1
        name = "{0}".format(self.ID_LINE)
        self.GMSH_CODE.append("Line({0}) = {{ {1},{2} }};".format(name,p0,p1))
        return name
        
    def add_circle(self,p0,p1,p2=None):
        self.ID_CIRCLE += 1
        name = "{0}".format(self.ID_CIRCLE)
        if p2 is None:
            self.GMSH_CODE.append("Circle({0}) = {{ {1},{2},{1} }};".format(name,p1,p0))
        else:
            self.GMSH_CODE.append("Circle({0}) = {{ {1},{2},{3} }};".format(name,p1,p0,p2))
        return name
        
    def add_line_loop(self,*lines):
        self.ID_LINE_LOOP += 1
        name = "{0}".format(self.ID_LINE_LOOP)
        self.GMSH_CODE.append("Line Loop({0}) = {{{1}}};".format(name,",".join(lines)))
        return name
    
    def add_plane_surface(self,*loops):
        self.ID_PLANE_SURFACE += 1
        name = "{0}".format(self.ID_PLANE_SURFACE)
        self.GMSH_CODE.append("Plane Surface({0}) = {{{1}}};".format(name, ",".join(loops)))
        return name
        
    def delete_surfaces(self,*surfaces):
        self.GMSH_CODE.append("Delete {{ Surface{{ {0} }}; }}".format(",".join(surfaces)))
        
    def get_code(self):
        return "\n".join(self.GMSH_CODE)

    def generate_mesh(self, verbose=False):
        import meshio
        import os
        import subprocess
        import tempfile
        
        gmsh_executable = 'gmsh'
        
        handle, filename = tempfile.mkstemp(suffix='.geo')
        os.write(handle, self.get_code().encode())
        os.close(handle)

        handle, outname = tempfile.mkstemp(suffix='.msh')
        cmd = [gmsh_executable, '-2', filename, '-o', outname]
        try:
            out = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
        except:
            out = os.system(" ".join(cmd))
        if verbose:
            print(out.decode())
        #points,cells = meshio.read(outname)
        mesh = meshio.read(outname)
        # ~ print(dir(mesh))
        #return points, cells
        return mesh.points, mesh.cells["triangle"]


if __name__=='__main__':
    pass
