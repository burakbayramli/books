# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
import nusa._mesh as msh
import meshio

class Modeler(object):
    def __init__(self):
        self.geom = msh.SimpleGMSH()
        
    def add_rectangle(self,p0,p1,esize=0.1):
        n = esize
        x0,y0 = p0[:2]
        x1,y1 = p1[:2]
        xa,ya = p1[0],p0[1]
        xb,yb = p0[0],p1[1]
        p1 = self.geom.add_point((x0,y0,0),n)
        p2 = self.geom.add_point((xa,ya,0),n)
        p3 = self.geom.add_point((x1,y1,0),n)
        p4 = self.geom.add_point((xb,yb,0),n)
        l1 = self.geom.add_line(p1,p2)
        l2 = self.geom.add_line(p2,p3)
        l3 = self.geom.add_line(p3,p4)
        l4 = self.geom.add_line(p4,p1)
        loop = self.geom.add_line_loop(l1,l2,l3,l4)
        surf = self.geom.add_plane_surface(loop)
        return loop,surf
        
    def add_poly(self,*points,**kw):
        if "esize" in kw:
            n = kw["esize"]
        else:
            n = 0.1
        pts = []
        for pt in points:
            cpt = self.geom.add_point((pt[0],pt[1],0),n)
            pts.append(cpt)
        
        lines = []
        for k,_pt in enumerate(pts):
            try:
                p0,p1 = _pt, pts[k+1]
            except:
                p0,p1 = _pt, pts[0]
            cl = self.geom.add_line(p0,p1)
            lines.append(cl)
            
        loop = self.geom.add_line_loop(*lines)
        surf = self.geom.add_plane_surface(loop)
        return loop,surf
        
    def add_circle(self,p0,r,esize=0.1):
        n = esize
        xc,yc = p0[0],p0[1]
        xa,ya = xc+r, yc
        pc = self.geom.add_point((xc,yc,0), n)
        pa = self.geom.add_point((xa,ya,0), n)
        c = self.geom.add_circle(pc,pa)
        loop = self.geom.add_line_loop(c)
        surf = self.geom.add_plane_surface(loop)
        return loop,surf
        
    def add_arc_circle(self,p0,p1,p2,esize=0.1):
        n = esize
        x0,y0 = p0[0],p0[1] # Center point
        x1,y1 = p1[0],p1[1] # Start point
        x2,y2 = p2[0],p2[1] # End point
        p0_ = self.geom.add_point((x0,y0,0), n)
        p1_ = self.geom.add_point((x1,y1,0), n)
        p2_ = self.geom.add_point((x2,y2,0), n)
        c = self.geom.add_circle(p0_,p1_,p2_)
        loop = self.geom.add_line_loop(c)
        surf = self.geom.add_plane_surface(loop)
        return loop,surf
        
    def substract_surfaces(self,s1,s2):
        loop1,surf1 = s1
        loop2,surf2 = s2
        self.geom.delete_surfaces(surf1,surf2)
        loop = []
        surf = self.geom.add_plane_surface(loop1,loop2)
        return loop,surf
        
    def plot_mesh(self):
        import matplotlib.pyplot as plt
        from matplotlib.patches import Polygon
        from matplotlib.collections import PatchCollection
        
        fig = plt.figure()
        ax = fig.add_subplot(111)

        _x,_y = [],[]
        patches = []
        for k,elm in enumerate(self.ec):
            _x,_y,_ux,_uy = [],[],[],[]
            for nd in elm:
                _x.append(self.nc[nd,0])
                _y.append(self.nc[nd,1])
            polygon = Polygon(list(zip(_x,_y)), True)
            patches.append(polygon)
            
        pc = PatchCollection(patches, color="#25CDCD", edgecolor="#435959", alpha=0.8, lw=0.5)
        ax.add_collection(pc)
        x0,x1,y0,y1 = self._rect_region()
        ax.set_xlim(x0,x1)
        ax.set_ylim(y0,y1)
        #~ ax.set_title("Model %s"%(self.name))
        ax.set_aspect("equal")
        plt.show()

    def _rect_region(self):
        x0,x1,y0,y1 = min(self.x), max(self.x), min(self.y), max(self.y)
        kx = (x1-x0)/10.
        ky = (y1-y0)/10.
        return x0-kx, x1+kx, y0-ky, y1+ky
        
    def generate_mesh(self):
        nc, ec = self.geom.generate_mesh()
        # ec = ec["triangle"]
        self.x, self.y = nc[:,0], nc[:,1]
        self.nc = nc
        self.ec = ec
        return nc,ec

    def generate_mesh_from_file(self,filename):
        mesh = meshio.read(filename)
        self.nc = mesh.points
        self.x, self.y = self.nc[:,0], self.nc[:,1]
        self.ec = mesh.cells["triangle"]
        return mesh.points, mesh.cells["triangle"]
    
    



    
if __name__=='__main__':
    geom = msh.SimpleGMSH()
    nc,ec = geom.generate_mesh()
