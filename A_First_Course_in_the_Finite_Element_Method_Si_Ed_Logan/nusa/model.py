# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
import re
import numpy as np
import numpy.linalg as la
import nusa.templates as tmp
import matplotlib.pyplot as plt
from .core import Model

#~ *********************************************************************
#~ ****************************  SpringModel ***************************
#~ *********************************************************************

class SpringModel(Model):
    """
    Spring Model for finite element analysis
    """
    def __init__(self,name="Spring Model 01"):
        Model.__init__(self,name=name,mtype="spring")
        self.F = {} # Forces
        self.U = {} # Displacements
        self.dof = 1 # 1 DOF per Node
        self.IS_KG_BUILDED = False

    def build_global_matrix(self):
        msz = (self.dof)*self.get_number_of_nodes() # Matrix size
        self.KG = np.zeros((msz,msz))
        for element in self.elements.values():
            ku = element.get_element_stiffness()
            n1,n2 = element.get_nodes()
            self.KG[n1.label, n1.label] += ku[0,0]
            self.KG[n1.label, n2.label] += ku[0,1]
            self.KG[n2.label, n1.label] += ku[1,0]
            self.KG[n2.label, n2.label] += ku[1,1]
        
        self.build_forces_vector()
        self.build_displacements_vector()
        self.IS_KG_BUILDED = True
        
    def _build_global_matrix(self):
        msz = (self.dof)*self.get_number_of_nodes() # Matrix size
        self.KG = np.zeros((msz,msz))
        for element in self.elements.values():
            ku = element.get_element_stiffness()
            n1,n2 = element.get_nodes()
            for ii,jj in self._nodal_index(n1.label,n2.label):
                self.KG[ii[0],ii[1]] += ku[jj[0],jj[1]]
        
        self.build_forces_vector()
        self.build_displacements_vector()
        self.IS_KG_BUILDED = True
        
    def _nodal_index(self,ii,jj):
        from itertools import product,izip
        iter1 = product((ii,jj),repeat=2)
        iter2 = product((0,1),repeat=2)
        return izip(iter1,iter2)
        
    def build_forces_vector(self):
        for node in self.nodes.values():
            self.F[node.label] = {"fx":0, "fy":0}
        
    def build_displacements_vector(self):
        for node in self.nodes.values():
            self.U[node.label] = {"ux":np.nan, "uy":np.nan}
        
    def add_force(self,node,force):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        self.F[node.label]["fx"] = force[0]
        
    def add_constraint(self,node,**constraint):
        """
        Only displacement in x-dir 
        """
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        if "ux" in constraint:
            ux = constraint.get("ux")
            node.set_displacements(ux=ux)
            self.U[node.label]["ux"] = ux
        
    def solve(self):
        # known and unknown values
        self.VU = [node[key] for node in self.U.values() for key in ("ux",)]
        self.VF = [node[key] for node in self.F.values() for key in ("fx",)]
        knw = [pos for pos,value in enumerate(self.VU) if not value is np.nan]
        unknw = [pos for pos,value in enumerate(self.VU) if value is np.nan]
        # Matrices to solve
        self.K2S = np.delete(np.delete(self.KG,knw,0),knw,1)
        self.F2S = np.delete(self.VF,knw,0)
        # For displacements
        self.solved_u = la.solve(self.K2S,self.F2S)
        # Updating U (displacements vector)
        for k,ic in enumerate(unknw):
            nd, var = self.index2key(ic)
            self.U[nd][var] = self.solved_u[k]
            self.nodes[ic].ux = self.solved_u[k]
        # For nodal forces/reactions
        self.NF = self.F.copy()
        self.VU = [node[key] for node in self.U.values() for key in ("ux",)]
        nf_calc = np.dot(self.KG, self.VU)
        for k,ic in enumerate(range(self.get_number_of_nodes())):
            nd, var = self.index2key(ic, ("fx",))
            self.NF[nd][var] = nf_calc[k]
            self.nodes[ic].fx = nf_calc[k]
            
    def index2key(self,idx,opts=("ux",)):
        node = idx
        var = opts[0]
        return node,var

    def simple_report(self,report_type="print",fname="nusa_rpt.txt"):
        from .templates import SPRING_SIMPLE_REPORT
        options = {"headers":"firstrow",
                   "tablefmt":"rst",
                   "numalign":"right"}
        _str = SPRING_SIMPLE_REPORT.format(
                model_name=self.name,
                nodes=self.get_number_of_nodes(),
                elements=self.get_number_of_elements(),
                nodal_displacements=self._get_ndisplacements(options),
                nodal_forces=self._get_nforces(options),
                element_forces=self._get_eforces(options),
                nodes_info=self._get_nodes_info(options),
                elements_info=self._get_elements_info(options))
        if report_type=="print": print(_str)
        elif report_type=="write": self._write_report(_str, fname)
        elif report_type=="string": return _str
        else: return _str

    def _get_eforces(self,options):
        from tabulate import tabulate
        F = [["Element","F"]]
        for elm in self.get_elements():
            F.append([elm.label+1, elm.fx])
        return tabulate(F, **options)
        


#~ *********************************************************************
#~ ****************************  BarModel ******************************
#~ *********************************************************************
class BarModel(Model):
    """
    Bar model for finite element analysis
    """
    def __init__(self,name="Bar Model 01"):
        Model.__init__(self,name=name,mtype="bar")
        self.F = {} # Forces
        self.U = {} # Displacements
        self.dof = 1 # 1 DOF for bar element (per node)
        self.IS_KG_BUILDED = False
        
    def build_forces_vector(self):
        for node in self.nodes.values():
            self.F[node.label] = {"fx":0, "fy":0}
        
    def build_global_matrix(self):
        msz = (self.dof)*self.get_number_of_nodes()
        self.KG = np.zeros((msz,msz))
        for element in self.elements.values():
            ku = element.get_element_stiffness()
            n1,n2 = element.get_nodes()
            self.KG[n1.label, n1.label] += ku[0,0]
            self.KG[n1.label, n2.label] += ku[0,1]
            self.KG[n2.label, n1.label] += ku[1,0]
            self.KG[n2.label, n2.label] += ku[1,1]
        self.build_forces_vector()
        self.build_displacements_vector()
        self.IS_KG_BUILDED = True
        
    def build_displacements_vector(self):
        for node in self.nodes.values():
            self.U[node.label] = {"ux":np.nan, "uy":np.nan}
        
    def add_force(self,node,force):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        self.F[node.label]["fx"] = force[0]
        
    def add_constraint(self,node,**constraint):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        if "ux" in constraint:
            ux = constraint.get('ux')
            node.set_displacements(ux=ux)
            self.U[node.label]["ux"] = ux
        
    def solve(self):
        # known and unknown values
        self.VU = [node[key] for node in self.U.values() for key in ("ux",)]
        self.VF = [node[key] for node in self.F.values() for key in ("fx",)]
        knw = [pos for pos,value in enumerate(self.VU) if not value is np.nan]
        unknw = [pos for pos,value in enumerate(self.VU) if value is np.nan]
        
        if len(unknw)==1:
            _k = unknw[0]
            _rowtmp = self.KG[_k,:]
            _ftmp = self.VF[_k]
            _fk = _ftmp - np.dot(np.delete(_rowtmp,_k), np.delete(self.VU,_k))
            _uk = _fk / self.KG[_k, _k]
            # Then 
            self.solved_u = np.array([_uk])
        else: # "Normal" case
            self.K2S = np.delete(np.delete(self.KG,knw,0),knw,1)
            self.F2S = np.delete(self.VF,knw,0)
            self.solved_u = la.solve(self.K2S,self.F2S)
            
        # For displacements
        # Updating U (displacements vector)
        for k,ic in enumerate(unknw):
            nd, var = self.index2key(ic)
            self.U[nd][var] = self.solved_u[k]
            self.nodes[ic].ux = self.solved_u[k]
        # For nodal forces/reactions
        self.NF = self.F.copy()
        self.VU = [node[key] for node in self.U.values() for key in ("ux",)]
        nf_calc = np.dot(self.KG, self.VU)
        for k,ic in enumerate(range(self.get_number_of_nodes())):
            nd, var = self.index2key(ic, ("fx",))
            self.NF[nd][var] = nf_calc[k]
            self.nodes[ic].fx = nf_calc[k]

    def index2key(self,idx,opts=("ux",)):
        node = idx
        var = opts[0]
        return node,var



#~ *********************************************************************
#~ ****************************  TrussModel ****************************
#~ *********************************************************************
class TrussModel(Model):
    """
    Truss model for finite element analysis
    """
    def __init__(self,name="Truss Model 01"):
        Model.__init__(self,name=name,mtype="truss")
        self.F = {} # Forces
        self.U = {} # Displacements
        self.dof = 2 # 2 DOF for truss element
        self.IS_KG_BUILDED = False
        
    def build_global_matrix(self):
        msz = (self.dof)*self.get_number_of_nodes()
        self.KG = np.zeros((msz,msz))
        for element in self.elements.values():
            ku = element.get_element_stiffness()
            n1,n2 = element.get_nodes()
            self.KG[2*n1.label, 2*n1.label] += ku[0,0]
            self.KG[2*n1.label, 2*n1.label+1] += ku[0,1]
            self.KG[2*n1.label, 2*n2.label] += ku[0,2]
            self.KG[2*n1.label, 2*n2.label+1] += ku[0,3]
            
            self.KG[2*n1.label+1, 2*n1.label] += ku[1,0]
            self.KG[2*n1.label+1, 2*n1.label+1] += ku[1,1]
            self.KG[2*n1.label+1, 2*n2.label] += ku[1,2]
            self.KG[2*n1.label+1, 2*n2.label+1] += ku[1,3]
            
            self.KG[2*n2.label, 2*n1.label] += ku[2,0]
            self.KG[2*n2.label, 2*n1.label+1] += ku[2,1]
            self.KG[2*n2.label, 2*n2.label] += ku[2,2]
            self.KG[2*n2.label, 2*n2.label+1] += ku[2,3]
            
            self.KG[2*n2.label+1, 2*n1.label] += ku[3,0]
            self.KG[2*n2.label+1, 2*n1.label+1] += ku[3,1]
            self.KG[2*n2.label+1, 2*n2.label] += ku[3,2]
            self.KG[2*n2.label+1, 2*n2.label+1] += ku[3,3]
            
        self.build_forces_vector()
        self.build_displacements_vector()
        self.IS_KG_BUILDED = True
        
    def build_forces_vector(self):
        for node in self.nodes.values():
            self.F[node.label] = {"fx":0, "fy":0}
        
    def build_displacements_vector(self):
        for node in self.nodes.values():
            self.U[node.label] = {"ux":np.nan, "uy":np.nan}
    
    def add_force(self,node,force):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        self.F[node.label]["fx"] = force[0]
        self.F[node.label]["fy"] = force[1]
        node.fx = force[0]
        node.fy = force[1]
        
    def add_constraint(self,node,**constraint):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        cs = constraint
        if "ux" in cs and "uy" in cs: #
            ux = cs.get('ux')
            uy = cs.get('uy')
            node.set_displacements(ux=ux, uy=uy) # eqv to node.ux = ux, node.uy = uy
            self.U[node.label]["ux"] = ux
            self.U[node.label]["uy"] = uy
        elif "ux" in cs:
            ux = cs.get('ux')
            node.set_displacements(ux=ux)
            self.U[node.label]["ux"] = ux
        elif "uy" in cs:
            uy = cs.get('uy')
            node.set_displacements(uy=uy)
            self.U[node.label]["uy"] = uy
        else: pass # todo
        
    def solve(self):
        # Solve LS
        self.VU = [node[key] for node in self.U.values() for key in ("ux","uy")]
        self.VF = [node[key] for node in self.F.values() for key in ("fx","fy")]
        knw = [pos for pos,value in enumerate(self.VU) if not value is np.nan]
        unknw = [pos for pos,value in enumerate(self.VU) if value is np.nan]
        self.K2S = np.delete(np.delete(self.KG,knw,0),knw,1)
        self.F2S = np.delete(self.VF,knw,0)
        
        # For displacements
        self.solved_u = la.solve(self.K2S,self.F2S)
        for k,ic in enumerate(unknw):
            nd, var = self.index2key(ic)
            self.U[nd][var] = self.solved_u[k]
            
        # Updating nodes displacements
        for nd in self.nodes.values():
            if np.isnan(nd.ux):
                nd.ux = self.U[nd.label]["ux"]
            if np.isnan(nd.uy):
                nd.uy = self.U[nd.label]["uy"]
                    
        # For nodal forces/reactions
        self.NF = self.F.copy()
        self.VU = [node[key] for node in self.U.values() for key in ("ux","uy")]
        nf_calc = np.dot(self.KG, self.VU)
        for k in range(2*self.get_number_of_nodes()):
            nd, var = self.index2key(k, ("fx","fy"))
            self.NF[nd][var] = nf_calc[k]
            cnlab = np.floor(k/float(self.dof))
            if var=="fx": 
                self.nodes[cnlab].fx = nf_calc[k]
            elif var=="fy":
                self.nodes[cnlab].fy = nf_calc[k]
                
    def index2key(self,idx,opts=("ux","uy")):
        """
        Index to key, where key can be ux or uy
        """
        node = idx//2
        var = opts[0] if ((-1)**idx)==1 else opts[1]
        return node,var
        
    def plot_model(self):
        """
        Plot the mesh model, including bcs
        """
        import matplotlib.pyplot as plt
        
        fig = plt.figure()
        ax = fig.add_subplot(111)
        
        for elm in self.get_elements():
            ni, nj = elm.get_nodes()
            ax.plot([ni.x,nj.x],[ni.y,nj.y],"b-")
            for nd in (ni,nj):
                if nd.fx > 0: self._draw_xforce(ax,nd.x,nd.y,1)
                if nd.fx < 0: self._draw_xforce(ax,nd.x,nd.y,-1)
                if nd.fy > 0: self._draw_yforce(ax,nd.x,nd.y,1)
                if nd.fy < 0: self._draw_yforce(ax,nd.x,nd.y,-1)
                if nd.ux == 0: self._draw_xconstraint(ax,nd.x,nd.y)
                if nd.uy == 0: self._draw_yconstraint(ax,nd.x,nd.y)
        
        x0,x1,y0,y1 = self.rect_region()
        plt.axis('equal')
        ax.set_xlim(x0,x1)
        ax.set_ylim(y0,y1)

    def _draw_xforce(self,axes,x,y,ddir=1):
        """
        Draw horizontal arrow -> Force in x-dir
        """
        dx, dy = self._calculate_arrow_size(), 0
        HW = dx/5.0
        HL = dx/3.0
        arrow_props = dict(head_width=HW, head_length=HL, fc='r', ec='r')
        axes.arrow(x, y, ddir*dx, dy, **arrow_props)
        
    def _draw_yforce(self,axes,x,y,ddir=1):
        """
        Draw vertical arrow -> Force in y-dir
        """
        dx,dy = 0, self._calculate_arrow_size()
        HW = dy/5.0
        HL = dy/3.0
        arrow_props = dict(head_width=HW, head_length=HL, fc='r', ec='r')
        axes.arrow(x, y, dx, ddir*dy, **arrow_props)
        
    def _draw_xconstraint(self,axes,x,y):
        axes.plot(x, y, "g<", markersize=10, alpha=0.6)
    
    def _draw_yconstraint(self,axes,x,y):
        axes.plot(x, y, "gv", markersize=10, alpha=0.6)
        
    def _calculate_arrow_size(self):
        x0,x1,y0,y1 = self.rect_region(factor=50)
        sf = 5e-2
        kfx = sf*(x1-x0)
        kfy = sf*(y1-y0)
        return np.mean([kfx,kfy])
        
    def plot_deformed_shape(self,dfactor=1.0):
        import matplotlib.pyplot as plt
        fig = plt.figure()
        ax = fig.add_subplot(111)
        
        df = dfactor*self._calculate_deformed_factor()
        
        for elm in self.get_elements():
            ni,nj = elm.get_nodes()
            x, y = [ni.x,nj.x], [ni.y,nj.y]
            xx = [ni.x+ni.ux*df, nj.x+nj.ux*df]
            yy = [ni.y+ni.uy*df, nj.y+nj.uy*df]
            ax.plot(x,y,'bo-')
            ax.plot(xx,yy,'ro--')

        x0,x1,y0,y1 = self.rect_region()
        plt.axis('equal')
        ax.set_xlim(x0,x1)
        ax.set_ylim(y0,y1)
        
    def _calculate_deformed_factor(self):
        x0,x1,y0,y1 = self.rect_region()
        ux = np.abs(np.array([n.ux for n in self.get_nodes()]))
        uy = np.abs(np.array([n.uy for n in self.get_nodes()]))
        sf = 1.5e-2
        if ux.max()==0 and uy.max()!=0:
            kfx = sf*(y1-y0)/uy.max()
            kfy = sf*(y1-y0)/uy.max()
        if uy.max()==0 and ux.max()!=0:
            kfx = sf*(x1-x0)/ux.max()
            kfy = sf*(x1-x0)/ux.max()
        if ux.max()!=0 and uy.max()!=0:
            kfx = sf*(x1-x0)/ux.max()
            kfy = sf*(y1-y0)/uy.max()
        return np.mean([kfx,kfy])

    def show(self):
        import matplotlib.pyplot as plt
        plt.show()
        
    def rect_region(self,factor=7.0):
        nx,ny = [],[]
        for n in self.get_nodes():
            nx.append(n.x)
            ny.append(n.y)
        xmn,xmx,ymn,ymx = min(nx),max(nx),min(ny),max(ny)
        kx = (xmx-xmn)/factor
        ky = (ymx-ymn)/factor
        return xmn-kx, xmx+kx, ymn-ky, ymx+ky
        
    def simple_report(self,report_type="print",fname="nusa_rpt.txt"):
        from .templates import TRUSS_SIMPLE_REPORT
        options = {"headers":"firstrow",
                   "tablefmt":"rst",
                   "numalign":"right"}
        _str = TRUSS_SIMPLE_REPORT.format(
                model_name=self.name,
                nodes=self.get_number_of_nodes(),
                elements=self.get_number_of_elements(),
                nodal_displacements=self._get_ndisplacements(options),
                nodal_forces=self._get_nforces(options),
                element_forces=self._get_eforces(options),
                element_stresses=self._get_estresses(options),
                nodes_info=self._get_nodes_info(options),
                elements_info=self._get_elements_info(options))
        if report_type=="print": print(_str)
        elif report_type=="write": self._write_report(_str, fname)
        elif report_type=="string": return _str
        else: return _str
        
    def _write_report(self,txt,fname):
        fobj = open(fname,"w")
        fobj.write(txt)
        fobj.close()
        
    def _get_ndisplacements(self,options):
        from tabulate import tabulate
        D = [["Node","UX","UY"]]
        for n in self.get_nodes():
            D.append([n.label+1,n.ux,n.uy])
        return tabulate(D, **options)
        
    def _get_nforces(self,options):
        from tabulate import tabulate
        F = [["Node","FX","FY"]]
        for n in self.get_nodes():
            F.append([n.label+1,n.fx,n.fy])
        return tabulate(F, **options)
        
    def _get_eforces(self,options):
        from tabulate import tabulate
        F = [["Element","F"]]
        for elm in self.get_elements():
            F.append([elm.label+1, elm.f])
        return tabulate(F, **options)
        
    def _get_estresses(self,options):
        from tabulate import tabulate
        S = [["Element","S"]]
        for elm in self.get_elements():
            S.append([elm.label+1, elm.s])
        return tabulate(S, **options)
    
    def _get_nodes_info(self,options):
        from tabulate import tabulate
        F = [["Node","X","Y"]]
        for n in self.get_nodes():
            F.append([n.label+1, n.x, n.y])
        return tabulate(F, **options)
    
    def _get_elements_info(self,options):
        from tabulate import tabulate
        S = [["Element","NI","NJ"]]
        for elm in self.get_elements():
            ni, nj = elm.get_nodes()
            S.append([elm.label+1, ni.label+1, nj.label+1])
        return tabulate(S, **options)



#~ *********************************************************************
#~ ****************************  BeamModel *****************************
#~ *********************************************************************    
class BeamModel(Model):
    """
    Model for finite element analysis
    """
    def __init__(self,name="Beam Model 01"):
        Model.__init__(self,name=name,mtype="beam")
        self.F = {} # Forces
        self.U = {} # Displacements
        self.dof = 2 # 2 DOF for beam element
        self.IS_KG_BUILDED = False
        
    def build_global_matrix(self):
        msz = (self.dof)*self.get_number_of_nodes()
        self.KG = np.zeros((msz,msz))
        for element in self.elements.values():
            ku = element.get_element_stiffness()
            n1,n2 = element.get_nodes()
            self.KG[2*n1.label, 2*n1.label] += ku[0,0]
            self.KG[2*n1.label, 2*n1.label+1] += ku[0,1]
            self.KG[2*n1.label, 2*n2.label] += ku[0,2]
            self.KG[2*n1.label, 2*n2.label+1] += ku[0,3]
            
            self.KG[2*n1.label+1, 2*n1.label] += ku[1,0]
            self.KG[2*n1.label+1, 2*n1.label+1] += ku[1,1]
            self.KG[2*n1.label+1, 2*n2.label] += ku[1,2]
            self.KG[2*n1.label+1, 2*n2.label+1] += ku[1,3]
            
            self.KG[2*n2.label, 2*n1.label] += ku[2,0]
            self.KG[2*n2.label, 2*n1.label+1] += ku[2,1]
            self.KG[2*n2.label, 2*n2.label] += ku[2,2]
            self.KG[2*n2.label, 2*n2.label+1] += ku[2,3]
            
            self.KG[2*n2.label+1, 2*n1.label] += ku[3,0]
            self.KG[2*n2.label+1, 2*n1.label+1] += ku[3,1]
            self.KG[2*n2.label+1, 2*n2.label] += ku[3,2]
            self.KG[2*n2.label+1, 2*n2.label+1] += ku[3,3]
            
        self.build_forces_vector()
        self.build_displacements_vector()
        self.IS_KG_BUILDED = True
    
    def _build_global_matrix(self):
        msz = (self.dof)*self.get_number_of_nodes()
        self.KG = np.zeros((msz,msz))
        for element in self.elements.values():
            ku = element.get_element_stiffness()
            n1,n2 = element.get_nodes()
            self.KG[2*n1.label, 2*n1.label] += ku[0,0]
            self.KG[2*n1.label, 2*n1.label+1] += ku[0,1]
            self.KG[2*n1.label, 2*n2.label] += ku[0,2]
            self.KG[2*n1.label, 2*n2.label+1] += ku[0,3]
            
            self.KG[2*n1.label+1, 2*n1.label] += ku[1,0]
            self.KG[2*n1.label+1, 2*n1.label+1] += ku[1,1]
            self.KG[2*n1.label+1, 2*n2.label] += ku[1,2]
            self.KG[2*n1.label+1, 2*n2.label+1] += ku[1,3]
            
            self.KG[2*n2.label, 2*n1.label] += ku[2,0]
            self.KG[2*n2.label, 2*n1.label+1] += ku[2,1]
            self.KG[2*n2.label, 2*n2.label] += ku[2,2]
            self.KG[2*n2.label, 2*n2.label+1] += ku[2,3]
            
            self.KG[2*n2.label+1, 2*n1.label] += ku[3,0]
            self.KG[2*n2.label+1, 2*n1.label+1] += ku[3,1]
            self.KG[2*n2.label+1, 2*n2.label] += ku[3,2]
            self.KG[2*n2.label+1, 2*n2.label+1] += ku[3,3]
            
        self.build_forces_vector()
        self.build_displacements_vector()
        self.IS_KG_BUILDED = True
    
    def build_forces_vector(self):
        for node in self.nodes.values():
            self.F[node.label] = {"fy":0.0, "m":0.0} # (fy, m)
            
    def build_displacements_vector(self):
        for node in self.nodes.values():
            self.U[node.label] = {"uy":np.nan, "ur":np.nan} # (uy, r)
    
    def add_force(self,node,force):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        self.F[node.label]["fy"] = force[0]
        node.fy = force[0]
        
    def add_moment(self,node,moment):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        self.F[node.label]["m"] = moment[0]
        node.m = moment[0]
        
    def add_constraint(self,node,**constraint):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        cs = constraint
        if "ux" in cs and "uy" in cs and "ur" in cs: # 
            ux = cs.get('ux')
            uy = cs.get('uy')
            ur = cs.get('ur')
            node.set_displacements(ux=ux, uy=uy, ur=ur)
            #~ print("Encastre")
            self.U[node.label]["uy"] = uy
            self.U[node.label]["ur"] = ur
        elif "ux" in cs and "uy" in cs: # 
            ux = cs.get('ux')
            uy = cs.get('uy')
            node.set_displacements(ux=ux, uy=uy)
            #~ print("Fixed")
            self.U[node.label]["uy"] = uy
        elif "uy" in cs:
            uy = cs.get('uy')
            node.set_displacements(uy=uy)
            #~ print("Simple support")
            self.U[node.label]["uy"] = uy
        
    def solve(self):
        # Solve LS
        self.VU = [node[key] for node in self.U.values() for key in ("uy","ur")]
        self.VF = [node[key] for node in self.F.values() for key in ("fy","m")]
        knw = [pos for pos,value in enumerate(self.VU) if not value is np.nan]
        unknw = [pos for pos,value in enumerate(self.VU) if value is np.nan]
        self.K2S = np.delete(np.delete(self.KG,knw,0),knw,1)
        self.F2S = np.delete(self.VF,knw,0)
        
        # For displacements
        self.solved_u = la.solve(self.K2S,self.F2S)
        for k,ic in enumerate(unknw):
            nd, var = self.index2key(ic)
            self.U[nd][var] = self.solved_u[k]
            
        # Updating nodes displacements
        for nd in self.nodes.values():
            if np.isnan(nd.uy):
                nd.uy = self.U[nd.label]["uy"]
            if np.isnan(nd.ur):
                nd.ur = self.U[nd.label]["ur"]
                    
        # For nodal forces/reactions
        self.NF = self.F.copy()
        self.VU = [node[key] for node in self.U.values() for key in ("uy","ur")]
        nf_calc = np.dot(self.KG, self.VU)
        for k in range(2*self.get_number_of_nodes()):
            nd, var = self.index2key(k, ("fy","m"))
            self.NF[nd][var] = nf_calc[k]
            cnlab = np.floor(k/float(self.dof))
            if var=="fy": 
                self.nodes[cnlab].fy = nf_calc[k]
            elif var=="m": 
                self.nodes[cnlab].m = nf_calc[k]
            
    def index2key(self,idx,opts=("uy","ur")):
        node = idx//2
        var = opts[0] if ((-1)**idx)==1 else opts[1]
        return node,var
        
    def plot_model(self):
        import matplotlib.pyplot as plt
        
        fig = plt.figure()
        ax = fig.add_subplot(111)
        
        for elm in self.get_elements():
            ni,nj = elm.get_nodes()
            xx = [ni.x, nj.x]
            yy = [ni.y, nj.y]
            ax.plot(xx, yy, "r.-")
            for nd in (ni,nj):
                if nd.fx > 0: self._draw_xforce(ax,nd.x,nd.y,1)
                if nd.fx < 0: self._draw_xforce(ax,nd.x,nd.y,-1)
                if nd.fy > 0: self._draw_yforce(ax,nd.x,nd.y,1)
                if nd.fy < 0: self._draw_yforce(ax,nd.x,nd.y,-1)
                if nd.ux == 0: self._draw_xconstraint(ax,nd.x,nd.y)
                if nd.uy == 0: self._draw_yconstraint(ax,nd.x,nd.y)
            
        ax.axis("equal")
        x0,x1,y0,y1 = self.rect_region()
        ax.set_xlim(x0,x1)
        ax.set_ylim(y0,y1)

    def _draw_xforce(self,axes,x,y,ddir=1):
        """
        Draw horizontal arrow -> Force in x-dir
        """
        dx, dy = self._calculate_arrow_size(), 0
        HW = dx/5.0
        HL = dx/3.0
        arrow_props = dict(head_width=HW, head_length=HL, fc='r', ec='r')
        axes.arrow(x, y, ddir*dx, dy, **arrow_props)
        
    def _draw_yforce(self,axes,x,y,ddir=1):
        """
        Draw vertical arrow -> Force in y-dir
        """
        dx,dy = 0, self._calculate_arrow_size()
        HW = dy/5.0
        HL = dy/3.0
        arrow_props = dict(head_width=HW, head_length=HL, fc='r', ec='r')
        axes.arrow(x, y, dx, ddir*dy, **arrow_props)
        
    def _draw_xconstraint(self,axes,x,y):
        axes.plot(x, y, "g<", markersize=10, alpha=0.6)
    
    def _draw_yconstraint(self,axes,x,y):
        axes.plot(x, y, "gv", markersize=10, alpha=0.6)
        
    def _calculate_arrow_size(self):
        x0,x1,y0,y1 = self.rect_region(factor=10)
        sf = 5e-2
        kfx = sf*(x1-x0)
        kfy = sf*(y1-y0)
        return np.mean([kfx,kfy])

    def rect_region(self,factor=7.0):
        nx,ny = [],[]
        for n in self.get_nodes():
            nx.append(n.x)
            ny.append(n.y)
        xmn,xmx,ymn,ymx = min(nx),max(nx),min(ny),max(ny)
        kx = (xmx-xmn)/factor
        if ymx==0 and ymn==0:
            ky = 1.0/factor
        else:
            ky = (ymx-ymn)/factor
        return xmn-kx, xmx+kx, ymn-ky, ymx+ky
        
    def plot_disp(self, df = 1000, **kwargs):
        fig = plt.figure()
        ax = fig.add_subplot(111)
        
        xx = []
        yy = []
        for elm in self.get_elements():
            ni,nj = elm.get_nodes()
            xx.append( ni.x )
            xx.append( nj.x )
            yy.append( ni.y+ni.uy*df )
            yy.append( nj.y+nj.uy*df )
        
        ax.plot(xx, yy, "ro--", **kwargs)
            
        ax.axis("equal")
        
    def plot_moment_diagram(self):
        import matplotlib.pyplot as plt
        
        fig = plt.figure()
        ax = fig.add_subplot(111)
        
        X,M = self._get_data_for_moment_diagram()
        ax.plot(X, M, "r")
        ax.fill_between(X, M, facecolor="#EE5B5B")
        
    def plot_shear_diagram(self):
        import matplotlib.pyplot as plt
        
        fig = plt.figure()
        ax = fig.add_subplot(111)
        
        X,S = self._get_data_for_shear_diagram()
        ax.plot(X, S, "b")
        ax.fill_between(X, S, facecolor="#559EE5")
        
    def _get_data_for_moment_diagram(self):
        cx = 0
        X, M = [], []
        for el in self.get_elements():
            L = el.L
            X = np.concatenate((X, np.array([cx, cx+L])))
            mel = el.m.squeeze()
            mel[0] = - mel[0]
            M = np.concatenate((M, mel))
            cx = cx + L
        return X, M
        
    def _get_data_for_shear_diagram(self):
        cx = 0
        X, S = [], []
        for el in self.get_elements():
            L = el.L # element length
            X = np.concatenate((X, np.array([cx, cx+L])))
            fel = el.fy.squeeze()
            fel[-1] = - fel[-1]
            S = np.concatenate((S, fel))
            cx = cx + L
        return X, S
    
    def show(self):
        import matplotlib.pyplot as plt
        plt.show()


#~ *********************************************************************
#~ ****************************  LinearTriangleModel *******************
#~ *********************************************************************    
class LinearTriangleModel(Model):
    """
    Model for finite element analysis
    """
    def __init__(self,name="LT Model 01"):
        Model.__init__(self,name=name,mtype="triangle")
        self.F = {} # Forces
        self.U = {} # Displacements
        self.dof = 2 # 2 DOF for triangle element (per node)
        self.IS_KG_BUILDED = False
        
    def build_global_matrix(self):
        """
        Build global matrix -> KG
        """
        msz = (self.dof)*self.get_number_of_nodes()
        self.KG = np.zeros((msz,msz))
        for element in self.elements.values():
            ku = element.get_element_stiffness()
            n1,n2,n3 = element.get_nodes()
            i, j, m = n1.label, n2.label, n3.label
            self.KG[2*i,2*i] += ku[0,0]
            self.KG[2*i,2*i+1] += ku[0,1]
            self.KG[2*i,2*j] += ku[0,2]
            self.KG[2*i,2*j+1] += ku[0,3]
            self.KG[2*i,2*m] += ku[0,4]
            self.KG[2*i,2*m+1] += ku[0,5]
            self.KG[2*i+1,2*i] += ku[1,0]
            self.KG[2*i+1,2*i+1] += ku[1,1]
            self.KG[2*i+1,2*j] += ku[1,2]
            self.KG[2*i+1,2*j+1] += ku[1,3]
            self.KG[2*i+1,2*m] += ku[1,4]
            self.KG[2*i+1,2*m+1] += ku[1,5]
            self.KG[2*j,2*i] += ku[2,0]
            self.KG[2*j,2*i+1] += ku[2,1]
            self.KG[2*j,2*j] += ku[2,2]
            self.KG[2*j,2*j+1] += ku[2,3]
            self.KG[2*j,2*m] += ku[2,4]
            self.KG[2*j,2*m+1] += ku[2,5]
            self.KG[2*j+1,2*i] += ku[3,0]
            self.KG[2*j+1,2*i+1] += ku[3,1]
            self.KG[2*j+1,2*j] += ku[3,2]
            self.KG[2*j+1,2*j+1] += ku[3,3]
            self.KG[2*j+1,2*m] += ku[3,4]
            self.KG[2*j+1,2*m+1] += ku[3,5]
            self.KG[2*m,2*i] += ku[4,0]
            self.KG[2*m,2*i+1] += ku[4,1]
            self.KG[2*m,2*j] += ku[4,2]
            self.KG[2*m,2*j+1] += ku[4,3]
            self.KG[2*m,2*m] += ku[4,4]
            self.KG[2*m,2*m+1] += ku[4,5]
            self.KG[2*m+1,2*i] += ku[5,0]
            self.KG[2*m+1,2*i+1] += ku[5,1]
            self.KG[2*m+1,2*j] += ku[5,2]
            self.KG[2*m+1,2*j+1] += ku[5,3]
            self.KG[2*m+1,2*m] += ku[5,4]
            self.KG[2*m+1,2*m+1] += ku[5,5]
            
        self.build_forces_vector()
        self.build_displacements_vector()
        self.IS_KG_BUILDED = True
    
    def build_forces_vector(self):
        for node in self.nodes.values():
            self.F[node.label] = {"fx":0.0, "fy":0.0} # (fy, m)
            
    def build_displacements_vector(self):
        for node in self.nodes.values():
            self.U[node.label] = {"ux":np.nan, "uy":np.nan} # (uy, r)
    
    def add_force(self,node,force):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        self.F[node.label]["fx"] = force[0]
        self.F[node.label]["fy"] = force[1]
        node.fx = force[0]
        node.fy = force[1]
        
    def add_moment(self,node,moment):
        pass
        
    def add_constraint(self,node,**constraint):
        if not(self.IS_KG_BUILDED): self.build_global_matrix()
        cs = constraint
        if "ux" in cs and "uy" in cs: # 
            ux = cs.get('ux')
            uy = cs.get('uy')
            node.set_displacements(ux=ux, uy=uy)
            self.U[node.label]["ux"] = ux
            self.U[node.label]["uy"] = uy
        elif "uy" in cs:
            uy = cs.get('uy')
            node.set_displacements(uy=uy)
            self.U[node.label]["uy"] = uy
        
    def _check_nodes(self):
        for node in self.get_nodes():
            if node._elements == []: self.add_constraint(node, ux=0, uy=0)
        
    def solve(self):
        self._check_nodes()
        # Solve LS
        self.VU = [node[key] for node in self.U.values() for key in ("ux","uy")]
        self.VF = [node[key] for node in self.F.values() for key in ("fx","fy")]
        knw = [pos for pos,value in enumerate(self.VU) if not value is np.nan]
        unknw = [pos for pos,value in enumerate(self.VU) if value is np.nan]
        self.K2S = np.delete(np.delete(self.KG,knw,0),knw,1)
        self.F2S = np.delete(self.VF,knw,0)
        
        # For displacements
        try:
            self.solved_u = la.solve(self.K2S,self.F2S)
        except:
            print("Solved using LSTSQ")
            self.solved_u = la.lstsq(self.K2S, self.F2S)[0]
            
        for k,ic in enumerate(unknw):
            nd, var = self.index2key(ic)
            self.U[nd][var] = self.solved_u[k]
            
        # Updating nodes displacements
        for nd in self.nodes.values():
            if np.isnan(nd.ux):
                nd.ux = self.U[nd.label]["ux"]
            if np.isnan(nd.uy):
                nd.uy = self.U[nd.label]["uy"]
                    
        # For nodal forces/reactions
        self.NF = self.F.copy()
        self.VU = [node[key] for node in self.U.values() for key in ("ux","uy")]
        nf_calc = np.dot(self.KG, self.VU)
        for k in range(2*self.get_number_of_nodes()):
            nd, var = self.index2key(k, ("fx","fy"))
            self.NF[nd][var] = nf_calc[k]
            cnlab = np.floor(k/float(self.dof))
            if var=="fx": 
                self.nodes[cnlab].fx = nf_calc[k]
            elif var=="fy": 
                self.nodes[cnlab].fy = nf_calc[k]
                
    def index2key(self,idx,opts=("ux","uy")):
        """
        Index to key, where key can be ux or uy
        """
        node = idx//2
        var = opts[0] if ((-1)**idx)==1 else opts[1]
        return node,var

    def plot_model(self):
        """
        Plot the mesh model, including bcs
        """
        import matplotlib.pyplot as plt
        from matplotlib.patches import Polygon
        from matplotlib.collections import PatchCollection
        
        fig = plt.figure()
        ax = fig.add_subplot(111)

        _x,_y = [],[]
        patches = []
        for k,elm in enumerate(self.get_elements()):
            _x,_y,_ux,_uy = [],[],[],[]
            for nd in elm.nodes:
                if nd.fx != 0: self._draw_xforce(ax,nd.x,nd.y)
                if nd.fy != 0: self._draw_yforce(ax,nd.x,nd.y)
                if nd.ux == 0 and nd.uy == 0: self._draw_xyconstraint(ax,nd.x,nd.y)
                _x.append(nd.x)
                _y.append(nd.y)
            polygon = Polygon(list(zip(_x,_y)), True)
            patches.append(polygon)

        pc = PatchCollection(patches, color="#7CE7FF", edgecolor="k", alpha=0.4)
        ax.add_collection(pc)
        x0,x1,y0,y1 = self.rect_region()
        ax.set_xlim(x0,x1)
        ax.set_ylim(y0,y1)
        ax.set_title("Model %s"%(self.name))
        ax.set_aspect("equal")

    def _draw_xforce(self,axes,x,y):
        """
        Draw horizontal arrow -> Force in x-dir
        """
        dx, dy = self._calculate_arrow_size(), 0
        HW = dx/5.0
        HL = dx/3.0
        arrow_props = dict(head_width=HW, head_length=HL, fc='r', ec='r')
        axes.arrow(x, y, dx, dy, **arrow_props)
        
    def _draw_yforce(self,axes,x,y):
        """
        Draw vertical arrow -> Force in y-dir
        """
        dx,dy = 0, self._calculate_arrow_size()
        HW = dy/5.0
        HL = dy/3.0
        arrow_props = dict(head_width=HW, head_length=HL, fc='r', ec='r')
        axes.arrow(x, y, dx, dy, **arrow_props)
        
    def _draw_xyconstraint(self,axes,x,y):
        axes.plot(x, y, "gv", markersize=10, alpha=0.6)
        axes.plot(x, y, "g<", markersize=10, alpha=0.6)
        
    def _calculate_arrow_size(self):
        x0,x1,y0,y1 = self.rect_region(factor=10)
        sf = 8e-2
        kfx = sf*(x1-x0)
        kfy = sf*(y1-y0)
        return np.mean([kfx,kfy])
        
    def _get_tri(self):
        import matplotlib.tri as tri
        
        _x,_y = [],[]
        # ~ df = 1
        for n in self.get_nodes():
            _x.append(n.x)
            # ~ _x.append(n.x + n.ux*df)
            _y.append(n.y)
            # ~ _y.append(n.y + n.uy*df)
            
        tg = []
        for e in self.get_elements():
            ni,nj,nm = e.get_nodes()
            tg.append([ni.label, nj.label, nm.label])
            
        tr = tri.Triangulation(_x,_y, triangles=tg)
        return tr


    def plot_nsol(self,var="ux"):
        import matplotlib.pyplot as plt
        import numpy as np
        
        fig = plt.figure()
        ax = fig.add_subplot(111)
        
        solutions = {
             "ux": (n.ux for n in self.get_nodes()),
             "uy": (n.uy for n in self.get_nodes()),
             "usum": (np.sqrt(n.ux**2 + n.uy**2) for n in self.get_nodes()),
             "sxx": (n.sx for n in self.get_nodes()),
             "syy": (n.sy for n in self.get_nodes()),
             "sxy": (n.sxy for n in self.get_nodes()),
             "seqv": (n.seqv for n in self.get_nodes()),
             "exx": (n.ex for n in self.get_nodes()),
             "eyy": (n.ey for n in self.get_nodes()),
             "exy": (n.exy for n in self.get_nodes())
             }
        
        tr = self._get_tri()
        try:
            fsol = list(solutions.get(var))
        except:
            return None
        if isinstance(fsol,list): fsol = np.array(fsol)
        tp = ax.tricontourf(tr, fsol, cmap="jet")
        fig.colorbar(tp)
        x0,x1,y0,y1 = self.rect_region()
        ax.set_xlim(x0,x1)
        ax.set_ylim(y0,y1)
        ax.set_aspect("equal")
        ax_title = "{0} (Max:{1:0.3e}, Min:{2:0.3e})".format(var,fsol.max(),fsol.min())
        ax.set_title(ax_title, fontsize=8)


    def plot_esol(self,var="ux"):
        import matplotlib.pyplot as plt
        import numpy as np
        from matplotlib.patches import Polygon
        from matplotlib.collections import PatchCollection
        
        fig = plt.figure()
        ax = fig.add_subplot(111)

        _x,_y = [],[]
        patches = []
        for k,elm in enumerate(self.get_elements()):
            _x,_y,_ux,_uy = [],[],[],[]
            for nd in elm.nodes:
                _x.append(nd.x)
                _y.append(nd.y)
            polygon = Polygon(list(zip(_x,_y)), True)
            patches.append(polygon)
            
        pc = PatchCollection(patches, cmap="jet", alpha=1)
        solutions = {
             "sxx": (e.sx for e in self.get_elements()),
             "syy": (e.sy for e in self.get_elements()),
             "sxy": (e.sxy for e in self.get_elements()),
             "exx": (e.ex for e in self.get_elements()),
             "eyy": (e.ey for e in self.get_elements()),
             "exy": (e.exy for e in self.get_elements())
             }
        fsol = np.array(list(solutions.get(var.lower())))
        pc.set_array(fsol)
        ax.add_collection(pc)
        fig.colorbar(pc)
        x0,x1,y0,y1 = self.rect_region()
        ax.set_xlim(x0,x1)
        ax.set_ylim(y0,y1)
        ax.set_aspect("equal")
        ax_title = "{0} (Max:{1:0.3e}, Min:{2:0.3e})".format(var,fsol.max(),fsol.min())
        ax.set_title(ax_title, fontsize=8)
        
    def show(self):
        """
        Show matplotlib plots
        """
        import matplotlib.pyplot as plt
        plt.show()
    
    def calculate_deformed_factor(self):
        x0,x1,y0,y1 = self.rect_region()
        ux = np.array([n.ux for n in self.get_nodes()])
        uy = np.array([n.uy for n in self.get_nodes()])
        sf = 1.5e-2
        kfx = sf*(x1-x0)/ux.max()
        kfy = sf*(y1-y0)/uy.max()
        return np.mean([kfx,kfy])
                
    def rect_region(self,factor=7.0):
        nx,ny = [],[]
        for n in self.get_nodes():
            nx.append(n.x)
            ny.append(n.y)
        xmn,xmx,ymn,ymx = min(nx),max(nx),min(ny),max(ny)
        kx = (xmx-xmn)/factor
        ky = (ymx-ymn)/factor
        return xmn-kx, xmx+kx, ymn-ky, ymx+ky




if __name__=='__main__':
    pass
