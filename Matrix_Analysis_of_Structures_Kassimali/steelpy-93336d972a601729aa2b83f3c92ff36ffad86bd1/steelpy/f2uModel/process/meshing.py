#
# Copyright (c) 2009 steelpy
#

# Python stdlib imports
from __future__ import annotations

import math
from collections import defaultdict
from dataclasses import dataclass
#from typing import Tuple
import os

# package imports
from steelpy.utils.units.main import Units
from steelpy.utils.geometry.L3D import DistancePointLine3D
from steelpy.f2uModel.mesh.main import Mesh
#
#
class Meshing:
    __slots__ = ["_mesh", "concept"]
    
    def __init__(self, concept, component:str) -> None:
        """
        """
        self.concept = concept
        #
        #filename = component + "_f2u.db"
        #path = os.path.abspath(filename)
        #db_file = path        
        #
        self._mesh = Mesh(db_name=component)
                          #materials=concept._materials,
                          #sections=concept._sections,
                          #mesh_type=mesh_type,
                          
        #print('--')
    #
    def get_mesh(self) -> None:
        """
        """
        print('-- Meshing Component')
        self._set_properties()
        self._set_mesh()
        self._set_boundary()
        self._set_load()
        print('-- Meshing Completed')
        return self._mesh
    #
    def _set_properties(self):
        """ """
        # Materials
        cmaterial = self.concept.materials()
        dfmat = cmaterial.df
        #
        group = dfmat.groupby("type")
        elastic = group.get_group("elastic")
        elastic = elastic.drop_duplicates(subset=['name'], keep='first')  
        self._mesh._materials._material._elastic.df = elastic
        #self._mesh._materials.df = dfmat
        #
        # Sections
        csections = self.concept.sections()
        dfsect = csections.df
        dfsect = dfsect.drop_duplicates(subset=['name'], keep='first')        
        self._mesh._sections.df = dfsect
        #
        #print('-->')
        #1 / 0
    #
    def _set_mesh(self):
        """ """
        print('--- Meshing Concepts')
        mesh = self._mesh
        melements = mesh.elements()
        #elem_number = elements.get_number()
        celements = self.concept.elements()
        cbeams = celements.beams()
        #
        for key, beam in cbeams.items():
            total_length = beam.L
            p1, p2 = beam.nodes
            node_res = self._get_node_name(p1[:3])
            node_end = self._get_node_name(p2[:3])
            for step in beam.step:
                mnumber = next(melements.get_number())
                step._mesh = mnumber
                print(f"concept: {key} --> element: {mnumber}")
                try:
                    1/step.length #.value
                    total_length -= step.length
                    coord = beam.find_coordinate(step.length)
                    new_node = self._get_node_name(coord)
                    # elements [node1, node2, material, section, beta, title]
                    melements[mnumber] = ['beam', node_res, new_node,
                                          step.material.name, 
                                          step.section.name, 
                                          beam.beta, key]
                    node_res = new_node
                except ZeroDivisionError:
                    # elements [node1, node2, material, section, beta, title]
                    melements[mnumber] = ['beam', node_res, node_end,
                                          step.material.name, 
                                          step.section.name, 
                                          beam.beta, key]
            #print('-->')
        #print('end meshing')
    #
    def _get_node_name(self, coord):
        """ """
        nodes = self._mesh.nodes()
        try:
            return nodes.get_node_name(coord)
        except IOError:
            return nodes.get_new_node(coord)
    #
    def _set_boundary(self):
        """ """
        print('--- Meshing Boundaries')
        units = Units()
        # Mesh
        mesh = self._mesh
        mnodes = mesh.nodes()
        melements = mesh.elements()
        mbeams = melements.beams()
        mboundaries = mesh.boundaries()
        msupports = mboundaries.supports()
        # concepts
        cboundary = self.concept.boundaries()
        csupports = cboundary.supports()
        celements = self.concept.elements()
        cbeams = celements.beams()
        #
        # FIXME : check if new loops this works properly
        #
        missing = defaultdict(list)
        # find existing nodes
        for key, value in csupports.items():
            #support = value.support
            support = value.points
            for point in support.points:
                try:
                    node_id = mnodes.get_node_name(point)
                    msupports[node_id] = [*support[:6], key]
                    print(f"Boundary: {key}  @ Node: {node_id}")
                except IOError:
                    missing[key].append(point)
        #
        # if missing boundaries, find if coordinates along members
        #if missing:
        for boundary, points, in missing.items():
            missing_found = [item.name for item in points] #defaultdict(list)
            for key, cbeam in cbeams.items():
                p1, p2 = cbeam.nodes
                #p1, p2 = cbeam.connectivity
                point_line = DistancePointLine3D(p1[:3], p2[:3])
                #for boundary, points, in missing.items():
                #Pb = cboundary[boundary].point
                # Fixme : what format?
                #Pb = [Pb[0].value, Pb[1].value, Pb[2].value]
                for point in points:
                    Pb = point[:3]
                    if point_line.is_on_segment(Pb):
                        missing_found.remove(point.name)
                        left_dist = point_line.left_dist
                        #missing_found[boundary].append(idx)
                        total_length = 0
                        step_no = len(cbeam.step)
                        for step in cbeam.step:
                            beam = mbeams[step._mesh]
                            total_length += beam.L
                            # TODO: capture warning when point misaligned due to tolerances
                            print(f"beam length step: {total_length:1.4e} left: {left_dist:1.4e}")
                            if total_length < left_dist:
                                continue
                            # get node coordinate
                            coord = cbeam.find_coordinate(left_dist) #*units.m
                            new_node = self._get_node_name(coord)
                            # set boundary
                            #support = cboundary[boundary].support
                            #support = csupports[boundary]
                            #if support:
                            msupports[new_node] = boundary
                            print(f"Boundary: {boundary} on Beam: {key} @ Node: {new_node}")
                            #
                            # existing element
                            mnodes = beam.connectivity
                            node_end = mnodes[-1]
                            mnodes[-1] = new_node
                            beam.connectivity = mnodes
                            # new element
                            mnumber = next(melements.get_number())
                            melements[mnumber] = ['beam', new_node, node_end,
                                                  step.material.name,
                                                  step.section.name,
                                                  beam.beta, cbeam.name]
                            # introduce new concept beam step to boundary coord
                            step_no += 1
                            cbeams[key].step[step_no].length = left_dist * units.m
                            cbeams[key].step[step_no].material = step.material.name
                            cbeams[key].step[step_no].section = step.section.name
                            cbeams[key].step[step_no]._mesh = mnumber
                            print(f"concept: {key} --> element: {mnumber}")
                            break
                        #continue
                    if not missing_found:
                        break
                #
                if not missing_found:
                    break
                #
                #for item in reversed(missing_found):
                #    #for item in reversed(items):
                #    #missing.remove(item)
                #    missing[boundary].pop(item)
                # TODO: capture if missing not empty
                #if not missing[boundary]:
                #    #del missing[boundary]
                #    break
        #print(' end meshing boundary')
    #
    def _set_load(self):
        """ """
        print( '--- Meshing Basic Load' )
        # Mesh
        mesh = self._mesh
        mnodes = mesh.nodes()
        melements = mesh.elements()
        mbeams = melements.beams()
        # Mesh Load
        mload = mesh.load()
        mlbasic = mload.basic()
        # Concept
        Concept = self.concept
        Celements = Concept.elements()
        Cbeams = Celements.beams()
        Cloads = Concept.load()
        Clbasic = Cloads.basic()
        CPoints = Concept.points()
        #
        for Clb_name, Clb_item in Clbasic.items():
            # clone mesh load
            mlbasic[Clb_name] = Clb_item.title
            mlb_node = mlbasic[Clb_name].node()
            mlb_beam = mlbasic[Clb_name].beam()
            #
            # Beam load process
            # TODO : update linefit
            for bname, CBloads in Clb_item.beams.items():
                cbeam = Cbeams[bname]
                Lc = cbeam.L #.value
                #print(f'---> Load: {load_name} Beam: {bname} L: {Lb:4.2f}')
                # Beam line load process
                for lbload in CBloads.line:
                    label = lbload.load_name
                    print(f'Load Title: {label} - Line Load')
                    waxial = linefit(lbload.qx0, lbload.qx1,
                                     Lc, lbload.L0, lbload.L1)
                    winplane = linefit(lbload.qy0, lbload.qy1,
                                       Lc, lbload.L0, lbload.L1)
                    woutplane = linefit(lbload.qz0, lbload.qz1,
                                        Lc, lbload.L0, lbload.L1)
                    # start loop beam steps
                    xi = 0
                    for step in cbeam.step:
                        bname = step._mesh
                        beam = mbeams[bname]
                        Lbi = beam.L
                        xi += Lbi
                        qaxial = waxial.qi(xi)
                        qinp = winplane.qi(xi)
                        qoutp = woutplane.qi(xi)
                        # check load on segment
                        try:
                            Li = winplane.Li(xi, Lbi)
                        except RuntimeWarning:
                            continue # no load should be applied to this segment
                        # set load for mesh element
                        print(f'Element: {bname} --> {Lbi:4.2f} {xi:4.2f} {qinp} {qoutp} {Li}')
                        mlb_beam[bname].line = [qaxial[0], qinp[0], qoutp[0],
                                                qaxial[1], qinp[1], qoutp[1],
                                                Li[0], Li[1], lbload.title]
                #
                # Beam point load process
                for pbload in CBloads.point:
                    label = pbload.load_name
                    print(f'Load Title: {label} - Point Load')
                    L1 = pbload.distance
                    # start loop beam steps
                    xi = 0                    
                    for step in cbeam.step:
                        bname = step._mesh
                        beam = mbeams[bname]
                        Lbs = beam.L
                        xi += Lbs
                        #
                        if xi < L1: # no load for this beam step
                            continue
                        else:
                            L2 = xi - L1
                            Li = (Lbs - L2)
                            self._beam_pload(beam, pbload,
                                             mlb_node, mlb_beam,
                                             Li=Li)
                            break
            #
            # Nodal load process
            for CPname, CPloads in Clb_item.points.items():
                point = CPoints[CPname]
                for pload in CPloads.load:
                    label = pload.load_name
                    print(f'Load Title: {label} - Nodal Load')
                    #
                    try:
                        node_name = mnodes.get_node_name(point[:3])
                        node = mnodes[node_name]
                        print(f'---> Load: {CPname} Point: {node_name} Node: {node.name}')
                        mlb_node[node.name].load = [*pload[:6], pload[7]]
                    except IOError: # check if point load on a beam
                        for beam_name, beam in mbeams.items():
                            # TODO: avoid to loop all beams elements
                            if beam.intersect_point(point):
                                self._beam_pload(beam, pload,
                                                 mlb_node, mlb_beam, 
                                                 point=point)
                                break
            #
            # wave
            for wname, wload in Clb_item.wave.items():
                mlbasic[Clb_name].wave(wave_load=wload.seastate,
                                       design_load=wload.design_load,
                                       criterion=wload.criterion)
        #
        #print('--> end mesh loading')
    #
    #
    def _beam_pload(self, beam, pload,
                    node_load, beam_load,
                    Li=None, point=None):
        """ """
        n1, n2 = beam.nodes
        if point:
            L1 = n1.distance(point)
            L2 = n2.distance(point)
        else:
            L1 = Li
            L2 = beam.L - Li
        #
        # TODO : adjust tolerancea
        if math.isclose(L1, 0, rel_tol=1e-09, abs_tol=0.0):
            print(f'Node {n1.name} Load')
            node_load[n1.name].load = [*pload[:6], pload.title]
        elif math.isclose(L2, 0, rel_tol=1e-09, abs_tol=0.0):
            print(f'Node {n2.name} Load')
            node_load[n2.name].load = [*pload[:6], pload.title]
        else:
            print(f'Beam {beam.name} Point load L1: {L1:4.2f}')
            beam_load[beam.name].point = [L1, *pload[:6], pload.title]
        #
#
#
@dataclass
class linefit:
    __slots__ = ['q0', 'q2', 'L', 'L0', 'L1',
                 'L2', 'Lstart', 'Lstop', '_qi']

    def __init__(self, q0:float, q2:float,
                 L:float, L0:float, L1:float) -> None:
        """ """
        self.q0:float = q0
        self.q2:float = q2
        self._qi:float = q0
        #
        self.L:float = L
        self.L0:float = L0
        self.L1:float = L1
        self.L2 = self.L - self.L1
        self.Lstop = self.L - self.L1
    #
    @property
    def slope(self) -> float:
        """ """
        return (self.q2-self.q0)/(self.L2-self.L0)
    #
    def qi(self, x:float) -> list[float]:
        """ """
        q1 = self._qi
        if x > self.L2:
            self._qi = self.q2
            #q2 = round(self.q1 + self.slope * (self.L3-self.L1), 3)
        else:
            self._qi = round(self.q0 + self.slope * (x-self.L0), 3)
        #self._qi = q2
        return [q1, self._qi]
    #
    def Li(self, x:float, Lb:float):
        """ """
        try:
            1/(self.L0 + self.L1)
            if x < self.L0: # no load for this step
                raise RuntimeWarning
            else:
                try:
                    1 / self.Lstop
                    try:
                        Lstart = self.Lstart
                    except AttributeError:
                        Lstart = Lb - (x - self.L0)
                        self.Lstart = 0
                    #
                    if x > self.L2:
                        self.Lstop = 0
                        return [Lstart, x - self.L2]
                    else:
                        return [Lstart, 0]
                except ZeroDivisionError: # no load after this step
                    raise RuntimeWarning
        except ZeroDivisionError:
            return [0, 0]
#
#