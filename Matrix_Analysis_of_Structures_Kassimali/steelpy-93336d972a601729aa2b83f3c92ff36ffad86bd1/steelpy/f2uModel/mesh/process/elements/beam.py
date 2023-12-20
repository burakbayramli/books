# 
# Copyright (c) 2019-2023 steelpy
# 

# Python stdlib imports
from __future__ import annotations
from array import array
from dataclasses import dataclass
from collections.abc import Mapping
from typing import NamedTuple

# package imports
from steelpy.utils.geometry.L3D import DistancePointLine3D, LineLineIntersect3D
from steelpy.f2uModel.mesh.process.elements.bstiffness import (beam3D_Klocal, 
                                                               Rmatrix, Rmatrix2, 
                                                               beam3D_K)
from steelpy.f2uModel.mesh.process.elements.bgeometry import kg_beam
from steelpy.f2uModel.mesh.process.elements.bmass import beam_mass
#
from steelpy.utils.math.operations import remove_column_row
#
import numpy as np
from numpy.linalg import inv

#
class BeamBasic(Mapping):
    __slots__ = ['_lables', '_number', '_title']
    
    def __init__(self, labels) -> None: #, m2D: bool
        """
        Beam element 
        """
        #
        #
        self._labels=labels
        self._number: array = array('i', [])
        self._title: list[int|str] = []
        #self._m2D = m2D
    #
    def _get_labels(self):
        """ """
        idx = [x for x, item in enumerate(self._type)
               if item == self._beam_type]
        labels = [self._labels[x] for x in idx]
        return labels
    #
    def __contains__(self, value) -> bool:
        return value in self._labels
    
    def __iter__(self):
        """
        """
        labels = self._get_labels()
        return iter(labels)    

    def __len__(self) -> float:
        labels = self._get_labels()
        return len(labels)
    #
    def __str__(self) -> str:
        """ """
        lenght = ' m'
        space = " "
        #
        output = "\n"
        output += "{:}\n".format(80*"_")
        output += "\n"
        output += f"{30*space}BEAM ELEMENTS\n"
        output += "\n"
        output += "\n"
        output += (f"Beam {7*space}Node1    Node2 {4*space}Material {5*space}Section")
        output += (f" {4*space}Beta {3*space}Len[{lenght}] {2*space}Title")
        output += "\n"
        output += "{:}\n".format(80*".")
        output += "\n"
        for beam_name in self._labels:
            beam = self.__getitem__(beam_name)
            output += beam.__str__()
        #print('beam basic')
        #1/0
        return output
#
#
#
@dataclass
class BeamItemBasic:
    """
    """
    __slots__ = ['name', '_releases', '_plane', 'type']

    def __init__(self, beam_name: int|str) -> None:
        """
        """
        self.name: int|str = beam_name
        #self._plane = plane
        #
        self._releases: list[bool] = [False] * 12
        # Kmatrix index  [z, Mx, My]
        #self._intersect = LineLineIntersect3D()
        self.type: str = "beam"
    #
    #
    # Transformation
    #
    @property
    def T(self):
        """
        Returns Beam transformation matrix
        """
        Tlg = self.T3D()
        if self._plane.plane2D:
            # removing z, Mx, My
            for i in self._plane.index_off:
                Tlg = remove_column_row(Tlg, i, i)
        return Tlg
    #
    def T3D(self):
        """ """
        nodei, nodej = self.nodes
        #return Rmatrix(*self.unit_vector, self.beta)
        return Rmatrix2(nodei, nodej, L=self.L)        
    #
    # Stiffness
    #
    @property
    def K(self):
        """
        Return Beam stiffness matrix in global coordinates
        """
        Tlg =  self.T
        Kl = self.k
        #Kl = self.k3D()
        #if self._plane.m2D:
        #    # removing z, Mx, My
        #    for i in self._plane.index_off:
        #        Kl = remove_column_row(Kl, i, i)
        #return (np.transpose(Tlg).dot(Kl)).dot(Tlg)
        return Tlg.T @ Kl @ Tlg
    #
    @property
    def k(self):
        """Return the 2D/3D stiffness matrix in local coordinates """
        kl = self.k3D()
        if self._plane.plane2D:
            for i in self._plane.index_off:
                kl = remove_column_row(kl, i, i)
        #else:
        #    return self.k3D()
        return kl
    #
    def k3D(self):
        """Returns the condensed (and expanded) local stiffness matrix for 3D beam"""
        # get section properties 
        section = self.section
        #section = self._cls._f2u_sections[section]
        section = section.properties()
        # get material properties
        material = self.material
        #material = self._cls._f2u_materials[material]        
        #
        #
        # solve K matrix
        #Kb = beam3D_Klocal(self.L,
        #                    self.area, self.J,
        #                    self.Iy, self.Iz,
        #                    self.E, self.G,
        #                    self.area, self.area)
        #        
        kb = beam3D_K(self.L,
                      section.area, section.J,
                      section.Iy, section.Iz,
                      material.E, material.G)
        #
        k_cond = self._k_unc(kb)
        return k_cond
        #return K    
    #
    # Geometry
    #
    def Kg(self, axial):
        """
        Return Beam geometrical stiffness matrix in global coordinates
        """
        Tlg =  self.T
        Kl = self.kg(axial=axial)
        #return (np.transpose(Tlg).dot(Kl)).dot(Tlg)
        return Tlg.T @ Kl @ Tlg
    #
    def kg(self, axial):
        """
        Return Beam geometrical stiffness matrix in local coordinates
        """
        kg = self.kg3D(axial=axial)
        if self._plane.plane2D:
            for i in self._plane.index_off:
                kg = remove_column_row(kg, i, i)
        return kg
    #
    def kg3D(self, axial: float = 0):
        """
        Returns the condensed (and expanded) local geometrical stiffness matrix for 3D beam
        """
        # get section properties 
        section = self.section
        section = section.properties()
        # get material properties
        material = self.material       
        #
        #        
        kg = kg_beam(axial, self.L,
                      section.area, section.J,
                      section.Iy, section.Iz,
                      material.E, material.G)
        #
        k_cond = self._k_unc(kg)
        return k_cond
    #
    # Mass
    #
    #@property
    def M(self):
        """
        Return Beam mass matrix in global coordinates
        """
        Tlg =  self.T
        Km = self.m()
        #return (np.transpose(Tlg).dot(Kl)).dot(Tlg)
        return Tlg.T @ Km @ Tlg
    #
    def m(self):
        """
        Return Beam mass matrix in local coordinates
        """
        km = self.m3D()
        if self._plane.plane2D:
            for i in self._plane.index_off:
                km = remove_column_row(km, i, i)
        return km
    #
    def m3D(self):
        """ Returns the condensed (and expanded) local mass matrix for 3D beam"""
        # get section properties 
        section = self.section
        section = section.properties()
        # get material properties
        material = self.material       
        #
        #        
        em = beam_mass(self.length,
                       section, material,
                       ilump=2)
        #
        k_cond = self._k_unc(em)
        return k_cond        
    #
    # Matrix operations
    #
    def _partition(self, unp_matrix):
        """
        Partitions a matrix into sub-matrices based on unreleased and released degree of freedom indices.
        """
        unp_matrix =  np.array(unp_matrix)
        # Create auxiliary lists of released/unreleased DOFs
        R1_indices, R2_indices = self._aux_list()
        # Partition the matrix by slicing
        if unp_matrix.shape[1] == 1:
            m1 = unp_matrix[R1_indices, :]
            m2 = unp_matrix[R2_indices, :]
            return m1, m2
        else:
            m11 = unp_matrix[R1_indices, :][:, R1_indices]
            m12 = unp_matrix[R1_indices, :][:, R2_indices]
            m21 = unp_matrix[R2_indices, :][:, R1_indices]
            m22 = unp_matrix[R2_indices, :][:, R2_indices]
            return  m11, m12, m21, m22
    #
    def _aux_list(self):
        """
        Builds lists of unreleased and released degree of freedom indices for the member.

        Returns
        -------
        R1_indices : list
            A list of the indices for the unreleased DOFs
        R2_indices : list
            A list of the indices for the released DOFs
        """
        R1i = [i for i, rel in enumerate(self._releases)
                      if not rel]
        R2i = [i for i, rel in enumerate(self._releases)
               if rel]
        return R1i, R2i
    #
    def _k_unc(self, k_unc):
        """Returns the uncondensed local stiffness matrix for the member"""
        # Partition the local stiffness matrix as 4 submatrices in
        # preparation for static condensation
        k11, k12, k21, k22 = self._partition(k_unc)
        #
        # Calculate the condensed local stiffness matrix
        k_condensed = np.subtract(k11, np.matmul(np.matmul(k12, inv(k22)), k21))
        #
        # Expand the condensed local stiffness matrix
        for i, DOF in enumerate(self._releases):
            if DOF:
                k_condensed = np.insert(k_condensed, i, 0, axis = 0)
                k_condensed = np.insert(k_condensed, i, 0, axis = 1)
        # Return the local stiffness matrix, with end releases applied
        return k_condensed
    #    
    #
    # Operations
    #
    def intersect_point(self, point:list):
        """line intersection """
        p1, p2 = self.nodes
        point_line = DistancePointLine3D(p1[:3], p2[:3])
        return point_line.is_on_segment(point[:3])
    #