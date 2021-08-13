% Fem1: Finite element routines for 2D problems using linear Lagrange triangles
%
% The Fem1 package is a collection of mfiles implementing the finite
% element method for 2D problems.  These routines perform calculations
% based on linear Lagrange triangles only.
%
% The code is based on Chapter 13 of "Partial Differential Equations:
% Analytical and Numerical Methods" (2nd edition) by Mark S. Gockenbach
% (SIAM, 2010).  The mesh data structured has been extended, as described
% in Section 13.1, by the addition of arrays CNodePtrs, FBndyList, and
% ElEdgeList.  The first two make it easy to solve inhomogeneous Dirichlet
% and Neumann problems, respectively, and the third makes it possible to
% write a routine to automatically refine a mesh.  However, the finite
% element code itself does not handle inhomogeneous boundary conditions.
% This extension is left to the reader as an exercise.
%
% The purpose of these routines is pedagogical.  No attempt has been made
% to implement every timesaving trick.
%
% The Fem1 package consists of the following routines, which are grouped
% by purpose:
%
%   Mesh creation and refinement:
%
%      RectangleMeshD1 - generates a regular mesh on a rectangular domain;
%                        Dirichlet boundary conditions
%
%      RectangleMeshN1 - generates a regular mesh on a rectangular domain;
%                        Neumann boundary conditions
%
%      RectangleMeshTopD1 - generates a regular mesh on a rectangular domain;
%                           Dirichlet boundary conditions on the top,
%                           Neumann boundary conditions elsewhere.
%
%   For a description of the data structure describing a triangular mesh,
%   see "help Mesh1".
%
%   Matrix/vector assembly
%
%      Stiffness1 - assembles the stiffness matrix for the standard
%                  divergence-form operator: -div(k(x)grad u)
%
%      Load1 - assembles the load vector
%
%   Graphics
%
%      ShowMesh1 - displays a triangular mesh
%
%      ShowPWLinFcn1 - displays a piecewise linear function
%
%   Norms and inner products
%
%      EnergyNorm1 - Computes the energy norm of a smooth function or
%                   a piecewise linear function
%
%      EnergyNormErr1 - Computes the energy norm of the difference between
%                      a smooth function and a piecewise linear function
%
%      L2Norm1 - Computes the L2 norm of a smooth function or
%                   a piecewise linear function
%
%      L2NormErr1 - Computes the L2 norm of the difference between
%                      a smooth function and a piecewise linear function
%
%   Input/output
%
%      WriteMesh1 - writes a mesh to a file
%
%      ReadMesh1 - read a mesh from a file
%
%   Examples
%
%      Example1 - solves a Dirichlet problem on the unit square
%
%      Example2 - solves a problem with mixed boundary conditions
%                 on a rectangle

