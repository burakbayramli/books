% The Fem1 package implements piecewise linear finite elements for two
% dimensional problems and is intended to accompany "Partial
% Differential Equations: Analytical and Numerical Methods" (second
% edition) by Mark S. Gockenbach (SIAM 2010).  It uses the data
% structure described in Section 13.1 of the text (that is, the four
% arrays NodeList, NodePtrs, FNodePtrs, and ElList, augmented by
% three more arrays (CNodePtrs, ElEdgeList, and FBndyList) to allow
% the code to handle inhomogeneous boundary conditions (as hinted at
% in the text).  These seven arrays are collected into a structure.
%
%    NodeList: Mx2 matrix, where M is the number of nodes in the mesh
%              (including boundary nodes).  Each row corresponds to a
%              node and contains the coordinates of the node.
%
%    NodePtrs: Mx1 matrix.  Each entry corresponds to a node:
%               if node i is free, NodePtrs(i) is the index of node i
%               in FNodePtrs; else NodePtrs(i) is the negative of
%               index of node i in CNodePtrs
%
%    FNodePtrs: Nx1 vector, where N is the number of free nodes.
%               NodePtrs(i) is the index into NodeList of the ith
%               free node.  So, for example, NodeList(NodePtrs(i),1:2)
%               are the coordinates of the ith free node.
%
%    CNodePtrs: Kx1 vector, where K=M-N is the number of constrained
%               nodes.  CNodePtrs(i) is the index into NodeList of the
%               ith constrained node.
%
%    ElList: Lx3 matrix, where L is the number of triangular elements
%            in the mesh.  Each row corresponds to one element and
%            contains pointers to the nodes of the triangle in
%            NodeList.
%
%    ElEdgeList: Lx3 matrix.  Each row contains flags indicating
%                whether each edge of the corresponding element is on
%                the boundary (flag is -1 if the edge is a constrained
%                boundary edge, otherwise it equals the index of the
%                edge in FBndyList) or not (flag is 0).  The edge
%                of the triangle are, in order, those joining vertices
%                1 and 2, 2 and 3, and 3 and 1.
%
%    FBndyList: Bx2 matrix, where B is the number of boundary edges
%               not constrained by Dirichlet conditions.  Each
%               row corresponds to one edge and contains pointers
%               into NodeList, yielding the two vertices of the edge.
