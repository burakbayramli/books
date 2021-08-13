function T=LMeshD1

% T=LMeshD1
%
%    This function creates the coarsest possible triangular mesh
%    for the "L"-shaped region consisting of the union of the
%    three squares (-1,0)x(0,1), (0,1)x(0,1), and (0,1)x(-1,0),
%    subject to Dirichlet conditions.
%
%    See "help Mesh1 for a description of the data structures.
%    See Refine1 to refine the mesh.

NL=[
0 -1
1 -1
-1 0
0 0
1 0
-1 1
0 1
1 1];
NP=[-1;-2;-3;-4;-5;-6;-7;-8];
FNP=[];

EL=[
1 4 5
1 2 5
3 6 7
3 4 7
4 7 8
4 5 8
];

EEL=[
-1 0 0
-1 -1 0
-1 -1 0
-1 0 0
0 -1 0
0 -1 0
];

BL=[];

T.NodeList=NL;
T.NodePtrs=NP;
T.FNodePtrs=FNP;
T.CNodePtrs=(1:8)';
T.ElList=EL;
T.ElEdgeList=EEL;
T.FBndyList=BL;
