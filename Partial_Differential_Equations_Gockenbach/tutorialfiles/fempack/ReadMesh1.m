function T=ReadMesh1(fname)

% T=ReadMesh1('file')
%
%   This function reads the description of a mesh from the file named
%   'file'.  The mesh is stored as follows:
%
%         length of NodeList
%         NodeList (one row per line)
%         NodePtrs
%         length of FNodePtrs
%         FNodePtrs
%         length of CNodePtrs
%         CNodePtrs
%         length of ElList
%         ElList (one row per line)
%         length of FBndyList
%         FBndyList (one row per line)

fid=fopen(fname);
M=fscanf(fid,'%d\n',1);
T.NodeList=reshape(fscanf(fid,'%f',2*M),2,M)';
T.NodePtrs=fscanf(fid,'%d\n',M);
N=fscanf(fid,'%d\n',1);
T.FNodePtrs=fscanf(fid,'%d\n',N);
K=fscanf(fid,'%d\n',1);
T.CNodePtrs=fscanf(fid,'%d\n',K);
L=fscanf(fid,'%d\n',1);
T.ElList=reshape(fscanf(fid,'%d\n',3*L),3,L)';
T.ElEdgeList=reshape(fscanf(fid,'%d\n',3*L),3,L)';
B=fscanf(fid,'%d\n',1);
T.FBndyList=reshape(fscanf(fid,'%d',2*B),2,B)';

