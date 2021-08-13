function WriteMesh1(T,fname)

% WriteMesh1(T,'file')
%
%   This function writes the description of a mesh to the file named
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
%         ElEdgeList (one row per line)
%         length of FBndyList
%         FBndyList (one row per line)

fid=fopen(fname,'w');
fprintf(fid,'%d\n',size(T.NodeList,1));
fprintf(fid,'%.16f %.16f\n',T.NodeList');
fprintf(fid,'%d\n',T.NodePtrs');
fprintf(fid,'%d\n',length(T.FNodePtrs));
fprintf(fid,'%d\n',T.FNodePtrs);
fprintf(fid,'%d\n',length(T.CNodePtrs));
fprintf(fid,'%d\n',T.CNodePtrs);
fprintf(fid,'%d\n',size(T.ElList,1));
fprintf(fid,'%d %d %d\n',T.ElList');
fprintf(fid,'%d %d %d\n',T.ElEdgeList');
fprintf(fid,'%d\n',size(T.FBndyList,1));
fprintf(fid,'%d %d\n',T.FBndyList');

