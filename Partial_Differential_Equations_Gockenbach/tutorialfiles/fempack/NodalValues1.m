function [v,g]=NodalValues1(T,u)

%[v,g]=NodalValues1(T,u)
%
%  This function sets v equal to the vector of
%  values of u(x,y) at the free nodes of the mesh T,
%  and g equal to the vector of values of u(x,y)
%  at the constrained nodes of the mesh T.
%  The function implementing u must be vectorized.
%
%  See "help Mesh1" for a description of the
%  data structure for T.

v=u(T.NodeList(T.FNodePtrs,1),T.NodeList(T.FNodePtrs,2));
if nargout>1
   g=u(T.NodeList(T.CNodePtrs,1),T.NodeList(T.CNodePtrs,2));
end

