function k = FindEdge(p1,p2,EdgeList)

% k = FindEdge(p1,p2,EdgeList)
%
%    This is a function used by Refine1 (which refines a triangular mesh).
%    The input EdgeList must be a Kxp array, K >= 0, p>=2; if, for any i,
%    Edgelist(i,1:2) == [p1,p2] or EdgeList[i,1:2] == [p2 p1], then
%    the function returns with k=i; otherwise, the function
%    returns with k = 0.

k = 0;

K=size(EdgeList,1);
if K>0
   i=find(EdgeList(:,1)==p1&EdgeList(:,2)==p2);
   if ~isempty(i)
      k=i(1);
   else
      i=find(EdgeList(:,1)==p2&EdgeList(:,2)==p1);
      if ~isempty(i)
         k=i(1);
      else
         k=0;
      end
   end
end
