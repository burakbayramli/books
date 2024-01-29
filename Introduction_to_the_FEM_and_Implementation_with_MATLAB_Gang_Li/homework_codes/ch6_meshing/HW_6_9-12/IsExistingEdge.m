function  re=IsExistingEdge(a,b)
global edges nodes;
% for-block: test the edges connected to node "a"
for i=1:nodes(a,3)
  if (edges(nodes(a,3+i),1)==b)||(edges(nodes(a,3+i),2)==b)
    re=nodes(a,3+i); return;           % returning the edge number
  end
end
re=0;  % return 0 (false)