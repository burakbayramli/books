%function  looplist(A);
%LOOPLIST Each row in A contains from-node, to-node
%        and observed value along the oriented edge.
%        The vector v contains all loop sums in an
%        order corresponding to the rows in A

%Kai Borre 04-16-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $date: 1997/09/26  $

global nodes
%if nargin == 0
A = [ '1' '2'  1.223;
      '1' '3'  2.620;
      '2' '3'  1.380;
      '2' '3'  1.382;
      '1' '4'  1.820;
      '2' '4'  0.600;
      '3' '4' -0.801;
      '4' '5'  0.500;
      '5' 'a'  0.500;
      'a' '3' -0.202];
%end

i_max = 100;  % max number of nodes in the graph
nodes = zeros(i_max,1);
[r,c] = size(A);
L = zeros(r,i_max);
for i = 1:r
   from_loc = n_locate(i_max, A(i,1));
   to_loc = n_locate(i_max, A(i,2));
   L(i,from_loc) = -1;
   L(i,to_loc) = 1;
end

[j,k] = find(nodes);
L = L(1:r,1:max(j));
N = f_loop(L');
for i = 1:size(N,2)
   v(i,1) = N(:,i)'*A(:,3);
   Nodes = find(N(:,i));
   n = size(Nodes,1);
   fprintf('\nEdges into loop:\n')
   for j = 1:n
      from = A(Nodes(j),1);
      to = A(Nodes(j),2);
      obs = A(Nodes(j),3);
      from = setstr(from);
      to = setstr(to);
      fprintf('From %3s to %3s : %+6.4f\n',from,to,obs)
   end
   fprintf('            Sum : %+2.4f\n',v(i))
end
fprintf('\n')
%%%%%%%%%%%% end looplist.m  %%%%%%%%%%%%%%%%
