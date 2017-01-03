function  v_loops(A)
%V_LOOPS  Each row in A contains from-node, to-node
%        and three observed coordinate differences
%        along the oriented GPS-vector

%Kai Borre 04-18-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26 $

global nodes

fidlog = fopen('vectors.log','w');
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
   v(1) = N(:,i)'*A(:,3);
   v(2) = N(:,i)'*A(:,4);
   v(3) = N(:,i)'*A(:,5);
   Nodes = find(N(:,i));
   n = size(Nodes,1);
   fprintf(fidlog,'\nVectors into loop:\n')
   for j = 1:n
      from = A(Nodes(j),1);
      to =   A(Nodes(j),2);
      x =    A(Nodes(j),3);
      y =    A(Nodes(j),4);
      z =    A(Nodes(j),5);
      from = setstr(from);
      to =   setstr(to);
      fprintf(fidlog,'From %3g to %3g : %+10.4f %+10.4f %+10.4f\n',...
                                                        from,to,x,y,z)
   end
   fprintf(fidlog,'            Sum : %+10.4f %+10.4f %+10.4f\n', ...
                                                       v(1),v(2),v(3))
end
fprintf(fidlog,'\n')
fclose(fidlog);
%%%%%%%%%%%% end v_loops.m  %%%%%%%%%%%%%%%%
