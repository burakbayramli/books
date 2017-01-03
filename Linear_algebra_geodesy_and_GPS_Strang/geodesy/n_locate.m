function iloc = n_locate(i_max,node)
%N_LOCATE Finds the index of node within an i_max by 1 vector of nodes

%Kai Borre 07-31-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26 $

global nodes

for i = 1:i_max
   if (nodes(i) == 0)
      iloc = i;
      nodes(i) = node;
      break;
   end;
   if (nodes(i) == node)
      iloc = i;
      break;
   end;
end;
%%%%%%%%%%%% end n_locate.m  %%%%%%%%%%%%%%%%%%%%%
