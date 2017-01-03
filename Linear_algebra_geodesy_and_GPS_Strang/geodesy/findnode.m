function iloc = findnode(i_max, n_value)
%FINDNODE For a given n_value we find the component number
%  	    iloc for the node in the vector (of unknowns)

%Kai Borre 04-16-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

global nodes
for i = 1:i_max
   if (nodes(i) == 0)
      iloc = i;
      nodes(i) = n_value;
      break;
   end;
   if (nodes(i) == n_value)
      iloc = i;
      break;
   end;
end;
%%%%%%%%%%%% end findnode.m  %%%%%%%%%%%%%%%%%%%%%
