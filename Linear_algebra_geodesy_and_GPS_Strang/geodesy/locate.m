function iloc = locate(iprn_value)
%LOCATE  For a given iprn_value we find the component
%        number iloc for the satellite in the vector
%        of unknowns

%Kai Borre 09-20-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26 $

global sat_index

for i = 1:12
   if (sat_index(i) == 0)
      iloc = i;
      sat_index(i) = iprn_value;
      break;
   end;
   if (sat_index(i) == iprn_value)
      iloc = i;
      break;
   end;
end;
iloc = iloc+3;   % we add 3 for the coordinate unknowns
%%%%%%%%%%%% end locate.m  %%%%%%%%%%%%%%%%%%%%%
