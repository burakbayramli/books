function icol = find_eph(Eph,sv,time)
%FIND_EPH  Finds the proper column in ephemeris array

%Kai Borre and C.C. Goad 11-26-96
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 1998/07/01  $

icol = 0;
isat = find(Eph(1,:) == sv);
n = size(isat,2);
if n == 0
   break
end;
icol = isat(1);
dtmin = Eph(21,icol)-time;
for t = isat
   dt = Eph(21,t)-time;
   if dt < 0
      if abs(dt) < abs(dtmin)
         icol = t;
         dtmin = dt;
      end
   end
end
%%%%%%%%%%%%  find_eph.m  %%%%%%%%%%%%%%%%%
