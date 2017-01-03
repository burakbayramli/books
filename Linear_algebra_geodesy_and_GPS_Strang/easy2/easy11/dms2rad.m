function result = dms2rad(deg,min,sec);
% Conversion of degrees, minutes, and seconds to radians

%Kai Borre
%Copyright (c) by Kai Borre
%$Revision 1.1 $  $Date:1999/01/12  $

neg_arg = 'FALSE';
if deg < 0
   neg_arg = 'TRUE ';
   deg = -deg;
end
arg = deg+min/60+sec/3600;
result = arg*pi/180;
if neg_arg == 'TRUE ';
   result = -result;
end
%%%%%%%%%%%%%%%%% end dms2rad.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%
