function result = r2dms(arg)
%R2DMS  Conversion of radians to degrees, minutes, and seconds

%Kai Borre 12-23-95
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/10/15  $

arg = arg*180/pi;
result = zeros(1,3);
result(1) = fix(arg);
result(2) = fix(rem(arg,result(1))*60);
result(3) = (arg-result(1)-result(2)/60)*3600;
%fprintf('\n Result =%3.0f %3.0f %10.6f\n',result(1),result(2),result(3))
%%%%%%%%%%%%%%%%% end r2dms.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%