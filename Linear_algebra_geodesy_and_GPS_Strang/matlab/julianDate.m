%*******************************************************
% function jd = julianDate(Y,M,D,H,min,sec)
%
% DESCRIPTION:
%  Determines the Julian date based on the Gregorian date (Y,M,D,h,m,s)
%  
% ARGUMENTS:
%  dateVec - Gregorian date vector ([Y M D h m s])
%       *can be produced for current time with clock command
%  
% OUTPUT:
%  jd - Julian date
%  
% CALLED BY:
%  Various
%
% FUNCTIONS CALLED:
%  None
%*******************************************************
function jd = julianDate(dateVec)

Y = dateVec(1);
M = dateVec(2);
D = dateVec(3);
H = dateVec(4);
min = dateVec(5);
sec = dateVec(6);

format long g;
UT = H+(min/60)+(sec/3600);
if M > 2
    y=Y;
    m=M;
else
    y=Y-1;
    m=M+12;
end;
jd = fix(365.25*y) + fix(30.6001*(m+1)) + D + (UT/24) + 1720981.5;