function jd = julday(y,m,d,h)
% JULDAY  Conversion of date as given by
%         y ... year (four digits)
%         m ... month
%         d ... day
%         h ... hour and fraction hereof
%         The conversion is only valid in the time span
%         from March 1900 to February 2100

% See  Hofmann-Wellenhof et al., p. 41--42

%Kai Borre 05-18-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26 $

if m <= 2
   y = y-1; 
   m = m+12;
end
jd = floor(365.25*y)+floor(30.6001*(m+1))+d+h/24+1720981.5;
% mjd = jd-2400000.5;
%%%%%%% end julday.m  %%%%%%%%%%%%
