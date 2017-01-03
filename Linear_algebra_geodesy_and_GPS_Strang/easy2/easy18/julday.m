function jd = julday(y,m,d,h)
% JULDAY  Conversion of date as given by
%         y ... year (four digits)
%         m ... month
%         d ... day
%         h ... hour and fraction hereof
%         The conversion is only valid in the time span
%         from March, 1, 1900 to February, 28, 2100

%  For further information see
%  Meeus, Jean (1991) Astronomical Algorithms, 
%         Willmann-Bell, Richmond, Virginia, p. 59--62

%  Written by Kai Borre
%  February 14,2001

      if m <= 2, y = y-1; m = m+12; end
      jd = floor(365.25*(y+4716))+floor(30.6001*(m+1))+d+h/24-1537.5;
%      mjd = jd-2400000.5;
%%%%%%% end julday.m  %%%%%%%%%%%%
