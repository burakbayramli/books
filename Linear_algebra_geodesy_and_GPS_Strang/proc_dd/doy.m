function  [day_of_year,hr,min,sec] = doy(year,month,day,hour)
%DOY	 Calculation of day number of year.
%	 hour is split into hr, min, and sec

%Kai Borre 10-07-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

day_of_year = julday(year,month,day,0)-julday(year,1,1,0)+1;
hr = floor(hour);
min = floor((hour-hr)*60);
sec = ((hour-hr)-min/60)*3600;
%%%%%%% end doy.m  %%%%%%%%%%%%%
