% PURPOSE: Examples of using tsprint()                               
%          to print matrices with variable name labels
%          and row labels for time-series dates
%---------------------------------------------------
% USAGE: tsprint_d
%---------------------------------------------------

data = randn(120,5);
dates = cal(1980,1,12);

vnames =['Illinois     ',
         'Ohio         ',
         'Indiana      ',
         'West Virginia',
         'Pennsylvania '];

fmt = '%14.3f';
begp = ical(1985,1,dates);
endp = ical(1985,12,dates);

fprintf(1,'using all options \n');
tsprint(data,dates,begp,endp,vnames,fmt);

fprintf(1,'using default format option \n');
tsprint(data,dates,begp,endp,vnames);

fmt = '%13d';
data2 = round(data)*100.0;
fprintf(1,'using integer format option \n');
tsprint(data2,dates,begp,endp,vnames,fmt);

tt=1:120;
testdata = [tt' data(:,2:5)];
fmt = '%13d';
fprintf(1,'using d format option with floating point #s \n');
tsprint(testdata,dates,begp,endp,fmt);

data2 = randn(12,10);
fmt = '%16.8f';
fprintf(1,'using format only option \n');
tsprint(data2,dates,fmt);


fmt = '%16.8f';
dates2 = cal(1980,1,4);
fprintf(1,'using format only option \n');
tsprint(data2,dates2,fmt);


fmt = '%16.8f';
dates3 = cal(1980,1,1);
fprintf(1,'using format only option \n');
tsprint(data2,dates3,fmt);
