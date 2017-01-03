% PURPOSE: demo of lprint() 
%          LaTeX table formatted matrix printing 
% 
%---------------------------------------------------
% USAGE: lprint_d
%---------------------------------------------------


table = randn(5,5);

fprintf(1,'using default options \n');
lprint(table);
table2 = round(table)*1000;
in.fmt = '%10d';
fprintf(1,'using fmt option \n');
lprint(table2,in);

vnames =['Illinois     ',
         'Ohio         ',
         'Indiana      ',
         'West Virginia',
         'Pennsylvania '];

fprintf(1,'using vnames and fmt option \n');
fmt = '%8.5f';
in.fmt = fmt;
in.cnames = vnames;
lprint(table,in);

table3 = randn(5,10);

vnames =['Illinois     ',
         'Ohio         ',
         'Indiana      ',
         'West Virginia',
         'Pennsylvania ',
         'Alaska       ',
         'Hawaii       ',
         'New York     ',
         'Mississippi  ',
         'Tennessee    '];

in.fmt = '%16.4f';
in.cnames = vnames;
fprintf(1,'wrapped output \n');
%fid = fopen('table.tex','w');
lprint(table3,in);

fprintf(1,'column and row names with wrapped output \n');
rnames = 'Row Labels';
for i=1:5
rown = ['row',num2str(i)];
rnames = strvcat(rnames,rown);
end;
in.rnames = rnames;
in.fmt = '%16.4f';
in.cnames = vnames;
%fid = fopen('table.tex','w');
lprint(table3,in);

fprintf(1,'demo of variable column formats \n');
table = randn(10,5);
table(:,1:2) = round(table(:,1:2))*100;
rnames = 'Row Labels';
for i=1:10
rown = ['row',num2str(i)];
rnames = strvcat(rnames,rown);
end;
in2.rnames = rnames;
in2.cnames = strvcat('IL','IN','OH','WV','NY');
in2.fmt = strvcat('%5d','%5d','%10.4f','%10.4f','%10.4f');
%fid = fopen('table.tex','w');
lprint(table,in2);


fprintf(1,'demo of printing selected rows and columns \n');
clear in;
table = randn(5,10);
in.begc = 5;
in.endc = 10;
in.begr = 2;
in.endr = 5;
lprint(table,in);

fprintf(1,'demo of printing selected rows and columns \n');
clear in;
table = randn(5,10);
in.begc = 5;
in.endc = 10;
in.begr = 2;
in.endr = 5;
cnames = strvcat('col1','col2','col3','col4');
cnames = strvcat(cnames,'col5','col6','col7','col8','col9','col10');
rnames = strvcat('Row-Labels','row1','row2','row3','row4','row5');
in.cnames = cnames;
in.rnames = rnames;
lprint(table,in);
