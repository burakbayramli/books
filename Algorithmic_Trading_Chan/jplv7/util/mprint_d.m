% PURPOSE: demo of mprint() 
%          formatted matrix printing 
% 
%---------------------------------------------------
% USAGE: mprint_d
%---------------------------------------------------

% use all defaults
tst = randn(4,3);
fprintf(1,'boring print-out \n');
mprint(tst); 

% demonstrate wrapping at 80/12 = 6 columns
tst = randn(4,10); 
clear in;
in.fmt = '%12.2f';
in.rflag = 1;
fprintf(1,'wrapping demonstration \n');
mprint(tst,in); 

% demonstrate wrapping at 80/20 = 4 columns
tst = randn(4,10); 
clear in;
in.fmt = '%20.2f';
in.rflag = 1;
fprintf(1,'wrapping demonstration \n');
mprint(tst,in); 


% demonstrate decimal format option
tst = round(100*randn(4,3));
clear in;
in.fmt = '%12d';
fprintf(1,'decimal format demo \n');
mprint(tst,in);

% demonstrate row numbers
clear in;
in.rflag = 1;
fprintf(1,'row #s demonstration \n');
mprint(tst,in);

% demonstrate column names and row #s
tst = randn(5,3);
cnames = strvcat('col1','col2','col3');
clear in;
in.cnames = cnames;
in.rflag = 1;
fprintf(1,'column names with row #s \n');
mprint(tst,in);

% turn row #'s off
in.rflag = 0;
fprintf(1,'column names without row #s \n');
mprint(tst,in);

% demonstrate row labels
clear in;
rnames = strvcat('row-header','row1','row2','row3','row4','row5');
in.rnames = rnames;
fprintf(1,'row lobels with no column headings \n');
mprint(tst,in);

% demonstrate row and column labels
clear in;
rnames = strvcat('rows','row1','row2','row3','row4','row5');
in.rnames = rnames;
in.cnames = cnames;
in.fmt = '%16.8f';
fprintf(1,'row lobels and column headings \n');
mprint(tst,in);

% demonstrate multiple formats
fprintf(1,'different formats for each column \n');
tst = randn(5,3);
tst(:,3) = round(tst(:,3));
fmt = strvcat('%12.3f','%12.3f','%6d');
clear in;
in.fmt = fmt;
mprint(tst,in);

% demonstrate multiple formats and colum-row names
fprintf(1,'different formats for each column with cnames,rnames \n');
rnames = strvcat('rows','row1','row2','row3','row4','row5');
cnames = strvcat('column 1','column 2','col3');
tst = randn(5,3);
tst(:,3) = round(tst(:,3));
fmt = strvcat('%12.3f','%12.3f','%7d');
clear in;
in.fmt = fmt;
in.cnames = cnames;
in.rnames = rnames;
mprint(tst,in);

% demonstrate narrow format adjustment
fprintf(1,'narrow format adjustment demo \n');
fprintf(1,'column headings are: ');
cnames = strvcat('a123456789','b123456789','c123456789');
fprintf(1,'%16s %16s %16s \n',cnames(1,:),cnames(2,:),cnames(3,:)); 
rnames = strvcat('variables','row1','row2','row3','row4','row5');
tst = randn(5,3);
fmt = '%6.3f';
fprintf(1,'format is:            ');
fprintf(1,'%16s \n',fmt);
clear in;
in.fmt = fmt;
in.cnames = cnames;
in.rnames = rnames;
mprint(tst,in);
fprintf(1,'which is spread to match wider column headings \n\n');

% demonstrate narrow format adjustment
fprintf(1,'narrow format adjustment demo \n');
fprintf(1,'column headings are: ');
cnames = strvcat('a12345','b12345','c12345');
fprintf(1,'%16s %16s %16s \n',cnames(1,:),cnames(2,:),cnames(3,:)); 
rnames = strvcat('variables','row1','row2','row3','row4','row5');
tst = randn(5,3);
fmt = strvcat('%6.3f','%9.3f','%12.3f');
fprintf(1,'formats are:         ');
fprintf(1,'%16s %16s %16s \n',fmt(1,:),fmt(2,:),fmt(3,:)); 
clear in;
in.fmt = fmt;
in.cnames = cnames;
in.rnames = rnames;
mprint(tst,in);
fprintf(1,'so the 9-wide and 12-wide formats are preserved \n');
fprintf(1,'with the column headings right-justified \n\n');

% demonstrate what happens with a narrow format
fmt = '%5.2f';
fprintf(1,'look what happens with a format = %6s that is narrow \n',fmt);
cnames = strvcat('a123456789','b123456789','c123456789');
rnames = strvcat('variables','row1','row2','row3','row4','row5');
tst = randn(5,3)*100;
clear in;
in.fmt = fmt;
in.cnames = cnames;
in.rnames = rnames;
mprint(tst,in);
fprintf(1,'the #s are spread to accomodate the column headings \n');
fprintf(1,'but the # of decimal points are preserved \n\n');

% demo more variable format issues
fmt = '%2d';
fprintf(1,'suppose we use too narrow of a format like %7s \n',fmt);
cnames = strvcat('a123456789','b123456789','c123456789','d123456789');
rnames = strvcat('variables','row1','row2','row3','row4','row5');
tst = randn(5,4)*10000;
fprintf(1,'with large #s \n');
tst = round(tst);
tst
clear in;
in.fmt = fmt;
in.cnames = cnames;
in.rnames = rnames;
mprint(tst,in);
fprintf(1,'no problem as long as the column headers are wider \n\n');

fmt = '%2d';
fprintf(1,'but suppose we do not have column headers \n');
rnames = strvcat('variables','row1','row2','row3','row4','row5');
tst = randn(5,4)*10000;
fprintf(1,'with large #s and a too-narrow format \n');
tst = round(tst);
tst
clear in;
in.fmt = fmt;
in.rnames = rnames;
mprint(tst,in);

in.fmt = '%8d';
fprintf(1,'now we have a problem \n');
fprintf(1,'that we must fix by adjusting the format \n');
fprintf(1,'to be wider, say %7s \n',in.fmt);
in.rnames = rnames;
mprint(tst,in);
fprintf(1,'which fixes the problem \n\n');

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
mprint(table,in);

fprintf(1,'selected rows and columns with variable formats \n');
clear in;
table = randn(5,10);
in.begc = 5;
in.endc = 10;
in.begr = 2;
in.endr = 5;
fmt1 = strvcat('%10.2f','%10.6f','%10.5f','%12.6f','%16.8f');
fmt2 = strvcat('%20.2f','%10.6f','%10.5f','%12.6f','%16.8f');
in.fmt = strvcat(fmt1,fmt2);
cnames = strvcat('col1','col2','col3','col4');
cnames = strvcat(cnames,'col5','col6','col7','col8','col9','col10');
rnames = strvcat('Row-Labels','row1','row2','row3','row4','row5');
in.cnames = cnames;
in.rnames = rnames;
mprint(table,in);

fprintf(1,'selected rows and columns with variable formats and wrapping \n');
clear in;
table = randn(5,10);
in.begc = 3;
in.endc = 9;
in.begr = 2;
in.endr = 5;
fmt1 = strvcat('%20.2f','%20.6f','%20.5f','%22.6f','%26.8f');
fmt2 = strvcat('%20.2f','%20.6f','%20.5f','%22.6f','%26.8f');
in.fmt = strvcat(fmt1,fmt2);
cnames = strvcat('col1','col2','col3','col4');
cnames = strvcat(cnames,'col5','col6','col7','col8','col9','col10');
rnames = strvcat('Row-Labels','row1','row2','row3','row4','row5');
in.cnames = cnames;
in.rnames = rnames;
mprint(table,in);

