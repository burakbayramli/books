function [dout,Yout,labout] = loadColDateData(fname,ncol,nhead,nrowl)
% loadColDateData  Import header text, column titles, date and numeric data
%
% Synopsis:
%   [d,Y] = loadColDateData(fname)
%   [d,Y] = loadColDateData(fname,ncol)
%   [d,Y] = loadColDateData(fname,ncol,nhead)
%   [d,Y] = loadColDateData(fname,ncol,nhead,nrowl,dateform)
%   [d,Y] = loadColDateData(fname,ncol,nhead,nrowl,dateform,pivotyear)
%   [d,Y,labels] = loadColDateData(fname)
%   [d,Y,labels] = loadColDateData(fname,ncol)
%   [d,Y,labels] = loadColDateData(fname,ncol,nhead)
%   [d,Y,labels] = loadColDateData(fname,ncol,nhead,nrowl)
%   [d,Y,labels] = loadColDateData(fname,ncol,nhead,nrowl,dateform)
%   [d,Y,labels] = loadColDateData(fname,ncol,nhead,nrowl,dateform,pivotyear)
%
%  loadColDateData reads plain text files with the following format
%
%           header line 1 ...
%           header line 2 ...
%           ...
%           col_1_label    col_2_label    col_3_label  ...
%           col_1_label2   col_2_label2   col_3_label2 ...
%           ...
%           date         number         number ...
%           date         number         number ...
%           date         number         number ...
%           ...
%
%     where ... indicates any number of similar lines or columns
%
% Input:   fname = (string) name of the file containing the data (required)
%          ncol  = (optional) total number of columns of data.  Default: ncol = 2
%          nhead = (optional) number of lines of header information at top of
%                  the file.  Header text is read and discarded.  Default = 0.
%          nrowl = (optional) number of rows of labels.  Default: nrowl = 1
%          dateform = (optional) number indicating format of date string in column 1.
%                     See "help datestr".  Default: dateform = 2
%          pivotyear = (optional) year denoting beginning of 100 year range in
%                      which two year dates are assumed to reside.  Example:
%                      pivotyear = 1980 ==> dates of form yy are assumed to begin
%                      in 1980 and end in 2080.  Default:  current year minus 50
%                      See "help datestr"
%
% Output:  d = vector of dates in MATLAB datenum format, d is converted from the
%              first column of data using dateform and pivotyear variables, above
%          Y = matrix of values from the second through ncol column of data
%          labels = (string) matrix of labels.  To provide for labels of
%                   arbitrary length, THE LABELS FOR EACH COLUMN OF DATA
%                   ARE STORED IN SEPARATE ROWS OF THE labels MATRIX.
%                   Thus, label(1,:) is the label for the first column,
%                   label(2,:) is the label for the second column, etc.
%                   More than one row of labels is allowed.  In this case
%                   the second row of the label for column one is
%                   label(1+ncol,:).  NOTE:  Individual column headings
%                   must not contain blanks.

%  Gerald Recktenwald, gerry@me.pdx.edu
%  Portland State University, Mechanical Engineering Department
%  24 August 1995, revised 29 April 1998, 27 Feb 1999

if nargin<2,  ncol = 2;   end
if nargin<3,  nhead = 0;  end
if nargin<4,  nrowl = 1;  end

% --- Open file for input, include error handling
fin = fopen(fname,'rt');   %  read as plain text
if fin < 0
   error(['Could not open ',fname,' for input']);
end

% --- Read and discard header text one line at a time
for i=1:nhead,  buffer = fgetl(fin);  end

% --- Read column titles
labels = '';                            %  Initialize the labels matrix
for i=1:nrowl
   buffer = fgetl(fin);                 %  Get next line as a string
   for j=1:ncol
      [next,buffer] = strtok(buffer);   %  Parse next column label
      labels = str2mat(labels,next);    %  Add another row to the labels matrix
   end
end
if nrowl>0
 labels(1,:) = [];  %  delete first row created when labels matrix initialized
end

%  --- Read in the rest of the file one line at at time
%      This is slow, a good candidate for MEX, or perl

i = 0;
while 1
  buffer = fgetl(fin);
  if ~isstr(buffer),  break;  end   %  EOF reached, or other problem
  i = i + 1;
  [next,buffer] = strtok(buffer);   %  Parse date string at beginning of line
  d(i) = datenum(next);
  for j=2:ncol
      [next,buffer] = strtok(buffer); %  Parse next column value
      Y(i,j-1) = str2num(next);       %  Add numerical value to Y matrix
  end
  
end
% --- fall through at end of preceding loop
if buffer~=-1
  fprintf('\n\n\tAbnormal termination while reading Error while reading %s\',fname);
  fprintf('\t\t%d good lines of data read\n',ngood);
end

dout = d(:);   %  reshape d as column vector
Yout = Y;      %  copy to contiguous memory locations

if nargout>2,  labout = labels;  end
