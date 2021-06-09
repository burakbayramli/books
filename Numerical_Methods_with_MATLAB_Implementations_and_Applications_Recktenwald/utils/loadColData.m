function [x,Y,labout] = loadColData(fname,ncol,nhead,nrowl)
% loadColData  Import a file containing header text, column titles and data
%
% Synopsis:  [x,Y] = loadColData(fname)
%            [x,Y] = loadColData(fname,ncol)
%            [x,Y] = loadColData(fname,ncol,nhead)
%            [x,Y] = loadColData(fname,ncol,nhead,nrowl)
%            [x,Y,labels] = loadColData(fname)
%            [x,Y,labels] = loadColData(fname,ncol)
%            [x,Y,labels] = loadColData(fname,ncol,nhead)
%            [x,Y,labels] = loadColData(fname,ncol,nhead,nrowl)
%
%     loadColData can read plain text files with the following format
%
%           header line 1 ...
%           header line 2 ...
%           ...
%           col_1_label    col_2_label    col_3_label  ...
%           col_1_label2   col_2_label2   col_3_label2 ...
%           ...
%           number         number         number ...
%           number         number         number ...
%           number         number         number ...
%           ...
%
%     where ... indicates any number of similar lines or columns
%
% Input:   fname = (string) name of the file containing the data (required)
%          ncol  = total number of columns of data.  Default: ncol = 2
%          nhead = number of lines of header information at the very top of
%                  the file.  Header text is read and discarded.  Default = 0.
%          nrowl = number of rows of labels.  Default: nrowl = 1
%
% Output:  x = vector of values from the first column of data
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

%  --- Read in the x-y data
data = fscanf(fin,'%f');        %  Load values into a column vector
fclose(fin);                    %  Close file, release handle
nd = length(data);              %  Total number of data points
nr = nd/ncol;                   %  Number of data rows after reshaping
if nr ~= round(nd/ncol)
   fprintf('Error in loadColData:\n');
   fprintf('\tnumber of data points = %d does not equal nrow*ncol\n',nd);
   fprintf('\tdata: nrow = %f\tncol = %d\n',nr,ncol);
   fprintf('\nHere are the column labels\n\t');
   for j=1:ncol,  fprintf('%s  ',labels(j,:));  end,  fprintf('\n');
   error(sprintf('data matrix cannot be reshaped into %d columns',ncol))
end

data = reshape(data,ncol,nr)';   %  Notice the transpose operator
x = data(:,1);
Y = data(:,2:ncol);
if nargout>2,  labout = labels;  end
