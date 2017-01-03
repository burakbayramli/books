% PURPOSE: makes HTML verion of contents.m files for the Econometrics Toolbox
% Does a lot of stuff specific to the Econometrics Toolbox and is
% probably of little use to anyone else. 
%
% USAGE: type make_html while in a directory with .m MATLAB functions
% -----------------------------------------------------------------------
% NOTES: This program requires that the first line in the function
% take the form: 
%                % PURPOSE: function purpose statement
% (which is the form taken by Econometrics Toolbox functions)   
% -----------------------------------------------------------------------           
% RETURNS: a file contents.html used on the Econometrics Toolbox web site
% -----------------------------------------------------------------------

% This program was hacked by:
% 
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% from mkcontnt.m 
% 
% Author: Denis Gilbert, Ph.D., physical oceanography
% Maurice Lamontagne Institute, Department of Fisheries and Oceans Canada
% email: gilbertd@dfo-mpo.gc.ca  
% August 1998; Last revision: December 07, 1998 

cdirectory = pwd;
disp(['Creating contents.html file in ' cdirectory])

fid = fopen('contents.html','w');

fprintf(fid,'<TITLE> Spatial econometrics functions library </TITLE>\n');
fprintf(fid,'<BODY> \n');
fprintf(fid,'<A HREF = "../../html/view.html" target="FRP">[Return to Master Index]</A>\n');
fprintf(fid,'</BR> \n');
fprintf(fid,'</BR> \n');

fprintf(fid,'<pre> \n');
fprintf(fid,'-------- first-order spatial autoregressive model functions -------- \n');
fprintf(fid,'</pre> \n');


% this should be done for all cases

files = what;  % Structure with fields files.m, files.mat, etc.
%Note: the field files.m does not include contents.m (IMPORTANT)
%Do not displace this line of code above or below its present location
%to avoid error messages.

if length(files.m)==0
   warning('No m-files found in this directory')
   return
end

blank_line = '%   ';  %Blank line

%Make sure all file names are in lowercase to allow proper alphabetical sorting
files.m = lower(files.m);
files.m = sort(files.m);  %Sort filenames in alphabetical order

% find longest file name
fnlength = 1;
for i=1:length(files.m)
tmp = length(files.m{i});
if tmp > fnlength
fnlength = tmp;
end;
end;

fprintf(fid,'<pre> \n');

%Write H1 lines to contents.html if they exist
for i = 1:length(files.m)
if (strcmp(files.m{i},'contents.m') == 0 & strcmp(files.m{i},'make_html.m') == 0)
   fid2=fopen(files.m{i},'r'); %Cell array of sorted file names
   %Search for first commented line (H1 line)
   count_percent = 0;
   while count_percent < 1 & feof(fid2)==0; 
      %True as long as we do not encounter a line with a "%" sign 
      %or reach the end of file
      line = fgetl(fid2);
      if length(line) > 0 %Allow for possibility that some lines may be empty
         if ~isempty(findstr(line,'%')) %LOOK for percent sign anywhere in the line
            count_percent = count_percent + 1;
            tmp = files.m{i}; flength = length(tmp);
            tmpt = tmp(1:flength-2);
% write HTML stuff
fprintf(fid,'<A HREF = "');
%fmt = ['%',num2str(fnlength+2)];
%fmt = [fmt 's'];

space = [' '];
[junk ilength] = size(tmp);
for k=1:ilength;
fprintf(fid,'%s',tmp(1,k)); % filename with .m extension
end;
for m=ilength+1:fnlength;
fprintf(fid,'%c',space);
end;
fprintf(fid,'">');

[junk ilength] = size(tmpt);
for k=1:ilength;
fprintf(fid,'%s',tmpt(1,k)); % filename without .m extension
end;
for m = ilength+1:fnlength;
fprintf(fid,'%c',space);
end;

fprintf(fid,'</A>');
            llength = length(line);
            fprintf(fid,'%s\n',line(10:llength)); %Write H1 line to contents.html
         end % end of if
      end % end of if
      if feof(fid2)==1  %End of file encountered without finding a single percent sign
         fprintf(fid,'%s\n',blank_line); %Write blank line to contents.html
      end % end of if
end % end of while
   fclose(fid2);
%else % else associated with if strcmp
%   fclose(fid2);
end; % end associated with if strcmp, else

end % end of for
fprintf(fid,'</pre> \n');
fprintf(fid,'</BODY> \n');
fclose(fid);
