% PURPOSE: makes pretty contents.m files for the Econometrics Toolbox
% Copies the H1 line (first comment line) of all m-files found
% in the current working directory to a file named "contents.m".
% If such a file already exists, a backup copy of it is made to
% "contents.old". 
%
% USAGE: type make_contents while in a directory with .m MATLAB functions
% -----------------------------------------------------------------------
% NOTES: This program requires that the first line in the function
% take the form: 
%                % PURPOSE: function purpose statement
% (which is the form taken by Econometrics Toolbox functions)   
%           
% It is important to note that any fancy editing done to a previous 
% version of contents.m will be lost. Only the top two lines from the
% old version are copied to the new version, but that number can easily
% be increased by minor modifications to the code. Use the top few
% lines of your contents.m files to describe in general terms what kinds 
% of tasks are performed by your m-files. 
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

disp(['Creating contents.m in ' pwd])
%Check if a contents.m file already exists in the current directory
if exist([pwd filesep 'contents.m'])==0 % Contents.m does not exist in pwd
   line1 = '%Write text describing the m-files in this directory';
   line2 = '%Write text describing the m-files in this directory (continued)';
else  %Open current version of contents.m and save its first two lines
   fid=fopen('contents.m','r');
   line1=fgetl(fid);   line2=fgetl(fid);
   fclose(fid);
   %Make backup copy before deleting contents.m
   copyfile('contents.m','contents.old');
   delete contents.m  %Delete current version of contents.m
end

files = what;  % Structure with fields files.m, files.mat, etc.
%Note: the field files.m does not include contents.m (IMPORTANT)
%Do not displace this line of code above or below its present location
%to avoid error messages.

if length(files.m)==0
   warning('No m-files found in this directory')
   return
end

blank_line = '%   ';  %Blank line
fcontents = fopen('contents.m','w'); %Write a new version of contents.m
fprintf(fcontents,'%s\n',line1);     %Copy descriptive header text from previous version
fprintf(fcontents,'%s\n',line2);     %Copy descriptive header text (continued)
fprintf(fcontents,'%s\n',blank_line);%Third line is blank

%Make sure all file names are in lowercase to allow proper alphabetical sorting
files.m = lower(files.m);
files.m = sort(files.m);  %Sort filenames in alphabetical order

%Write H1 lines to contents.m if they exist
for i = 1:length(files.m)
   fid=fopen(files.m{i},'r'); %Cell array of sorted file names
   %Search for first commented line (H1 line)
   count_percent = 0;
   while count_percent < 1 & feof(fid)==0; 
      %True as long as we do not encounter a line with a "%" sign 
      %or reach the end of file
      line = fgetl(fid);
      if length(line) > 0 %Allow for possibility that some lines may be empty
         if ~isempty(findstr(line,'%')) %LOOK for percent sign anywhere in the line
            count_percent = count_percent + 1;
            tmp = files.m{i}; flength = length(tmp);
            tmpt = tmp(1:flength-2);
            fprintf(fcontents,'%s ','%');
            fprintf(fcontents,'%16s ',tmpt);
            llength = length(line);
            fprintf(fcontents,'%s\n',line(10:llength)); %Write H1 line to contents.m
         end
      end
      if feof(fid)==1  %End of file encountered without finding a single percent sign
         fprintf(fcontents,'%s\n',blank_line); %Write blank line to contents.m
      end
   end
   fclose(fid);
end

fclose(fcontents);
