%%  
%%  Grewal, Andrews, and Bartone,
%%  Global Navigation Satellite Systems, Inertial Navigation Systems, and Integration
%%  John Wiley & Sons, 2012.
%%  
%
% This m-file converts downloaded GPS ephemerides to four data sets:
%
%    PRN = PRNs of satellites used
%    OI  = orbital inclination [rad]
%    RA  = right ascension [rad]
%    PA  = argument of perigee + mean anomaly [rad]
%       = in-orbit angle from crossing of the equator south-to-north
%
% The saved files 'OI.dat' and 'RA.dat' are used for simulating GNSS navigation
% They can be loaded into working memory by using the MATLAB 'load' command
% and made into global variables for handing off to subroutines and functions.
% 
% Users can download more recent emphemerides, and add a line beginning with
% "DEFINITION" to the end, to make them compatible with this file-reading routine.
% See trailer of downloaded file 'YUMAdata.txt' for further details
% 
clear all;
close all;
fid = fopen('YUMAdata.txt','r');
k = 0;
while 1
    tline = fgetl(fid);
    %if ~ischar(tline), break, end;
    [rows,ll] = size(tline);
    if size(tline) ~= [0 0], 
        if tline(1:length('ID:                         ')) == 'ID:                         ',
            k = k + 1;
            PRN(k) = str2num(tline(length('ID:                         ')+1:ll));
        elseif (tline(1:length('Right Ascen at Week(rad):  '))=='Right Ascen at Week(rad):  '),
            RA(k) = str2num(tline(length('Right Ascen at Week(rad):  ')+1:ll));
        elseif (tline(1:length('Argument of Perigee(rad):  '))=='Argument of Perigee(rad):  '),
            PA(k) = str2num(tline(length('Argument of Perigee(rad):  ')+1:ll));
        elseif (tline(1:length('Mean Anom(rad):            '))=='Mean Anom(rad):            '),
            PA(k) = PA(k) + str2num(tline(length('Mean Anom(rad):            ')+1:ll));
        elseif (tline(1:length('Orbital Inclination(rad):  ')) == 'Orbital Inclination(rad):  '),
            OI(k) = str2num(tline(28:ll));
        elseif tline(1:length('DEFINITION')) == 'DEFINITION',
            break;           
        end;
    end;
end;
fclose(fid);
save('OI','OI');
save('PRNo','PRN');
save('RA','RA');
save('PA','PA');