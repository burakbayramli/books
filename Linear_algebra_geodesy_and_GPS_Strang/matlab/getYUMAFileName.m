%*******************************************************
% function [filename] = getYUMAFileName(date)
%
% DESCRIPTION: 
%  This function determines the appropriate YUMA file name from the user
%  specified date in the format YUMA<GPSweek>.txt
%  
% ARGUMENTS:
%  date - date struct specified through user input, either file or GUI,
%       which includes Gregorian date and GPS week/second
%  
% OUTPUT:
%  filename - YUMA file name
%  
% CALLED BY:
%  loadSimulationParameters
%
% FUNCTIONS CALLED:
%  julianDate
%
%*******************************************************

function [filename] = getYUMAFileName(dateStructure)

% % Week beginning              GPS Week Number
% % at 0000 GPS Time on         broadcast by satellites 
% % 08 Aug 1999                         1022 
% % 15 Aug 1999                         1023 
% % 22 Aug 1999                          0 
% % 29 Aug 1999                          1 

% Need to add hhmmss as well to conditional
jdRollover = julianDate([1999,8,22,0,0,0]);

jdDate = julianDate([dateStructure.year,dateStructure.month,dateStructure.day,...
    dateStructure.hour,dateStructure.min,dateStructure.sec]);

% root = [ cd '\YUMA'];

slash = slashType(cd);

root = [ cd slash 'YUMA'];

if (jdDate < jdRollover),
%     root = [root '\PreRollover'];
    root = [root slash 'PreRollover'];    
else
%     root = [root '\PostRollover\' num2str(dateStructure.year)];
%     root = [root '\PostRollover' ];   
    root = [root slash 'PostRollover' ];       
end

string = ['YUMA', num2str(dateStructure.GPSwk), '.txt'];
% filename = [root '\' string];
filename = [root slash string];