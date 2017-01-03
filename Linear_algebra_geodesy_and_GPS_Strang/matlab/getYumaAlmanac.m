%*******************************************************
% function almanac = getYumaAlmanac(dateStructure)
%
% DESCRIPTION:
%  This function opens the specified YUMA almanac file and
%  the data are read in and stored in the structure array 'almanac'
%  
% ARGUMENTS:
%  dateStructure - a structure containing the Gregorian date vector
%       .year
%       .month
%       .day
%       .hour
%       .min
%       .sec
%  
% OUTPUT:
%  almanac - struct:
%       .prn - PRN number
%       .e - eccentricity
%       .toa - time of applicability
%       .i - inclination
%       .OMEGAo - Right Ascension of Ascending Node (rad)
%       .OMEGAdot - Rate of RAAN (rad/s)
%       .a - semi-major axis
%       .omega - argument of perigee
%       .M - mean anomaly
%  
% CALLED BY:
%  loadSimulationParameters
%
% FUNCTIONS CALLED:
%  None
%
%*******************************************************

function almanac = getYumaAlmanac(dateStructure)


fid = openYumaFile(dateStructure);
if fid < 0,
    error_str = sprintf('YUMA file for this GPS week not found');
    errordlg(error_str, 'Error');
    error(error_str);
end

almanac = readAlmanacFile(fid);
fclose(fid);



% =========================================================================    
function almanac = readAlmanacFile(fid)
    % read first line of the file
    line = fgets(fid);

    j = 0;

    while (line ~= -1)
        lead = line(1);
        switch (lead)
        case 'I'     % PRN number
            temp = sscanf(line,'%*27c%d')';
            j = j+1;
         almanac(j).prn = temp;
        case 'E'     % eccentricity
            temp = sscanf(line,'%*27c%f')';
            almanac(j).e = temp;
        case 'T'       % time of applicability, seconds
            temp = sscanf(line,'%*27c%f')';
            almanac(j).toa = temp;
        case 'O'     % inclination
            temp = sscanf(line,'%*27c%f')';
            almanac(j).i = temp;
        case 'R'     
            if line(2) == 'i'  % Right Ascension of the Ascending Node (RAAN), in radians
                temp2 = sscanf(line,'%*27c%f')';
                almanac(j).OMEGAo = temp2;
            elseif line(2) == 'a'           % Rate of RAAN, radians per second
                temp = sscanf(line,'%*27c%f')';
                almanac(j).OMEGAdot = temp;
            end
        case 'S'     % semimajor axis, meters
            temp = sscanf(line,'%*27c%f')';
            almanac(j).a = temp^2;
        case 'A'     % argument of perigee
            if line(2) == 'r'
                temp = sscanf(line,'%*27c%f')';
                almanac(j).omega = temp;
            end
        case 'M'     % mean anomaly
            temp = sscanf(line,'%*27c%f')';
            almanac(j).M = temp;
        end      % end case/switch
        line = fgets(fid);
    end     % end while loop
    
    
    
% =========================================================================
function fid = openYumaFile(dateStructure)    
    
YUMAfile = [ 'YUMA\' num2str(dateStructure.year) slashType(cd) 'YUMA', num2str(dateStructure.GPSwk), '.txt' ];
fid = fopen(YUMAfile);   


    
