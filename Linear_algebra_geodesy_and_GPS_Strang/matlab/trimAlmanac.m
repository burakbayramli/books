%*******************************************************
% function [PRN, almanac] = trimAlmanac(PRN, almanac)
%
% DESCRIPTION:
%  This function trims the almanac data to just contain information
%  pertinent to the specifed satellites.
%  
% ARGUMENTS:
%  PRN - a list of specified PRNs
%  almanac - untrimmed almanac data set
%  
% OUTPUT:
%  PRN - list of PRN numbers (this is returned because it could possibly be
%       modified by changing a 0 to the full list of all available PRNs)
%  almanac - trimmed almanac data set
%  
% CALLED BY:
%  loadSimulationParameters
%
% FUNCTIONS CALLED:
%  None
%
%*******************************************************
function [PRN, almanac] = trimAlmanac(PRN, almanac)

% A '0' in the variable 'PRN' indicates that all PRNs in the almanac file
% are to be calculated
if (PRN == 0)
    for i = 1:length(almanac)
        PRN(i) = almanac(i).prn;
    end
else
    alm_trim = [ ];
    PRN_trim = [ ];
    for i = 1:length(almanac)
        idx = find(almanac(i).prn == PRN);
        if (idx ~= 0)
            alm_trim = cat(2, alm_trim, almanac(i));
            PRN_trim = cat(2, PRN_trim, almanac(i).prn);
        end
    end
    almanac = alm_trim;
    PRN = PRN_trim;
end