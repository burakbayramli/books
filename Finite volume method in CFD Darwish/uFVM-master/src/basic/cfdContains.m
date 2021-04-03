function state = cfdContains(string, sub_string)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function checks if the sub_string is contained instring
%--------------------------------------------------------------------------

if isempty(strfind(string, sub_string))
    state = false;
else
    state = true;
end