% preprocessing read input data and sets up mesh information
function  [K,f,d] = preprocessor;
include_flags;

% input file to include all variables 
input_file5_2_1ele;
%input_file5_2_2ele;
%
% generate LM and ID arrays 
d = setup_ID_LM(d);
