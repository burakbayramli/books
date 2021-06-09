function  [K,f,d] = preprocessor;
include_flags;


% read input file 
input_file_1ele;
%input_file_16ele;
%input_file_64ele;


% generate ID array and LM arrays 
d = setup_ID_LM(d);


