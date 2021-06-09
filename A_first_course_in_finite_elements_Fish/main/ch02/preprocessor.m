function  [K,f,d] = preprocessor;
include_flags;

% input file to include all variables 
  input_file_example2_2;
%  input_file_example2_8;

% Generate LM array 

for e = 1:nel
    for j = 1:nen
        for m = 1:ndof
            ind = (j-1)*ndof + m;
            LM(ind,e) = ndof*IEN(j,e) - ndof + m;
        end
    end
end
