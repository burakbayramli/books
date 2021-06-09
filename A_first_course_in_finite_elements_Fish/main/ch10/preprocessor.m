function  [K,f,d] = preprocessor;
include_flags;

% input file to include all variables 
  input_file_example10_1;

% Generate LM array 
count = 0; count1 = 0;
for i = 1:neq
   if flags(i) == 2                % check if essential boundary
     count = count + 1;
     ID(i) = count;                % number first the degrees-of-freedom on essential boundary
      d(count)= e_bc(i);           % store the reordered values of essential B.C
   else
     count1 = count1 + 1;
     ID(i) = nd + count1;
   end
end
for e = 1:nel
  for j = 1:nen
    for m = 1:ndof
            ind = (j-1)*ndof + m;
            LM(ind,e) = ID(ndof*IEN(j,e) - ndof + m) ;% create the LM matrix
    end

  end
end
