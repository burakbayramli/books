function  d=setup_ID_LM(d);
include_flags;

count = 0; count1 = 0;   
for i = 1:neq
    if flags(i) == 2            % check if essential B.C   
        count   = count + 1;    
        ID(i)   = count;        % arrange essential B.C nodes first
        d(count)= e_bc(i);      % store reordered essential B.C 
    else
        count1 = count1 + 1;
        ID(i) = nd + count1;    
    end
end


for i = 1:nel
    for j = 1:nen
        LM(j,i)=ID(IEN(j,i));   % create the LM matrix 
    end
end
