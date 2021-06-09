function  d=setup_ID_LM(d);
include_flags;

count = 0; count1 = 0;   
for i = 1:neq
    if flags(i) == 2            % check if node on essential boundary   
        count   = count + 1;    
        ID(i)   = count;        % arrange essential B.C nodes first
        d(count)= e_bc(i);      % store reordered essential B.C 
    else
        count1 = count1 + 1;
        ID(i) = nd + count1;    
    end
end


for e = 1:nel
    n = 1;
    for j = 1:nen
        blk     = ndof*(IEN(j,e)-1);    
        for k = 1:ndof  
            LM(n,e) = ID( blk + k  );   % create the LM matrix
            n = n + 1;
        end
    end
end
