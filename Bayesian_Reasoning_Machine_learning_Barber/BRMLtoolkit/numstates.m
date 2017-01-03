function nstates=numstates(pot)
%NUMSTATES Number of states of the variables in a potential
% nstates=numstates(pot)
for i=1:length(pot)
    if isfield(pot(i),'table')
        if potistyped(pot(i))
            nstates{i}=feval(['numstates' pot(i).table.type],pot(i));
        else
            s=size(pot(i).table);
            if length(s)==2 && sum(s==1)==1
                nstates{i}=max(s);
            elseif  prod(s)<=1
                nstates{i}=prod(s);
            else
                nstates{i}=s;
            end
        end
    else
        warning(['I cannot determine the number of states of potential ' num2str(i) ' since it does not contain a table'])
    end
end
if i==1; nstates=cell2mat(nstates); end