function x=potsample(pot,nsamples)
%POTSAMPLE Draw exact samples from a Markov network
% x=potsample(pot,nsamples)
% Uses Junction Tree to perform exact sampling
if length(pot)==1
    [variables nstates]=potvariables(pot);
    if isempty(nstates); x=[]; return; end
    tab = pot.table(:);
    x = zeros(length(variables),nsamples);
    for samp=1:nsamples
        randindex = randgen(tab);
        x(:,samp) = ind2subv(nstates,randindex);
    end
else
    [jtpot jtsep infostruct]=jtree(pot); % setup the Junction Tree
    jtpot=absorption(jtpot,jtsep,infostruct); % do full round of absorption
    x=JTsample(jtpot,infostruct,nsamples); % sample from the Junction Tree
end