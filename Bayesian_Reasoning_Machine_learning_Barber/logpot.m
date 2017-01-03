function newpot=logpot(pot)
%LOGPOT logarithm of the potential
for p=1:length(pot)
    newpot(p)=pot(p);
    newpot(p).table=log(replace(pot(p).table,0,10e-10));
end