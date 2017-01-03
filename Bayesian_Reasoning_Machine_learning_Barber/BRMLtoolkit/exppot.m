function newpot=exppot(pot)
%EXPPOT exponential of a potential
for p=1:length(pot)
    newpot(p)=pot(p);
    newpot(p).table=exp(pot(p).table);
end