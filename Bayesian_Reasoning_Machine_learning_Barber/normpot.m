function pot=normpot(pot)
%NORMPOT normalise a potential
%pot=normpot(pot)
for p=1:length(pot)
    if potistyped(pot(p))
        pot(p)=feval(['normpot' pot(p).table.type],pot(p));
    else
        pot(p).table=pot(p).table+eps; pot(p).table=pot(p).table./sum(pot(p).table(:));
    end
end