function c = condindepPot(pot,X,Y,Z)
%CONDINDEPPOT Numerical conditional independence measure
% c = condindepPot(pot,X,Y,Z)
% Returns the mean absolute deviatian between p(X|Z)p(Y|Z)p(Z) and p(X,Y,Z)
jointpot=sumpot(multpots(pot),[X Y Z],0);
pXgZ = condpot(jointpot,X,Z);
pYgZ = condpot(jointpot,Y,Z);
pZ=condpot(jointpot,Z);
pXgZpYgZpZ = multpots([pXgZ pYgZ pZ]); % p(X|Z)p(Y|Z)p(Z)
c = mean(abs(pXgZpYgZpZ.table(:)-jointpot.table(:)));