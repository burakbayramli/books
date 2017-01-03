function demoMaxprod
%DEMOMAXPROD demo of maximum product algorithm on a factor graph
variables=1:5; [a b c d e]=assign(variables); % Variable order is arbitary
nstates=ceil(3*rand(1,5)+1); % random number of states for each variable

pot(1).variables=[a b]; pot(1).table=rand(nstates(pot(1).variables));
pot(2).variables=[b c d]; pot(2). table=rand(nstates(pot(2).variables));
pot(3).variables=[c]; pot(3).table=rand(nstates(pot(3).variables),1);
pot(4).variables=[e d]; pot(4).table=rand(nstates(pot(4).variables));
pot(5).variables=[d];  pot(5).table=rand(nstates(pot(5).variables),1);
A = FactorGraph(pot); figure; drawFG(A); % variable nodes are first in A
[maxstate maxval mess]=maxprodFG(pot,A,[]);
disp(['Max prod has optimal state                ',num2str(maxstate),' with value ',num2str(maxval)])

% check if this is correct using raw optimisation:
jointpot = multpots(pot);
[mpot mstate_joint]=maxpot(jointpot,jointpot.variables);
disp(['Brute form optimisation has optimal state ',num2str(mstate_joint),' with value ',num2str(mpot.table)])

% This is using a Junction Tree with Max-absorption:
[jtpot jtsep infostruct]=jtree(pot); % setup the Junction Tree
[jtpot2 jtsep2]=absorption(jtpot,jtsep,infostruct,0); % do full round of absorption
for i=1:length(jtpot2)
[newpot JTmaxstate(jtpot2(i).variables)] = maxpot(jtpot2(i),[],0); % find max over each clique
end
disp(['Max absorb on JT has optimal state        ',num2str(JTmaxstate),' with value ',num2str(newpot.table)])