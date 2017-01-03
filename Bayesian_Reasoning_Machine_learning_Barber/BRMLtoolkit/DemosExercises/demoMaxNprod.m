function demoMaxNprod
%DEMOMAXNPROD demo of N-most likely states in a factor graph
variables=1:5; [a b c d e]=assign(variables); % Variable order is arbitary
nstates=ceil(10*rand(1,5)+1); % random number of states for each variable

pot(1).variables=[a b]; pot(1).table=rand(nstates(pot(1).variables));
pot(2).variables=[b c d]; pot(2).table=rand(nstates(pot(2).variables));
pot(3).variables=[c]; pot(3).table=rand(nstates(pot(3).variables),1);
pot(4).variables=[e d]; pot(4).table=rand(nstates(pot(4).variables));
pot(5).variables=[d]; pot(5).table=rand(nstates(pot(5).variables),1);
A = FactorGraph(pot); % variable nodes are first in A
N=size(A,1); nodetype=zeros(1,N);
drawFG(A);
Nmax=3;
[maxstate maxval mess]=maxNprodFG(pot,A,[],Nmax);
maxval
maxstate
% check if this is correct:
disp('Raw maximisation of joint table:');
jointpot = multpots(pot);
[a b]=maxNpot(jointpot,[],Nmax,0);
a.table
vertcat(b{1:Nmax})