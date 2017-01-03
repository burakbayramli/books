function A=markov(pot)
%MARKOV Return a symmetric adjacency matrix of Markov Network in pot
% A=markov(pot)
pot=uniquepots(pot);
N=length(potvariables(pot)); A = zeros(N,N);
for i=1:length(pot)
	family=pot(i).variables; A(family,family)=1; % put all family in a cliquo (moralises if a BN)
end
A=A-diag(diag(A));