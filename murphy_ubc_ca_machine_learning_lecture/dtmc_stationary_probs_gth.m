function [pi_vec,amat] = dtmc_stationary_probs_gth(trans_mat)
% Stationary distrribution of a discrete time Markov chain
%
% Using GTH algorithm, calculate the stationary probability row vector 
% of the DTMC (discrete-time Markov chain) specified by the trans_mat.
% According to Grassmann, Taksar, Heyman "Regenerative Analysis
% and Steady State Distributions for Markov Chain", 1985, 
% Operations Research, Vol.33, No.5.1107-1116.
% function [pi_vec,amat] = dtmc_stationary_probs_g1(trans_mat)

% Author: Rui Kang, June,2003, IE Dept. Lehigh University.
% http://www.lehigh.edu/~ruk2/Research/dtmc_stationary_probs_g1.m

amat = trans_mat;
N = size(amat,1);                              %state number
r = zeros(1,N);
S = 0;

% Get the amat(i,j) values after Gaussian Elimination.
% To avoid subtraction, using the property that the sum of every row is zero, then get the diagonal element.
for n = N : -1 : 2
    S = sum(amat(n, 1:n-1));
    amat(1:n-1, n) = amat(1: n-1, n)/S;
    ainvec = amat(1:n-1, n);
    anjvec = amat(n, 1:n-1);
    amat(1:n-1, 1:n-1) = amat(1:n-1, 1:n-1)  + ainvec * anjvec;
end

r(1) = 1;

% Get the stationary probability pi_vec, 1/TOT is p0.
for j = 2:N
   r(j) = amat(1, j)+ r(2: j-1) * amat(2:j-1, j);
end

TOT = sum(r);
pi_vec = r/TOT;
   
return;
