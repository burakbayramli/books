% merge_process.m
clear all,clc
% load a matrix with edges, for example:
E=[ 1      2
    1      3
    1      4
    2      5
    2      6
    3      7
    4      7
    5      8
    6      8
    7      9
    7     10
    7     11
    8      9
    9     13
    10    14
    11    12
    12    14
    13    14];
n=max(E(:));    % number of nodes
m=size(E,1);    % number of edges
p=ones(m,1)*.999; % reliability of edges
lam=-log(1-p'); % repair time rates
N=10^3; % sample size
ell=nan(N,1);
for i=1:N
    X=-log(rand(1,m))./lam; % generate repair times
    [x_sorted,perm]=sort(X); % find permutation pi
    % compute rates for convolution using merge process
    LAM_perm=merge(E(perm,:),lam(perm));
    % compute probability given merge process configuration
    ell(i)=convolution(1,LAM_perm);
end
estimate=mean(ell)
percent_rel_error=std(ell)/sqrt(N)/mean(ell)*100