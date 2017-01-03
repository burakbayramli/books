function [K,F]=mkmodel(n,varargin)
%MKMODEL Assemble model problem with constraints and load vector.
%   [K,F]=MKMODEL(N,MATERIAL) assembles "the model problem" on the unit
%   square with (N+1)-by-(N+1) node points. The left side is constrained
%   to zero displacement, and on the right side a distributed force [1,-1]
%   is applied.
%
%   Example (solve and plot with 21-by-21 nodes and the default material):
%      [K,F]=mkmodel(20);
%      U=K\F;
%      qdplot(U);
%
%   See also: ELMATRIX, ASSEMBLE, QDPLOT.

%   Per-Olof Persson <persson@math.mit.edu>

K=assemble(n,varargin{:});

xfix=ones(n+1,1);
yfix=(1:n+1)';
fix=xfix+(yfix-1)*(n+1);
fix=[fix;fix+(n+1)^2];
K(fix,:)=0;
K(:,fix)=0;
K(fix,fix)=speye(2*(n+1),2*(n+1));

F=zeros(2*(n+1)^2,1);

xload=(n+1)*ones(n+1,1);
yload=(1:n+1)';
load=xload+(yload-1)*(n+1);

h=1/n;
F(load)=h;
F(load([1,end]))=h/2;
F(load+(n+1)^2)=-F(load);
