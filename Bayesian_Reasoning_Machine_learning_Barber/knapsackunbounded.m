function [optvalue items]=knapsackunbounded(weight,value,W)
% KNAPSACKUNBOUNDED: unbounded knapsack problem
% [optm items]=knapsackunbounded(weight,value,W)
%
% The problem is to return a list of items that should be in the sack, possibly multiple
% times such that sum(value(items)) is maximal and sum(weight(items))<=W
%
% Inputs:
% weight: vector of positive integers.
% values: vector of positive reals.
% W is the maximum total weight.
%
% Outputs:
% optvalue: optimum sum of the values
% items: list of items to be included in the knapsack
% 
% example:
% weight=[4 1 2 7 4] % integer weights
% value=[0.7 0.1 0.3 1.2 0.5] % non negative values
% W=9 % total weight constraint
% In this case the optimal solution is to take item 1 twice and item 2
% once. This has combined values 2*0.7+0.1=1.5 and combined weight 2*4+1=9

% Use the classical dynamic programming method:
N=length(weight);
for wc=1:W
    tmp=zeros(1,N);
    for i=1:N
        if weight(i)<wc
            tmp(i)=value(i)+m(wc-weight(i));
        end
        if weight(i)==wc
            tmp(i)=value(i);
        end
    end
    if wc>1
        m(wc)=max([m(wc-1) max(tmp)]);
    else
        m(wc)=max(tmp);
    end
end
optvalue=m(end);
% backtrack by finding which item was added to make the last value:
items=[]; Wnew=W;
while Wnew>0
    tmp=zeros(1,N);
    for i=1:N
        if Wnew-weight(i)>0
            tmp(i)=m(Wnew-weight(i));
        end
    end
    items=[items find(tmp+value==m(Wnew),1)];
    Wnew=Wnew-weight(items(end));
end