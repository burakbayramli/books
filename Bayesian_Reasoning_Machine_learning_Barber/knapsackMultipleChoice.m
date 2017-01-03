function [optvalue items]=knapsackMultipleChoice(weight,value,N,W)
%KNAPSACKMULTIPLECHOICE Multiple choice binary knapsack
% [optvalue items]=knapsackMultipleChoice(weight,value,N,W)
%
% Inputs:
% weight: vector of non-negative integers. 
% value: vector of non-negative reals.
% N{i}: is a vector describing the members of set (`compartment') i
% W: maximum total weight.
% 
% Outputs:
% optvalue: optimum sum of values
% items: which items are used in forming the optimum solution
%
% The binary multiple choise knapsack problem is to return a list of items
% (one per set) such that the sum(value(items)) is maximal and sum(weight(items))<=W
% The additional constraint is that only one member per set (`comparment')
% is allowed.
%
% example:
% weight=[2 3 2 5 4] % integer weights
% value=[0.7 0.2 0.3 1.2 0.5] % non negative values
% N{1}=[1 2]; N{2}=[3 4 5]; % which members of the knapsack are in the same
% compartment (two compartments in this case)
% W=6 % total weight constraint
% The solution in this case is to take item 1 from compartment 1, and item
% 5 from compartment 2. Their combined weight is 2+4=6 and their combined
% value is 0.7+0.5=1.2

% Use the dynamic programming method
% Dudzinski and Walukiewicz, Journal of Operational Research 28 (1987) 3-21
c=value; a=weight;
I=length(N);
bigminus=-inf;
for y=1:W
    for i=1:I
        tmp=[];
        for j=N{i}
            ii=i-1;
            yy=y-a(j);
            if ii==0
                if yy>=0; viiyy=0;
                else
                    viiyy=bigminus;
                end
            end
            if ii>0
                if yy<=0; viiyy=bigminus;
                else
                    viiyy=v(ii,yy);
                end
            end
            tmp=[tmp c(j)+viiyy];
        end
        v(i,y) = max(tmp);
    end
end
vs=v(end,end);
% backtrack:
items=[];
for i=I:-1:1;
    tmp=[];
    for j=N{i}
        ii=i-1;
        yy=y-a(j);
        if ii==0
            if yy>=0; viiyy=0;
            else
                viiyy=bigminus;
            end
        end
        if ii>0
            if yy<=0; viiyy=bigminus;
            else
                viiyy=v(ii,yy);
            end
        end
        tmp=[tmp (c(j)+viiyy)];
    end
    [val ind]=min(abs(tmp-vs));
    items=[items N{i}(ind)];
    vs=vs-c(N{i}(ind));
    y=y-a((N{i}(ind))) ;
end
optvalue=v(end,end); 