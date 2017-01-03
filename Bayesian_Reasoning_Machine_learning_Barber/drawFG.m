function drawFG(A,varargin)
%DRAWFG Draw the Factor Graph 
% drawFG(A,<varinf>)
% If A is takes 0/1 binary elements, the routine assumes that A is a VxF
% matrix with A(v,f)=1 indicating an edge between variable v and factor f
%
% For non-binary A,  A is assumed a (V+F)x(V+F) square  Factor Graph  adjacency 
% matrix where V are the total number of variables and F the number of factors.
% A(1:V,1:V) and A(V+1:end,V+1:end) are empty;
% A(1:V,V+1:end) contains the variable to factor message indices and
% A(V+1:end,1:V) contains the factor to variable message indices
% varinf(1:V).name contains the names of each variable
%
% See also FactorGraph.m
if  sum(A(:)>1)>1 % A has message indices
    N=checkFactorGraph(A); nodetype=zeros(1,N);
    V=min(find(A(1,:)))-1; % variables are first in the order
    for i=1:N;
        if i<V+1;
            if isempty(varargin)
                label{i}=num2str(i); % variable node
            else
                label{i}=varargin{1}(i).name; % variable node
            end
        else label{i}=num2str(i-V); nodetype(i)=1; % factor node
        end
    end; cla; draw_layout(A>0,label,nodetype);
else
    V=size(A,1);N=sum(size(A));AA=zeros(N);
    AA(1:V,V+1:end)=A; AA=AA+AA';
    for i=1:N;
        if i<V+1;
            if isempty(varargin)
                label{i}=num2str(i); % variable node
            else
                label{i}=varargin{1}(i).name; % variable node
            end
        else label{i}=num2str(i-V); nodetype(i)=1; % factor node
        end
    end; cla; draw_layout(AA>0,label,nodetype);    
end