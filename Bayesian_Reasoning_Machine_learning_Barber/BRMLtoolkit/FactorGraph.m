function A=FactorGraph(pot)
%FACTORGRAPH Returns a Factor Graph adjacency matrix based on potentials
% A = FactorGraph(pot)
%
% pot is a set of potentials and the routine returns the Factor Graph 
% (sparse) adjacency matrix A. The size of A is equal to 
% (V+F)*(V+F) where V are the total number of variables and F the total
% number of factors. A(1:V,1:V) and A(V+1:end,V+1:end) are empty;
% A(1:V,V+1:end) contains the variable to factor message indices and
% A(V+1:end,1:V) contains the factor to variable message indices
% If the set of potentials pot is not singly-connected, all message indices are -1
%
%
% A(i,j)=k means that message number k is from FGnodei -> FGnodej
% Going through the messages in sequence corresponds to a valid
% forward-backward procedure over all variable nodes in the Factor Graph.
% See also FactorConnectingVariable.m, VariableConnectingFactor.m
% Note that the variables in pot must be numbered 1,2,3,...
%
% See also demoSumProd.m, squeezepots.m
F =length(pot); % number of factors (potentials in distribution)
variables=potvariables(pot); % number of variables
if variables(end)~=length(variables); warning('potential variables used are not numbered 1:end. Use squeezepots.'); end
V=length(variables);
N=V+F; % all nodes in factor graph

vnodes=zeros(1,N); vnodes(1:V)=1:V; % variable nodes
fnodes=zeros(1,N); fnodes(V+1:end)=1:F; % factor nodes
A = sparse(N,N);
for f=1:length(pot)
    FGnodeA=find(fnodes==f); FGnodeB=pot(f).variables;
    A(FGnodeA,FGnodeB)=1; A(FGnodeB,FGnodeA)=1;
end

% get a message passing sequence and initialise the messages
[tree elimseq sched]=istree(A);
forwardschedule=sched(1:end-1,:);
reverseschedule=flipud(fliplr(forwardschedule));
schedule=vertcat(forwardschedule,reverseschedule);

if tree
    for count=1:length(schedule)
        % setup the structure for a message from FGnodeA -> FGnodeB
        [FGnodeA FGnodeB]=assign(schedule(count,:));
        A(FGnodeA,FGnodeB)=count; % store the FG adjacency matrix with mess number on edge
    end
else
    A = replace(A,1,-1);
end