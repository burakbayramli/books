function [marg mess lognormconst messlogprefactor]=sumprodFG(pot,A,varargin)
%SUMPRODFG Sum-Product algorithm on a Factor Graph represented by A
% [marg mess lognormconst messlogprefactor]=sumprodFG(pot,A,<mess>,<donorm>)
%
% Inputs:
% pot : Factor Graph potentials
% A : matrix representing the Factor Graph (see FactorGraph.m)
% mess are the initial messages. If mess=[], the standard initialisation  is used.
% donorm=1 : normalises each message table to sum to 1. Default is no normalisation
%
% Outputs:
% marg : the single variable marginals
% mess: the FG messages
% lognormconst = log(sum_{all variables} prod_{all factors} pot)
% messlogprefactor: message log prefactors resulting from normalisation.
% Hence the true messages are given by mess.table*exp(messlogprefactor)
%
% Note that the log prefactors can be useful in large graphs to avoid over/underflow.
%
% see demoSumprod.m, FactorConnectingVariable.m
[variables nstates dum dum varinf]=potvariables(pot);
V=length(variables); N=size(A,1);
fnodes=zeros(1,N); fnodes(V+1:end)=1:N-V; % factor nodes
vnodes=zeros(1,N); vnodes(1:V)=1:V; % variable nodes
nmess=full(max(max(A))); % number of messages
if nargin==2; initmess=[]; else initmess=varargin{1};end
if nargin==4; donorm=varargin{2}; else donorm=0; end
if ~isempty(initmess); mess=initmess; end
if isempty(initmess) % message initialisation
    for count=1:nmess
        mess(count)=struct('variables',[],'table',[]);
        [FGnodeA FGnodeB]=find(A==count);
        if fnodes(FGnodeA)>0 % factor to variable message:
            mess(count).variables=vnodes(FGnodeB); % variable of this message
            % if the factor is at the edge (simplical), need to set message to the factor potential
            if length(find(A(FGnodeA,:)))==1
                mess(count).table=pot(fnodes(FGnodeA)).table;
            else % not a simplical node:
                mess(count).table=myones(nstates(vnodes(FGnodeB)),varinf.pottype);
            end
        else % variable to factor message:
            mess(count).variables=vnodes(FGnodeA); % variable of this message
            mess(count).table=myones(nstates(vnodes(FGnodeA)),varinf.pottype);
        end
    end
end
messlogprefactor=zeros(1,nmess);

% Do the message passing:
for count=1:length(mess)
    [FGnodeA FGnodeB]=find(A==count);
    FGparents=setdiff(find(A(FGnodeA,:)),FGnodeB); % FG parent nodes of FGnodeA
    if ~isempty(FGparents)
        tmpmess = multpots(mess(A(FGparents,FGnodeA)));
        if donorm; logprefactor=sum(messlogprefactor(A(FGparents,FGnodeA))); end
        factor=fnodes(FGnodeA);
        if factor==0; % variable to factor message
            mess(count).table=tmpmess.table;
        else
            tmpmess = multpots([tmpmess pot(factor)]);
            mess(count) = sumpot(tmpmess,setdiff(tmpmess.variables,mess(count).variables));
        end
        if donorm && ~potistyped(mess(count)); % normalise the table to avoid over/under flow
            prefactor=sum(mess(count).table(:));
            messlogprefactor(count)=log(prefactor)+logprefactor;
            mess(count).table=(mess(count).table)./prefactor;
        end
    end
end

% Get all the marginals: variable nodes are first in the ordering, so
for i=1:V
    [dum1 dum2 incoming]=find(A(:,i));
    tmpmess = multpots(mess(incoming));% marg(i)=tmpmess;
    marg(i)=normpot(tmpmess);
end
if ~potistyped(mess(count));
    if donorm;lognormconst=log(sum(table(multpots(mess(mess2var(1,A))))))+sum(messlogprefactor(mess2var(1,A)));
    else lognormconst=log(table(sumpot(tmpmess))); end
else
    lognormconst=[];
end
