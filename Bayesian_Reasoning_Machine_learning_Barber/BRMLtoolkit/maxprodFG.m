function [maxstate maxval mess logmaxval messlogprefactor]=maxprodFG(pot,A,varargin)
% maxprodFG Max-Product algorithm on a Factor Graph
% [maxstate maxval mess logmaxval logprefactor]=maxprodFG(pot,A,<mess>,<donorm>)
%
% Inputs:
% pot : a set of potentials
% A : a Factor Graph adjacency matrix with message numbers on the non-zero elements
% mess: the message initialisation. If mess=[] the standard initialisation is used.
% donorm=1 : normalises each message table to have maximum value 1. Default is no normalisation
%
% Outputs:
% maxstate : maximum joint state
% maxval : associated (unnormalised) maximum value
% mess : Max Product messages
% logmaxval : log maximum value
% messlogprefactor: log normalisation factors for each message
[variables nstates]=potvariables(pot);
V=length(variables); N=size(A,1);
fnodes=zeros(1,N); fnodes(V+1:end)=1:N-V; % factor nodes
vnodes=zeros(1,N); vnodes(1:V)=1:V; % variable nodes
nmess=full(max(max(A))); % number of messages

if nargin==2; mess=[]; else mess=varargin{1};end
if nargin==4; donorm=varargin{2}; else donorm=0; end
if isempty(mess) % message initialisation:
    for count=1:nmess
        [FGnodeA FGnodeB]=find(A==count);
        if fnodes(FGnodeA)>0 % factor to variable message:
            mess(count).variables=vnodes(FGnodeB); % variable of this message
            % if the factor is at the edge (simplical), need to set message to the factor potential
            if length(find(A(FGnodeA,:)))==1
                mess(count).table=pot(fnodes(FGnodeA)).table;
            else % not a simplical node:
                mess(count).table=myones(nstates(vnodes(FGnodeB)));
            end
        else % variable to factor message:
            mess(count).variables=vnodes(FGnodeA); % variable of this message
            mess(count).table=myones(nstates(vnodes(FGnodeA)));
        end
    end
end
messlogprefactor=zeros(1,nmess);


% now do the message passing:
for count=1:nmess
    [FGnodeA FGnodeB]=find(A==count);
    FGparents=setdiff(find(A(FGnodeA,:)),FGnodeB); % FGparent nodes of FGnodeA
    if ~isempty(FGparents)
        factor=fnodes(FGnodeA);
        tmpmess = multpots(mess(A(FGparents,FGnodeA)));
        if donorm; logprefactor=sum(messlogprefactor(A(FGparents,FGnodeA))); end
        if factor==0; % variable to factor message
            mess(count).table=tmpmess.table;
        else % factor to variable message
            tmpmess = multpots([tmpmess pot(factor)]);
            %[mess(count) mstate] = maxpot(tmpmess,setdiff(tmpmess.variables,mess(count).variables));
            [mess(count) mstate] = maxpot(tmpmess,mess(count).variables,0);
        end
        if donorm && ~potistyped(mess(count)); % normalise the table to avoid over/under flow
            prefactor=max(mess(count).table(:));
            messlogprefactor(count)=log(prefactor)+logprefactor;
            mess(count).table=(mess(count).table)./prefactor;
        end
    end
end

% now find the maximum states: variable nodes are first in the ordering, so
for i=1:V
    [dum1 dum2 incoming]=find(A(:,i));
    tmpmess = multpots(mess(incoming));
    [tmpmess maxstate(i)]=maxpot(tmpmess,i);
end
maxval=tmpmess.table;
if donorm;logmaxval=log(max(table(multpots(mess(mess2var(1,A))))))+sum(messlogprefactor(mess2var(1,A)));
else logmaxval=log(maxval);
end
