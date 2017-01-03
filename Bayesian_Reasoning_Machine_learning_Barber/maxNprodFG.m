function [maxstate maxval mess]=maxNprodFG(pot,A,mess,Nmax)
%MAXNPRODFG N-Max-Product algorithm on a Factor Graph (Returns the Nmax most probable States)
% [maxstate maxval mess]=maxNprodFG(pot,A,mess,Nmax)
%
% pot is a set of potentials
% A is a FG adjacency matrix with message numbers on the non-zero elements
% mess is the message initialisation. If mess=[] the standard
% initialisation is used.
%
% returns most Nmax most probable joint states and their values, along with the messages.
% maxstate : each row is a state, with the first row the most likely state,
% followed by the next most likely etc.
[variables nstates]=potvariables(pot);
V=length(variables); N=size(A,1);
fnodes=zeros(1,N); fnodes(V+1:end)=1:N-V; % factor nodes
vnodes=zeros(1,N); vnodes(1:V)=1:V; % variable nodes
nmess=full(max(max(A))); % number of messages

% The Nmax most probable messages are stored using an extra variable for each message:
mvars=V+1:V+nmess; % For each message this will index the n-most probable messages

if isempty(mess) % message initialisation:
    for count=1:nmess
        [FGnodeA FGnodeB]=find(A==count);
        if fnodes(FGnodeA)>0 % factor to variable message:
            mess(count).variables=vnodes(FGnodeB); % variable of this message
            mess(count).variables=vnodes(FGnodeB); % variable of this message
            % if the factor is at the edge (simplical), need to set message to the factor potential
            if length(find(A(FGnodeA,:)))==1
                mess(count).table=pot(fnodes(FGnodeA)).table;
            else % not a simplical node:
                mess(count).table=myones(nstates(vnodes(FGnodeB)));
            end
        else % variable to factor message:
            mess(count).variables=vnodes(FGnodeA); % variable of this message
            for n=1:Nmax
                mess(count).table=myones(nstates(vnodes(FGnodeA)));
            end
        end
    end
end

% now do the message passing:
for count=1:nmess
    [FGnodeA FGnodeB]=find(A==count);
    FGparents=setdiff(find(A(FGnodeA,:)),FGnodeB); % FGparent nodes of FGnodeA
    if ~isempty(FGparents)
        factor=fnodes(FGnodeA);
        tmpmess = multpots(mess(A(FGparents,FGnodeA)));
        if factor==0; % variable to factor message
            mess(count)=tmpmess;
        else % factor to variable message
            tmpmess = multpots([tmpmess pot(factor)]); % multiply all the messages
            [tmess maxst]= maxNpot(tmpmess,mess(count).variables,Nmax,0); % max over all messages retaining only Nmax
            mess(count).table = reshape(vertcat(tmess.table{1:Nmax}),[prod(size(tmess.table{1})),Nmax]);
            mess(count).variables=[mess(count).variables mvars(count)];  
        end
    end
end

% now find the maximum states: variable nodes are first in the ordering, so
for i=1:V
    [dum1 dum2 incoming]=find(A(:,i));
    tmpmess = multpots(mess(incoming));
    [a b]=maxNpot(tmpmess,[],Nmax,0);
    for n=1:Nmax
        maxstate(n,i)=b{n}(1); % the first component is the variable
    end
end
maxval=a.table;