function [jtpot jtsep pottoJTclique]=jtassignpot(pot,infostruct,varargin)
%JTASSIGNPOT Assign potentials to cliques in a Junction Tree
% [jtpot jtsep pottoJTclique]=jtassignpot(pot,infostruct,<pottoJTclique>)
% Inputs:
% pot : a set of potentials
% infostruct : information about the structure of the Junction Tree (see jtree.m)
% optional <pottoJTclique> : a vector so that pottoclique(i)=j says that pot(i) is assigned to clique jtpot(j)
% Outputs:
% pottoJTclique : contains the clique potential that each pot is assigned to
% See also jtree.m
if isempty(varargin);
    pottoJTclique=[];
else
    pottoJTclique=varargin{1};
end
sepind=infostruct.sepind;
% Assign all separator potentials to 1 initially
C=size(infostruct.cliquetree,1);
[variables nstates]=potvariables(pot);

jtsep=[]; % needed in case there are no separators
for s=1:max(sepind(:))
    jtsep(s).variables=infostruct.separator{s};
    jtsep(s).table=myones(nstates(jtsep(s).variables));
end

% Assign all clique potentials to 1 initially
for c=1:C
    jtpot(c).variables=infostruct.clique{c};
    jtpot(c).table=myones(nstates(jtpot(c).variables));
end
if isempty(pottoJTclique)
    for p=1:length(pot)
        % find the clique for this potential and multiply it to the existing potential
        for c=1:C
            %if prod(real(ismember(pot(p).variables,jtpot(c).variables)))
                if all(ismember(pot(p).variables,jtpot(c).variables))
                jtpot(c)=multpots([jtpot(c) pot(p)]);
                pottoJTclique(p)=c; % store for future use
                break
            end
        end
    end
else
    for p=1:length(pot)
        c=pottoJTclique(p);
        jtpot(c)=multpots([jtpot(c) pot(p)]);
    end
end