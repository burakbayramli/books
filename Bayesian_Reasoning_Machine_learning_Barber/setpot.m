function [newpot]= setpot(pot,evvariables,evidstates,varargin)
%SETPOT sets potential variables to specified states
% newpot = setpot(pot,variables,evidstates,<large>)
%
% set variables in potential pot to evidential states in evidstates
% Note that the new potential does not contain the evidential variables
% This contrasts with setevpot.m which does contain the evidential variables
% Use large=1 to run on large potentials
if isempty(varargin); large=0; else large=varargin{1}; end
if ~large
    for p=1:length(pot)
        [newpot(p).variables]= setdiff_unsorted(pot(p).variables,evvariables);
        [tmp newpot_variables]=ismember(newpot(p).variables,pot(p).variables);
        nstates=numstates(pot(p));
        [dum toelim]=ismember(evvariables,pot(p).variables);
        thisevvariables=evvariables(find(toelim));
        thisevidstates=evidstates(find(toelim));
        totalstates=prod(nstates);  % total number of states in original potential
        allstates = ind2subv(nstates,1:totalstates);
        % now find the rows we want:
        [dum thistoelim]=ismember(thisevvariables,pot(p).variables);
        poss=1:totalstates;
        for v=1:length(thisevvariables)
            if v==1
                poss = find(allstates(:,thistoelim(v))==thisevidstates(v));
            else
                poss =intersect(poss,find(allstates(:,thistoelim(v))==thisevidstates(v)));
            end
        end
        if length(newpot_variables)>1
            newpot(p).table = reshape(pot(p).table(poss),nstates(newpot_variables));
        else
            newpot(p).table = pot(p).table(poss);
        end
    end
else
    for p=1:length(pot)
        [vars nstates] = potvariables(pot(p));
        for vind=1:length(evvariables);
            tmppot.variables=evvariables(vind);
            tmppot.table=zeros(nstates(vars==evvariables(vind)),1);
            tmppot.table(evidstates(vind))=1;
            if vind>1
                delpot=multpots([tmppot delpot]);
            else
                delpot=tmppot;
            end
        end
        newpot(p) = sumpot(multpots([delpot pot(p)]),evvariables);
    end
end