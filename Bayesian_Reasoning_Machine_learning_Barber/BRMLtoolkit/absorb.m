function [newpots,newpotb] = absorb(pota,pots,potb,varargin)
%ABSORB Update potentials in absorption message passing on a Junction Tree
% [newpots,newpotb] = absorb(pota,pots,potb,<sum/max>)
%
% Absorption: update separator potential pots and potb, after absorbing
% from potential pota.  pota --> pots --> potb
% By default sum-absorption is carried out. If the optional flag is 1, sum
% absorption is performed, otherwise max-absorption is performed.
if isempty(varargin)
    dosum=1;
else
    dosum=varargin{1};
end
if dosum
    newpots = sumpot(pota,setdiff(pota.variables,pots.variables));
else
    newpots = maxpot(pota,setdiff(pota.variables,pots.variables));
end
newpots.table=newpots.table+eps; pots.table=pots.table+eps; % in case any zeros
invpots  = pots; invpots.table=1./(invpots.table);
newpotb = multpots([potb newpots invpots]);