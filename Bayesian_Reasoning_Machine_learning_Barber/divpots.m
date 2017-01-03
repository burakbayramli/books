function newpot=divpots(pota,potb)
%DIVPOTS Divide potential pota by potb
% newpot=divpots(pota,potb)
pota.table=pota.table+eps;potb.table=potb.table+eps; % in case any zeros
if length(potb.table)==1;
    newpot=pota; newpot.table=pota.table./potb.table;
else
    potb.table=1./potb.table;
    newpot=multpots([pota potb]);
end