function newpot=orderpot(pot,varargin)
%ORDERPOT Return potential with variables reordered according to order
% newpot=orderpot(pot,<order>)
% if order is missing or empty, the variables are sorted (low to high)
newpot=pot;
if nargin==2; inorder=varargin{1}; else inorder=[]; end
for p=1:length(pot)
    if isempty(inorder)
        order=sort(pot(p).variables);
    else
        order=inorder;
    end
    [a iperm]=ismember(order,pot(p).variables);
    iperm=iperm(iperm>0);
    % [a iperm]=ismember(pot(p).variables,order);
    newpot(p).variables=order(a); nstates=numstates(pot(p));
    if ~isscalar(nstates) && ~isempty(nstates)
        newpot(p).table=permute(pot(p).table,iperm);
    else
        newpot(p).table=pot(p).table;
    end
end