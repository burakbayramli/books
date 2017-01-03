function newpot = condpot(pot,varargin)
%CONDPOT Return a potential conditioned on another variable
% newpot = condpot(pot,x,y)
% condition the potential to return potential with distribution p(x|y), summing over
% remaining variables. If y is empty (or missing), return the marginal p(x)
% If both x and y are missing, just return the normalised table
if length(varargin)>0
    x=varargin{1};
    if length(varargin)==1
        y=[];
    else
        y=varargin{2};
    end
    for p=1:length(pot)
        if isempty(y)
            newpot(p)=sumpot(pot(p),setdiff(pot(p).variables,x));
            newpot(p).table=newpot(p).table./sum(newpot(p).table(:));
            
        else
            pxy=sumpot(pot(p),[x(:)' y(:)'],0);
            py =sumpot(pxy,y,0);            
            newpot(p)=divpots(pxy,py); % p(x|y) = p(x,y)/p(y)
        end
    end
else
    newpot=pot;
    for p=1:length(pot)
        newpot(p).table=newpot(p).table./sum(newpot(p).table(:));
    end
end