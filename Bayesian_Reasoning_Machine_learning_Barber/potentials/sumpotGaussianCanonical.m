function newtable=sumpotGaussianCanonical(pot,vars)
newtable=pot.table;
[v ns]=potvariables(pot);
newvars=setdiff(1:length(v),vars);
newinds=getdimind(pot.table.dim,setdiff(1:length(v),vars));
oldinds=getdimind(pot.table.dim,vars);
%newinds=setdiff(1:length(pot.table.invmean),vars);
newtable.invcovariance=pot.table.invcovariance(newinds,newinds)-...
    pot.table.invcovariance(newinds,oldinds)*inv(pot.table.invcovariance(oldinds,oldinds))*pot.table.invcovariance(oldinds,newinds);
newtable.invmean=pot.table.invmean(newinds)-...
    pot.table.invcovariance(newinds,oldinds)*inv(pot.table.invcovariance(oldinds,oldinds))*pot.table.invmean(oldinds);
newtable.logprefactor=pot.table.logprefactor+....
    0.5*pot.table.invmean(oldinds)'*inv(pot.table.invcovariance(oldinds,oldinds))*pot.table.invmean(oldinds)+...
    0.5*logdet(2*pi*pot.table.invcovariance(oldinds,oldinds));
newtable.type='GaussianCanonical';
newtable.dim=pot.table.dim(newvars);
