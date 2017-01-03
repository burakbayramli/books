function newtable=sumpotGaussianMoment(pot,vars)
newtable=pot.table;
v=potvariables(pot);
newvars=setdiff(1:length(v),vars);
newinds=getdimind(pot.table.dim,newvars);
newtable.mean=pot.table.mean(newinds);
newtable.covariance=pot.table.covariance(newinds,newinds);
newtable.dim=pot.table.dim(newvars);
