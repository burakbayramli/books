function newtable=sumGaussianMoment(table,vars)
newtable=table;
newvars=setdiff(1:length(table.mean),vars);
newtable.mean=table.mean(newvars);
newtable.covariance=table.covariance(newvars,newvars);