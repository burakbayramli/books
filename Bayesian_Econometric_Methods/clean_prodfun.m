
load raw_cigdata.txt;
%Clean the production function data
labor = raw_cigdata(:,1);
capital = raw_cigdata(:,3);
real_out = raw_cigdata(:,2).*(1./raw_cigdata(:,4));
%real_out = raw_cigdata(:,2);
x = [ones(length(real_out),1) log(labor) log(capital)];
y = log(real_out);
[bhat stderr tstat sig rsq] = ols(x,y)
fff = [labor capital real_out];
save prod_function fff -ascii -tabs