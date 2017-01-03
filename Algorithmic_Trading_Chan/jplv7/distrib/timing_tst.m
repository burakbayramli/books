% timing tests for chi-squared deviation generation

n = 3000;

tic;
for i=1:1500;
out = chi2rnd(4,n,1);
end;
toc;

tic;
for i=1:1500;
out = rchisq(n,4);
end;
toc;
