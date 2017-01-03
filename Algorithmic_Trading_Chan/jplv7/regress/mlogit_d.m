% PURPOSE: An example of mlogit(),
%                        prt_reg().
%  maximum likelihood estimation
%  of multinomial logit model
%---------------------------------------------------
% USAGE: mlogit_d
%---------------------------------------------------

n=100; k=3;
x = randn(n,k);
beta = ones(k,1);
y = x*beta + randn(n,1);

[ys yi] = sort(y);
xs = x(yi,:);
z = zeros(n,1);

for i=1:33
z(i,1) = 0;
end;
for i=34:66
        z(i,1) = 1;
end;
for i=67:n
        z(i,1) = 2;
end;

res = mlogit(z,xs);
prt_reg(res);


