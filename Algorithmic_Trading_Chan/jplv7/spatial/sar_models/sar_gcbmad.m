% PURPOSE: compute posterior probabilities of
% different SAR models based on varying X matrices

clear all;

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));

% latt = anselin(:,4);
% long = anselin(:,5);
% [junk W junk] = xy2cont(latt,long);
[n junk] = size(W);

kin = 3;
kout = 5;

xo = randn(n,kin);
x = [ones(n,1) xo];
z = randn(n,kout);
[n,k] = size(x);
vnames = strvcat('y','constant','x1','x2','x3','x1out','x2out','x3out','x4out','x5out');
rho = 0.6;
sige = 0.1;
b = ones(kin+1,1);
b(1,1) = 1.0;
y = (speye(n) - rho*W)\(x*b) + (speye(n) - rho*W)\(randn(n,1)*sqrt(sige));

% write data out to a file
out = [y xo z];
in.fid = fopen('test.data','w');
in.width = 10000;
mprint(out,in);
fclose(in.fid);

ndraw = 10000;
prior.rmin = -1.0;
prior.rmax = 1.0;
xmat = studentize([xo z]);

result = sar(y,xmat,W);
prt(result);

res3 = sar_gcbma(y,xmat,W,ndraw,prior);
prt_bmas(res3,vnames);

