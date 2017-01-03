function [rslt,xx,yy]=nngridsamp(woh,woo,N,rng,stps,ou)
% [rr,xx,yy]=ngridsamp(woh,woo,N,range,steps,ou) sample NN output over a grid
%   woh and woo define a neural net.  Vary each input dimension 
%   over <steps> values between the corresponding pair of 
%   numers in <range>, (subject to normalization in N)
%   and return result for output unit <ou>.
% 2001-02-09 dpwe@ee.columbia.edu
% $Header: $

xmin = rng(1);
xmax = rng(2);
ymin = rng(3);
ymax = rng(4);

xx = (xmin:((xmax-xmin)/(stps-1)):xmax)';
yy = (ymin:((ymax-ymin)/(stps-1)):ymax)';

rslt = zeros(stps, stps);

isize = 2;

if length(N) == 0
  N = ones(2, isize);
end

npats = stps;

for i = 1:stps;
  xxx = xx(i);
  pats = [(xxx*ones(npats,1)),yy];
  out = nnfwd(pats,N,woh,woo);
  rslt(:,i) = out(:,ou);
end

%contour(xx,yy,rslt);
% Npats = (pats-(ones(npats,1)*N(1,:)))./(ones(npats,1)*N(2,:));
