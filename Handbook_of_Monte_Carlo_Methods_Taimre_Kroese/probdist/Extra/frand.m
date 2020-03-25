function z=frand(m,n)
% F(m,n) generator via the Gamma distribution (Algorithm 4.30)
% vectors 'm' and 'n' have to be of the same size

z=nan(size(m));
for i=1:length(m)
z(i)=gamrand(m(i)/2,1/2)/gamrand(n(i)/2,1/2)*n(i)/m(i);
end