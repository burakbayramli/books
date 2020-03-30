function xlat = lattice(nmax, d)

% nmax: number of points (should be power of 2)
% d:    dimension of the problem

mmax=log2(nmax); 
h=17797; %Korobov generator that works for a range of n=2^m

%van der corput sequence
y=zeros(nmax,1); twom=1;
for m=1:mmax
    twomold=twom; twom=2*twom;
    y(twomold+1:twom)=y(1:twomold)+1/twom;
end

%rank-1 lattice points
xlat=zeros(nmax,d);
xlat(:,1)=y;
for j=2:d
    xlat(:,j)=mod(xlat(:,j-1)*h,1);
end

%otherwise do it like this
% xlat(:,1)=(0:nmax-1)/nmax;
% for j=1:d
%     xlat(:,d)=mod(xlat(:,d-1)*h,1);
% end
%simpler but only good for n=nmax

