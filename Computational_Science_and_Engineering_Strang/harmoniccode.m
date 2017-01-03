%1.5  harmoniccode.m

%The parameters are L=10, n=200 and k=20, which means that the 
%interval is [-10,10], there are 200 meshpoints in [0,1] and we want 
%the first 20 eigenvalues.

%    to plot the wave function of the jth eigen state:
%    plot(x,V(1:dim,k-j+1));
%    We can convince ourselves that [-10,10] is big enough by looking at the
%    graph of the highest wave function.  SEE harmonic.mat****

function harmonic(L,n,k)        % positive integers L,n,k

h=1/n; N=2*n*L+1;               % N points in interval [-L,L]
K=toeplitz([2 -1 zeros(1,N-2]); % second difference matrix
V=diag((-L:h:L).^2/2);          % diagonal matrix from x^2/2
H=K/(2*h^2)+V;                  % Hamiltonian = kinetic + potential
[V,F]=eig(H,k,'sm');            % k eigenvectors and eigenvalues
E=diag(F)                       % smallest k eigenvalues of H
j=1:k;
plot(k+1-j,E);                  % MAYBE plot(1:k,E)?
