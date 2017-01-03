%nm132_2
clear
N=1000; x=rand(1,N);
kk=[-100:100]'; W=kk*pi/100;
%for for loop
tic
for k =1: length(W)
   X_for1(k) =0; %zeros(size(W));
   for n=1:N, X_for1(k) = X_for1(k) +x(n)*exp(-j*W(k)*(n-1)); end
end
toc
%for vector loop
tic
X_for2 =0 ; %zeros(size(W));
for n =1: N
    X_for2 = X_for2 +x(n)*exp(-j*W*(n-1));
end
toc
%vector operation
tic
nn=[1:N];
X_vec =exp(-j*W*(nn-1))*x(nn)';
toc
discrepancy= norm(X_for1-X_vec.')
discrepancy= norm(X_for2-X_vec)
