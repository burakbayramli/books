function D = scale(A);
% calculates equilibration factors for rowwise equilibration
[M,N] = size(A);
D = sparse(N,N); E = ones(N,1);
for I = 1:N
   C = abs(A(I,:))*E;
   D(I,I) = 1/C;
end

