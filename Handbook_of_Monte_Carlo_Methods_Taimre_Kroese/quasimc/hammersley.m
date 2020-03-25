function out = hammersley(b,N)
dim = numel(b);
out = zeros(N,dim);
out(2:N,2:dim) = halton(b(1:dim-1),N-1);
out(:,1) = [0:N-1]/N;