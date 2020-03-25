function out = halton(b,N);
dim = numel(b);
out = zeros(N,dim);
for i=1:dim
    out(:,i) = vdc(b(i),N); 
end
    
