function A=randint(n,m,k)
%generates an n by m matrix whose entries are random integers whose
%absolute values do not exceed k
A=zeros(n,m);
for i=1:n
   for j=1:m
      x=(2*k+1)*rand-k; %produces a random real number in (-k,k+1)
      A(i,j)=floor(x);
   end
end

      
      