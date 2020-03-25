function p = faure(b,d,N)
r = floor(log(N)/log(b))+1; %largest number of digits
bb=repmat(1./b.^(1:r),N+1,1); %rows (1/b, 1/b^2,...)
p = zeros(N+1,d);
C = zeros(r,r);
for j=1:r
    	for i=1:j
		C(i,j) = mod(nchoosek(j-1,i-1),b);
    end 
end
C=C';
a=repmat((0:N)',1,r); 
for i=1:r-1
    	a(:,i)=mod(a(:,i),b); 
    	a(:,(i+1):r)=floor(a(:,(i+1):r)/b);
    	% a now contains the b-ary expansion of 0:N
end
p(:,1)=sum(bb.*a,2);
for k=2:d
    	a=mod(a*C,b); %permuted b-ary expansion of 0:N
    	p(:,k)=sum(bb.*a,2);
end
