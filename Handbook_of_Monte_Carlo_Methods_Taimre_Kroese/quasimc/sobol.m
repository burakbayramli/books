function p = sobol(d,N)
b=2; %always base 2
r =  floor(log(N)/log(b))+1;
bb = 1./b.^(1:r);
bbb = repmat(bb,N+1,1);
p = zeros(N+1,d);
V=zeros(r,r);
VV=sobmat(d,r);
a=repmat((0:N)',1,r);
for i=1:r
    	a(:,i) = mod(a(:,i),b);
    	a(:,(i+1):r) = floor(a(:,(i+1):end)./b);
    	% a contains now the b-ary expansion of 0:N    
end
for i=1:d
    	V(:,:)=VV(:,:,i); 
    	p(:,i)=sum(bbb.*mod(a*V',b),2);
end

