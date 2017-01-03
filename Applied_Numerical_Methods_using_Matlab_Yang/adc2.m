function d=adc2(a,b,c)
N=length(c);
d(find(a<b(2)))=c(1);
for i=2:N-1
   index=find(b(i)<=a&a<=b(i+1));  d(index)=c(i);
end
d(find(b(N)<=a))=c(N);
