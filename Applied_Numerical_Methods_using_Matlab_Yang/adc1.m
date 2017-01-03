function d=adc1(a,b,c)
%Analog-to-Digital Converter
%Input   a=analog signal, b(1:N+1)=boundary vector
c(1:N)=centroid vector
%Output: d=digital samples
N=length(c);  
for n=1:length(a)
  I=find(a(n)<b(2:N));
  if ~isempty(I), d(n)=c(I(1));
   else           d(n)=c(N);
  end    
end
