function out=stratified_split(N,Nt)
r=mod(N,Nt); 
if abs(r)<0.01,   r=0;end
B=zeros(Nt,1);
B(randsample(Nt,r))=1;
out=B+floor(N/Nt);

