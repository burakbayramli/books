%nm131_2_1: nested structure computation may be critical
clear
K=155; lambda=100; 
p=exp(-lambda);
S=0;
for k=1:K
  p= p*lambda/k; S=S+p;
end
S
