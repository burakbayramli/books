function [J,JJ]=Jkb(K,beta)%the 1st kind of kth-order Bessel ftn
tmpk= ones(size(beta));
for k=0:K
  tmp= tmpk;  JJ(k+1,:)= tmp;
  for m=1:100
    tmp= ?????????????????????;
    JJ(k+1,:)= JJ(k+1,:)+tmp;
    if norm(tmp)<.001, break; end
  end
  tmpk=tmpk.*beta/2/(k+1);  
end 
J=JJ(K+1,:);
