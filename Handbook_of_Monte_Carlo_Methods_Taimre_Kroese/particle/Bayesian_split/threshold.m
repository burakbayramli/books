function gam=threshold(S,rho,Gamma)
% determine threshold
S=sort(S);
for i=1:length(S)
   if  mean(S>=S(i))<=rho, break ,end
end
gam=min(S(i),Gamma);   


