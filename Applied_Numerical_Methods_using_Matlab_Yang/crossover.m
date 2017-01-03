function chrms2=crossover(chrms2,Nb)
% crossover between two chromosomes
Nbb=length(Nb); 
b2=0;
for m=1:Nbb
   b1=b2+1; bi=b1+mod(floor(rand*Nb(m)),Nb(m)); b2=b2+Nb(m);
   tmp=chrms2(1,bi:b2); 
   chrms2(1,bi:b2)=chrms2(2,bi:b2);
   chrms2(2,bi:b2)=tmp;
end
