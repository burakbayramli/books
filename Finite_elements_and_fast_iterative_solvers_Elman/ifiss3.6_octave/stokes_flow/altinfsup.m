%ALTINFSUP inf-sup eigenvalue distribution of potential flow
%   IFISS scriptfile: DJS; 19 March 2006. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('alternative inf-sup eigenvalue estimation \n')
leig=default('eig/eigs? 0/1 (default is eig)',0);
stab=default('stable/stabilised? 0/1 (default is stable)',0);
[np,nu]=size(Bst);
S=Bst*(G\Bst');
if stab==1, 
    [Cp1,Cp2] = Cpre_q1p0(xy,xyp,ev,domain);
    S=S+beta*Cp1; end
%
% compute discrete eigenvalues
S=0.5*(S+S');Q=0.5*(Q+Q');
if leig==1 
   [V,D,flag] = eigs(S,Q,4,'sm');
   e=diag(D); e=sort(real(e));
   fprintf('inf-sup constant is %g \n',e(2))
else
   [V,D]=eig(full(S),full(Q));
e=diag(D); [e,k]=sort(real(e)); V=V(:,k); pp=length(e);
fprintf('inf-sup constant is %g \n',e(2))
fprintf('     upper bound is %g \n',e(pp))
end
evn=default('input number of plotted eigenvectors? (default is 0)',0);
if evn > 0
for kk=2:evn
     if domain==1,   eplot(V(:,kk),ev,xy,x,y,kk); 
     elseif domain==3,  eplotl(V(:,kk),ev,xy,x,y,kk); end
     title(['eigenvector number ', num2str(kk)])
end
end
