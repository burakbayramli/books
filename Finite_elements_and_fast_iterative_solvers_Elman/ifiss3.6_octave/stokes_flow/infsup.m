%INFSUP computes inf-sup eigenvalue distribution 
%   IFISS scriptfile: DJS; 7 March 2005. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('inf-sup eigenvalue estimation \n')
leig=default('eig/eigs? 0/1 (default is eig)',0);
stab=default('stable/stabilised? 0/1 (default is stable)',0);
[np,nu]=size(Bst);
S=Bst*(Ast\Bst');
if stab==1, S=S+beta*C; end
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
