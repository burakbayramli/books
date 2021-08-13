%BSOLVE solves Galerkin system using backslash
%IFISS scriptfile: DJS;  18 November 2009. 
%Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage
%
if exist('pde','var')==0,
   error('You need to set up a specific discrete problem first!'), 
end
tic, fprintf('solving linear system ...  ')
xgal=Agal\fgal;
fprintf('done\n')
etoc=toc; fprintf('Galerkin system solved in  %8.3e seconds\n',etoc) 
