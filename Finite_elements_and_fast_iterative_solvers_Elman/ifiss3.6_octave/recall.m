%RECALL   query the current discretized problem in the workspace
%IFISS scriptfile: DJS; 27 April 2012, 9 January 2019
%Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
%
%For more help, type 'helpme'.
if exist('pde','var')==0,
error('No discrete problem found in the workspace!'), 
end
%
if pde>10 & pde <20, fprintf(' Unsteady solution\n'), pdex=pde-10; 
else, pdex=pde; end
%
if pdex<3,
fprintf(' scalar problem       : domain       : approximation') 
     if pdex==1, fprintf('\n Poisson equation     :'),
 elseif pdex==2, fprintf('\n Convection-Diffusion :'),
 elseif pdex==0, fprintf('\n Biharmonic equation  :'),
    end
    if domain==1, fprintf(' square       :'), 
elseif domain==2, fprintf(' L-shaped     :'), 
            else, fprintf(' non-standard :'),  
    end
    if qmethod==1, fprintf(' Q1\n'), 
elseif qmethod==2, fprintf(' Q2\n'),
elseif qmethod==3, fprintf(' Q3\n'),
			else,  fprintf(' non-standard\n'), 
    end
else
fprintf(' flow problem  :        geometry          : approximation') 
    if pdex==3, fprintf('\n Stokes        :'), 
elseif pdex==4, fprintf('\n Navier-Stokes :'),
elseif pdex==5, fprintf('\n Boussinesq    :'), 
    end
	if domain==1,  fprintf(' enclosed square          :'), 
elseif domain==3,  fprintf(' step with inflow/outflow :'),
elseif domain==4,  fprintf(' channel with obstacle    :'), 
elseif domain==7,  fprintf(' enclosed cavity          :'), 
elseif domain==10, fprintf(' simple channel           :'),						
	         else, fprintf(' non-standard'),  
	end					
	if qmethod==0, fprintf(' Q1-Q1\n'), 
elseif qmethod==1, fprintf(' Q1-P0\n'), 
elseif qmethod==2, fprintf(' Q2-Q1\n'), 
elseif qmethod==3, fprintf(' Q2-P1\n'), 
elseif qmethod==12, fprintf(' Q2-Q1-Q2\n'), 
else, fprintf('non-standard\n'), 
    end
end
