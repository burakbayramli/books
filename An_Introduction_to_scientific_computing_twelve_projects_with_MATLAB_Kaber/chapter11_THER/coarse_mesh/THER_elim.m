%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [Aelim,belim]=THER_elim(Atotal,btotal,Refpt,Refdir,Valdir); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [Aelim,belim]=THER_elim(Atotal,btotal,Refpt,Refdir,Valdir); 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Construction of the linear system (direct problem)
%%   Boundary conditions modifications
%%   THER: Thermic engineering
%%   
%%   Input : Atotal assembled matrix (direct problem)  
%%           btotal assembled right-hand side  
%%           Refpt  triangles vertices boundary references 
%%           Refdir boundary list with Dirichlet boundary conditions
%%           Valdir values for Dirichlet boundary conditions 
%%
%%   Output : Aelim modified matrix (direct problem)
%%            belim modified right-hand side
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    
    Nbpt=size(Atotal,1); 
    Ndir=size(Refdir,1);
%-> 1) Boundary values 
    TD=zeros(Nbpt,1); 
    for k=1:Ndir
    for i=1:Nbpt  
       if Refpt(i) == Refdir(k)  
          TD(i)=Valdir(k); 
       end    
    end
    end 
%-> 2) Right-hand side modification 
    belim=btotal-Atotal*TD; 
%-> 3) Matrix modification
    Aelim=Atotal; 
    for k=1:Ndir
    for i=1:Nbpt 
       if Refpt(i) == Refdir(k)
         for j=1:Nbpt 
            Aelim(i,j)=0; 
            Aelim(j,i)=0; 
         end 
         Aelim(i,i)=Atotal(i,i); 
%-> 4)   Right-hand side modification 
         belim(i)=TD(i)*Atotal(i,i); 
      end 
    end
    end    
