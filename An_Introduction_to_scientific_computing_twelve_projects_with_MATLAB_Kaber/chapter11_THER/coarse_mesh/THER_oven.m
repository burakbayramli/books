%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [T0,Tres,Numtri,Coorpt]=THER_oven(Refdir,Valdir,Conduc,Pres,Filename); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [T0,Tres,Numtri,Coorpt]=THER_oven(Refdir,Valdir,Conduc,Pres,Filename)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Solution of the direct problem 
%%   THER: Thermic engineering
%%   
%%   Input : Refdir  Boundary Dirichlet references 
%%           Valdir  Boundary Dirichlet values 
%%           Conduc  heat conductivity coefficients 
%%           Pres    resistances positions (coordinates)
%%           Filename name of the mesh file
%%
%%   Output : T0 first temperature field (no warming)
%%            Tres temperature fields associated to warming terms
%%            Numtri  list of triangles vertices  
%%            Coorpt  triangles vertices coordinates
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%-> Read the mesh file
    [Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri]=THER_readmesh(Filename); 
%-> Plot the mesh
    THER_meshplot(Coorpt,Numtri,Reftri,Nbtri);
%-> Compute the linear system
    [Atotal]=THER_matrix_dir(Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri,Conduc); 
    btotal=zeros(Nbpt,1);
%-> Boundary conditions 
    [Aelim,belim]=THER_elim(Atotal,btotal,Refpt,Refdir,Valdir); 
%   Plot matrix A 
    nf=25;figure(nf);fs=18;
    spy(Aelim); 
    title('Matrix A (coarse mesh)','FontSize',fs);
%-> Solution of the first problem (no warming)
    T0=Aelim\belim; 
    fprintf('\n Homegeneous temperature field') 
%-> Solution of all problems (with warming terms) 
%   Boundary condition is now TD=0
    for i=1:size(Pres,1), 
      Hsp=Pres(i,:); 
      b=THER_rhs_dir(Hsp,Numtri,Coorpt); 
      Tres(:,i)=Aelim\b; 
      fprintf('\n Temperature field for resistance %i',i) ;
    end 
