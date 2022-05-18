%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 2 - project 11
%%   THER: Thermic engineering
%%   Solution of the inverse problem : computation of the
%%   thermic resistances when temperature field is known
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    clear all ; close all ;
%   Geometry
%%  Coarse mesh "mesh.coarse" : 173 vertices and  304 triangles
    Filename='mesh.coarse';
    [Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri]=THER_readmesh(Filename);
%   Read the temperature fields (output from ovenexo1)
    fid  = fopen ('temp.end','r');
    load fid T0 Tres;
%-> Linear system (optimization problem)
    Tcui = 250 ;  % Optimal temperature (data)
    [Aopt,bopt]=THER_matrix_inv(Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri,T0,Tres,Tcui); 
%-> Solution of the optimization problem
    alpha=Aopt\bopt; 
%-> Optimized temperature field
    fprintf('\n Optimized temperature') 
    Topt=zeros(Nbpt,1); 
    for k=1:Nbtri
       for i=1:3
       Topt(Numtri(k,i))=T0(Numtri(k,i));
       for j=1:size(Tres,2)
          Topt(Numtri(k,i))=Topt(Numtri(k,i))+Tres(Numtri(k,i),j)*alpha(j);
       end    
     end 
     end    
     nf=10;figure(nf);fs=18;
     trisurf(Numtri,Coorpt(:,1),Coorpt(:,2),Topt,Topt); 
     title('Optimized temperature field','FontSize',fs);
     fid = fopen('topt.end','w');
     save fid Topt;