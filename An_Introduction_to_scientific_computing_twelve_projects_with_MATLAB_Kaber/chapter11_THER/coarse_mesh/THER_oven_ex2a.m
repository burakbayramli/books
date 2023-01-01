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
%%   Computation of the global temperature field
%%   when the resistances values are known
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
    nr=size(Tres,2);alpha=zeros(nr,1);
    alpha(1:nr)=25000; 
%-> Temperature field
    fprintf('\n Combinated temperature field') 
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
     title('Global temperature field','FontSize',fs);
     fid = fopen('topt.end','w');
     save fid Topt;