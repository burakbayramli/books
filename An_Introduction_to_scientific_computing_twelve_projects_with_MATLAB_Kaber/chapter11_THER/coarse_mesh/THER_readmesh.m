%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri]=THER_readmesh(Filename);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%       function [Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri]=readmesh(Filename)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%   
%%   Read the file containing the mesh (project 11)
%%   THER: Thermic engineering
%%
%%  Input  : Filename name of the file 
%%
%%  Output : Nbpt    number of mesh points
%%           Nbtri   number of mesh triangles
%%           Coorpt  triangles vertices coordinates
%%           Refpt   triangles vertices boundary references 
%%           Numtri  list of triangles vertices  
%%           Reftri  list of triangles references
%%
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    fid=fopen(Filename,'r');
    N=fscanf(fid,'%i');
    Nbpt=N(1);
    Nbtri=N(2);
    line=fgets(fid);
    tmp = fscanf(fid,'%f',[4,Nbpt]);
    Coorpt=tmp(2:3,:)';
    Refpt=tmp(4,:)';
    tmp = fscanf(fid,'%i',[5,Nbtri]);
    Numtri=tmp(2:4,:)'; 
    Reftri=tmp(5,:)'; 
