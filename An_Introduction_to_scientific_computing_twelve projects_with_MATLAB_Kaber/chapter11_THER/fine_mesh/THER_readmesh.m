%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri]=THER_readmesh;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%       function [Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri]=THER_readmesh
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%   
%%   Read the files containing the mesh data (created by PDE toolbox)
%%   THER: Thermic engineering
%%
%%  Output : Nbpt    number of mesh points 
%%           Nbtri    number of triangles
%%           Coorpt  triangles vertices coordinates
%%           Refpt   triangles vertices boundary references 
%%           Numtri  list of triangles vertices  
%%           Reftri  list of triangles references
%%
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
   load Nbpt Nbtri ;
   load Coorneu  ; Coorpt=Coorneu ; clear Coorneu ;
   load Refneu ; Refpt=Refneu ; clear Refneu ;
   load Numtri ;
   load Reftri ;