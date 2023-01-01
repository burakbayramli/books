%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [Numt]=THER_local(Pt,Numtri,Coorpt); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%       function [Numt]=THER_local(Pt,Numtri,Coorpt); 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Localization of the point Pt inside a triangle
%%   THER: Thermic engineering
%%   
%%   Input : Pt      coordinates of the point Pt  
%%           Coorpt  triangles vertices coordinates
%%           Numtri  list of triangles vertices  
%%
%%   Output : Numt number of the triangle containing point Pt
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    
    Numt=0; 
    for k=1:size(Numtri,1) 
       M=[Coorpt(Numtri(k,1),:); Coorpt(Numtri(k,2),:); Coorpt(Numtri(k,3),:)]'; 
       Delta=det([M; 1 1 1]); 
       Coorb1=((M(2,2)-M(2,3))*Pt(1)+(M(1,3)-M(1,2))*Pt(2)+M(1,2)*M(2,3)-M(2,2)*M(1,3)); 
       Coorb2=((M(2,3)-M(2,1))*Pt(1)+(M(1,1)-M(1,3))*Pt(2)+M(1,3)*M(2,1)-M(2,3)*M(1,1)); 
       Coorb3=((M(2,1)-M(2,2))*Pt(1)+(M(1,2)-M(1,1))*Pt(2)+M(1,1)*M(2,2)-M(2,1)*M(1,2)); 
       Coorb1 = Coorb1 / Delta;
       Coorb2 = Coorb2 / Delta;
       Coorb3 = Coorb3 / Delta;
       if Coorb1 >= 0 & Coorb2 >= 0 & Coorb3 >= 0  
%--->      Point Pt lays inside triangle k  
           Numt=k; 
          return 
       end
    end 
