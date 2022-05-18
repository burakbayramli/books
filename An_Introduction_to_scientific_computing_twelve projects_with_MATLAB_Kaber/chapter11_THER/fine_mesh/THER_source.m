%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [b]=THER_source(Pt,Numtri,Coorpt); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%       function [b]=THER_source(Pt,Numtri,Coorpt); 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%
%%   Localization of the point Pt inside a triangle
%%   Computation of the corresponding element right-hand side
%%   THER: Thermic engineering
%%   
%%   Input : Pt      coordinates of the point Pt  
%%           Coorpt  triangles vertices coordinates
%%           Numtri  list of triangles vertices  
%%
%%   Output : b corresponding element right-hand side
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    
    Numt=THER_local(Pt,Numtri,Coorpt); 
    F=1;
    if Numt== 0  
       fprintf('\n Point Pt is outside the mesh. End of search'); 
       stop; 
    end  
    M=[Coorpt(Numtri(Numt,1),:); Coorpt(Numtri(Numt,2),:); Coorpt(Numtri(Numt,3),:)]'; 
    Delta=abs(det([M; 1 1 1]));D6=Delta/6.;
    Nbpt=size(Coorpt,1);
    b=zeros(Nbpt,1);
%%  use of general F
    R=5.d-02;R2=2.*R*R;F0=1.;F=F0/2.;
    th=zeros(3,1);
    for i=1:3
       dx=Coorpt(Numtri(Numt,i),1)-Pt(1);dx2=dx*dx;
       dy=Coorpt(Numtri(Numt,i),2)-Pt(2);dy2=dy*dy;
       dz=(dx2+dy2)/R2;th(i)=F*exp(-dz);
    end
    for i=1:3 
       b(Numtri(Numt,i))=th(i)*D6;
    end 