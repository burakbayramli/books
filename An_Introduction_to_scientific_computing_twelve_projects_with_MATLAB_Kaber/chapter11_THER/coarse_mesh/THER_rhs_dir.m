%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [b]=THER_rhs_dir(Hsp,Numtri,Coorpt); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%       function [b]=THER_rhs_dir(Hsp,Numtri,Coorpt); 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%
%%   Computation of the right-hand side corresponding to
%%   a single heat source
%%   THER: Thermic engineering
%%   
%%   Input : Hsp     heat source position    
%%           Coorpt  triangles vertices coordinates
%%           Numtri  list of triangles vertices  
%%
%%   Output : b       heat source right-hand side
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    
    Nbpt=size(Coorpt,1);
    Nbtri=size(Numtri,1);
    b=zeros(Nbpt,1);
%%  heat source definition :
%%   
%%  f(x,y)=F0/2. exp(-D2) with  D2=[(x-xh)**2+(y-yh)**2]/2.*R**2
%%
    R=5.d-02;R2=2.*R*R;F0=1.;F=F0/2.;
    for k=1:Nbtri
%      element geometry
       M=[Coorpt(Numtri(k,1),:); Coorpt(Numtri(k,2),:); Coorpt(Numtri(k,3),:)]'; 
       Delta=abs(det([M; 1 1 1]));D6=Delta/6.;
%      element right-hand side
       th=zeros(3,1);
       for i=1:3
          dx=Coorpt(Numtri(k,i),1)-Hsp(1);dx2=dx*dx;
          dy=Coorpt(Numtri(k,i),2)-Hsp(2);dy2=dy*dy;
          dz=(dx2+dy2)/R2;
          th(i)=F*exp(-dz);
       end
%      element contribution
       for i=1:3 
          b(Numtri(k,i))=b(Numtri(k,i))+th(i)*D6;
       end 
    end
