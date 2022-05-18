%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function THER_meshplot(Coorpt,Numtri,Reftri,Nbtri) ;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function THER_meshplot(Coorpt,Numtri,Reftri,Nbtri)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Plotting the finite element mesh (direct problem)
%%   THER: Thermic engineering
%%  
%%   Input : Nbtri   number of mesh triangles 
%%           Coorpt  triangles vertices coordinates
%%           Refpt   triangles vertices boundary references 
%%           Numtri  list of triangles vertices  
%%           Reftri  list of triangles references
%%
%%   Output : Plot the mesh
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    
x=zeros(4,1);y=x;
xmin=min(Coorpt(:,1));
xmax=max(Coorpt(:,1));
ymin=min(Coorpt(:,2));
ymax=max(Coorpt(:,2));
nf=15;figure (nf);
xlim([xmin,xmax]);
ylim([ymin,ymax]);
hold on
for tri=1:Nbtri
    for nd=1:3
        x(nd)=Coorpt(Numtri(tri,nd),1);
        y(nd)=Coorpt(Numtri(tri,nd),2);
    end
    x(4)=x(1); y(4)=y(1);
    if  Reftri(tri)==1 
       color='red';
    end
    if  Reftri(tri)==2 
       color='white';
    end
    patch(x,y,color);
end
fs=18;title('Finite element (coarse) mesh','FontSize',fs);