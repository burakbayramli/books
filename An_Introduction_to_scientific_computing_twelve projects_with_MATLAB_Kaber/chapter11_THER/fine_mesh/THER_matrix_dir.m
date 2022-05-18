%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [Atotal]=THER_matrix_dir(Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri,Conduc); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [Atotal]=THER_matrix_dir(Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri,Conduc)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Construction of the linear system matrix (direct problem)
%%   THER: Thermic engineering
%%   
%%   Input : Nbpt    number of mesh points 
%%           Nbtri   number of mesh triangles 
%%           Coorpt  triangles vertices coordinates
%%           Refpt   triangles vertices boundary references 
%%           Numtri  list of triangles vertices  
%%           Reftri  list of triangles references
%%           Conduc  heat conductivity coefficients 
%%
%%   Output : Atotal assembled matrix (direct problem)
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    
    Atotal=sparse(Nbpt,Nbpt); 
%-> Do loop on triangles 
    for k=1:Nbtri 
%--->  Vertices coordinates
       M=[Coorpt(Numtri(k,1),:); Coorpt(Numtri(k,2),:); Coorpt(Numtri(k,3),:)]'; 
%--->  Determinant
       Delta=abs(det([M; 1 1 1])); 
%--->  Gradient matrix
       Dp=[M(2,2)-M(2,3) M(2,3)-M(2,1) M(2,1)-M(2,2); 
       M(1,3)-M(1,2) M(1,1)-M(1,3) M(1,2)-M(1,1)];
       Dp=Dp/Delta; 
%--->  Matrix construction
       for i=1:3 
       for j=1:3 
          Atotal(Numtri(k,i),Numtri(k,j))=Atotal(Numtri(k,i),Numtri(k,j))+ ... 
                Conduc(Reftri(k))*Delta*(Dp(:,i)'*Dp(:,j))/2;                
       end 
       end    
   end 
%-> End do loop