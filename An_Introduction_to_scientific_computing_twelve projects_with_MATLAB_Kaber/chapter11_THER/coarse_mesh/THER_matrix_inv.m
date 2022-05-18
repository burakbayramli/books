%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
    function [Aopt,bopt]=THER_matrix_inv(Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri,T0,Tres,Tcui); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [Aopt,bopt]=THER_matrix_inv(Nbpt,Nbtri,Coorpt,Refpt,Numtri,Reftri,T0,Tres,Tcui)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Construction of the linear system matrix (inverse problem)
%%   THER: Thermic engineering
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   
%%    
    Nopt=size(Tres,2);
    Aopt=zeros(Nopt,Nopt);
    bopt=zeros(Nopt,1); 
    Refcui = 1;
    for i=1:3 
       for j=1:3 
          AK(i,j)= 1/12.;
       end 
       AK(i,i)= 1/6.;  
    end    
%-> Do loop on triangles 
    for k=1:Nbtri 
%--->  Check triangle reference
       if Reftri(k) == Refcui  
%--->  Vertices coordinates
       M=[Coorpt(Numtri(k,1),:); Coorpt(Numtri(k,2),:); Coorpt(Numtri(k,3),:)]'; 
%--->  Determinant
       Delta=abs(det([M; 1 1 1])); 
%--->  Matrix construction 
       for i=1:Nopt 
       for j=1:Nopt 
          for ii=1:3 
          for jj=1:3 
             Aopt(i,j)=Aopt(i,j)+AK(ii,jj)*Delta*Tres(Numtri(k,ii),i)*Tres(Numtri(k,jj),j);
          end 
          end    
       end 
       end 
%--->  Right-hand side construction
       for i=1:Nopt 
          for ii=1:3 
          for jj=1:3 
             bopt(i)=bopt(i)+AK(ii,jj)*Delta*Tres(Numtri(k,ii),i)*(Tcui-T0(Numtri(k,jj)));
          end 
          end    
       end 
       end
   end 
