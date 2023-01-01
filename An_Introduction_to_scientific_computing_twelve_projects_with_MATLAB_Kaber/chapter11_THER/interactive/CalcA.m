%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=============================================
% Computes the final matrix (size Ns*Ns)
% and the RHS term for the problem without resistances
% matrix computation using a loop on triangles
% elimination of Dirichlet boundary conditions
% K         stores the conductivities 
% Irefd     stores the labels of Dirichlet boundaries
% TD        stores the imposed temperatures
%===================================================
function [A,Fo]=CalcA(K,Irefd,TD,Ireft)

global Ns Nt XYs I123 Refs Reft

A=sparse(zeros(Ns,Ns));

for k=1:Nt                       % loop on triangles
   M=XYs(I123(k,:)',:)'; 
   Gi=[M(2,2)-M(2,3) M(2,3)-M(2,1) M(2,1)-M(2,2); 
    M(1,3)-M(1,2) M(1,1)-M(1,3) M(1,2)-M(1,1)]; 
   fact=1/(4*Aire(k))*K(find(Reft(k)==Ireft));
   for i=1:3                     % local number of the vertex
      ig=I123(k,i);              % global number of the vertex
      if(Refs(ig) == 0)          % internal vertex only
         for j=1:3
            jg=I123(k,j);
            A(ig,jg)=A(ig,jg)+fact*(Gi(:,i)'*Gi(:,j));
         end
      else
         A(ig,ig)=1;
      end
   end
end

% Elimination of Dirichlet boundary conditions

Te=zeros(Ns,1);                  % vector of imposed temperatures
for i=1:length(Irefd)
   clear v; v=find(Refs == Irefd(i));
   Te(v)=TD(i);
end
Fo=-A*Te;                        % RHS of the system
                                 % !! the imposed temperature has the wrong
                                 % sign

clear v; v=[];                 
for i=1:length(Irefd)
   v=[v; find(Refs == Irefd(i))]; %  Dirichlet vertices
end
 

for i=1:Ns                       % elimination of Dirichlet BC
   if Refs(i)==0                 % inside vertex
      A(i,v)=0;
    else
      Fo(i)=-Fo(i);              % correction of the imposed temperatures
   end
end

