%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%================================
% Computes the RHS term F
% for a unitary resistance
% Resistances modelled as Gaussian functions
%  R(x,y)=1/2*exp(-r^2/2/R^2)
%  where r^2=(x-xr)^2=(y-yr)^2
%================================

function [F]=CalcF(XYres,Rr,Runit)

global Ns Nt XYs I123 Refs Reft

F=zeros(Ns,1);                   % initialization

%============ When the resistance is acting only in its triangle

% ir=Find_triang(XYres);             % triangle containing the resistance
% 
% floc=Aire(ir);
% F(I123(ir,:))=Runit*floc/3;            % source term for the vertices of the triangle

%==========================================================================

%============ Model of the resistance using a Gaussian function

%---------------integration using the barycenter

% for k=1:Nt                       % loop on triangles
%     M=XYs(I123(k,:)',:) ;        %  position of the vertex
%     Bary=sum(M)/3;
%     Fbary=Runit*Res_Gauss(Bary',XYres',Rr);
%    fact=Fbary*Aire(k)/3.;
%    for i=1:3                     % local number of the vertex
%       ig=I123(k,i);              % global number of the vertex
%       if(Refs(ig) == 0)          % internal vertex only
%           F(ig)=F(ig)+fact;
%       end
%    end
% end

%--------------- more precise integration

Mint=[2 1 1;1 2 1;1 1 2];

for k=1:Nt                       % loop on triangles
    M=XYs(I123(k,:)',:)' ;        %  position of the vertex
   FM=Runit*Res_Gauss(M,XYres',Rr);FM=FM'; % Gaussian values at the vertices
   fact=Aire(k)/12.;
   for i=1:3                     % local number of the vertex
      ig=I123(k,i);              % global number of the vertex
      if(Refs(ig) == 0)          % internal vertex only
          F(ig)=F(ig)+fact*Mint(i,:)*FM;
      end
   end
end
