function[lattice]=wingrotation2(wingno,geo,lattice,Raxle,hinge_pos,alpha)
%%%
% This function is a part of TORNADO, the vortex lattice method.
%
% wingrotation2 rotates the lattice of a wing according to user input.
% This might be used if the incidence of an entire wing changes, as would
% be the case with tailerons, all-moving canards, and other suchlike.
%
% WINGROTATION2(WINGNO,GEO,LATTICE,RAXLE,HINGE_POS,ALPHA) rotates wing
% number WINGNO, ALPHA radians around the rotaion axis RAXLE, with the
% origin in HINGE_POS.
%
% GEO and LATTICE are the standard Tornado geometry data.
%
% Output is the new, rotated lattice to be used en the tornado solver.
%
%Example:
% 
%   [lattice,ref]=fLattice_setup(geo,state);
%   geometryplot(lattice,geo,ref);
% 
%  Calls:
%       None.
% 
%  Author: Tomas Melin <dr.tomas.melin@gmail.com>
%  Keywords: Tornado text based user interface
% 
%  Revision History:
%    Bristol,    2008 12 07:  Function created. T.M.
%   Spånga, 2021-09-19:   Updated to MATLAB R2020, TM  
%   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lemma1=((geo.nx+geo.fnx).*geo.ny);
lemma2=(geo.symetric+1)';

[a b]=size(lemma1);
for i=1:b
    npan_m1(:,i)=lemma1(:,i).*lemma2;
end

npan=sum(npan_m1,2);

if wingno==1;
    startindex=1;
    endindex=npan(1);
else
    startindex_m1=cumsum(npan);
    startindex=startindex_m1(wingno-1)+1;
    endindex=startindex_m1(wingno);
end


%%Move to rotation centre
l.V(:,:,1)=lattice.VORTEX(startindex:endindex,:,1)-hinge_pos(1);
l.V(:,:,2)=lattice.VORTEX(startindex:endindex,:,2)-hinge_pos(2);
l.V(:,:,3)=lattice.VORTEX(startindex:endindex,:,3)-hinge_pos(3);

l.C(:,1)=lattice.COLLOC(startindex:endindex,1)-hinge_pos(1);
l.C(:,2)=lattice.COLLOC(startindex:endindex,2)-hinge_pos(2);
l.C(:,3)=lattice.COLLOC(startindex:endindex,3)-hinge_pos(3);

l.XYZ(:,:,1)=lattice.XYZ(startindex:endindex,:,1)-hinge_pos(1);
l.XYZ(:,:,2)=lattice.XYZ(startindex:endindex,:,2)-hinge_pos(2);
l.XYZ(:,:,3)=lattice.XYZ(startindex:endindex,:,3)-hinge_pos(3);

l.N(:,:,:)=lattice.N(startindex:endindex,:,:);


[ai bi ci]=size(l.V);

l2.C=trot4(Raxle,l.C,alpha)';
l2.N=trot4(Raxle,l.N,alpha)';

for i=2:(bi-1)
    for j=1:ai;
        A(j,:)=l.V(j,i,:);
    end
    
    
    B=trot4(Raxle,A,alpha)';
    l2.V(:,i,:)=B;
end

    l2.V(:,1,:)=l.V(:,1,:);
    l2.V(:,bi,:)=l.V(:,bi,:);
    
    
for i=1:5
    for j=1:ai;
        A(j,:)=l.XYZ(j,i,:);
    end
    
    B=trot4(Raxle,A,alpha)';
    l2.XYZ(:,i,:)=B;
end

%%Move back from rotation centre
l2.V(:,:,1)=l2.V(:,:,1)+hinge_pos(1);
l2.V(:,:,2)=l2.V(:,:,2)+hinge_pos(2);
l2.V(:,:,3)=l2.V(:,:,3)+hinge_pos(3);

l2.C(:,1)=l2.C(:,1)+hinge_pos(1);
l2.C(:,2)=l2.C(:,2)+hinge_pos(2);
l2.C(:,3)=l2.C(:,3)+hinge_pos(3);

l2.XYZ(:,:,1)=l2.XYZ(:,:,1)+hinge_pos(1);
l2.XYZ(:,:,2)=l2.XYZ(:,:,2)+hinge_pos(2);
l2.XYZ(:,:,3)=l2.XYZ(:,:,3)+hinge_pos(3);


lattice.VORTEX(startindex:endindex,:,:)=l2.V;
lattice.COLLOC(startindex:endindex,:,:)=l2.C;
lattice.XYZ(startindex:endindex,:,:)=l2.XYZ;
lattice.N(startindex:endindex,:,:)=l2.N;

end%function wingrotation
function[p2]=trot4(hinge,p,alpha)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TROT: Auxillary rotation function			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rotates point p around hinge alpha rads.%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ref: 	Råde, Westergren, BETA 4th ed,   
%			studentlitteratur, 1998			    	
%			pp:107-108							   	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: 	Tomas Melin, KTH,Department of%
% 				aeronautics, Copyright 2000	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Context:	Auxillary function for			
%				TORNADO.								
% Called by: setrudder, normals			
% Calls:		norm (MATLAB std fcn)			
%				sin			"						
%				cos			"						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELP:		Hinge=vector around rotation  
%						takes place.				
%				p=point to be rotated			
%				alpha=radians of rotation		
%				3D-workspace						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
a=hinge(1);
b=hinge(2);
c=hinge(3);

rho=sqrt(a^2+b^2);
r=sqrt(a^2+b^2+c^2);

if r==0
   cost=0
   sint=1;
else
   cost=c/r;
   sint=rho/r;
end

if rho==0
   cosf=0;
   sinf=1;
else
   cosf=a/rho;
	sinf=b/rho;
end   

cosa=cos(alpha);
sina=sin(alpha);

RZF=[[cosf -sinf 0];[sinf cosf 0];[0 0 1]];
RYT=[[cost 0 sint];[0 1 0];[-sint 0 cost]];
RZA=[[cosa -sina 0];[sina cosa 0];[0 0 1]];
RYMT=[[cost 0 -sint];[0 1 0];[sint 0 cost]];
RZMF=[[cosf sinf 0];[-sinf cosf 0];[0 0 1]];

P=RZF*RYT*RZA*RYMT*RZMF;
p2=P*p';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end%function wingrotation