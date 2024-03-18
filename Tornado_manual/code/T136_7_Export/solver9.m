function [results]=solver9(results,state,geo,lattice,ref)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1999, 2007 Tomas Melin
%
% This file is part of Tornado
%
% Tornado is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public
% License as published by the Free Software Foundation;
% either version 2, or (at your option) any later version.
%
% Tornado is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied
% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE.  See the GNU General Public License for more
% details.
%
% You should have received a copy of the GNU General Public
% License along with Tornado; see the file GNU GENERAL 
% PUBLIC LICENSE.TXT.  If not, write to the Free Software 
% Foundation, 59 Temple Place -Suite 330, Boston, MA
% 02111-1307, USA.
%
% usage: [RESULTS] = solver8(results,state,geo,lattice,ref)
%
% This function computes forces and moments on each panel.
% Inputs are coordinades for old resluts, collocationpoints, 
%   vorticies and Normals, reference area and chord
%
% Example:
%
%   [results]=solver8(results,state,geo,lattice,ref);
%
% Calls:
%           Setboundary    
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado core function
%
% Revision History:
%   Bristol,  2007 06 27:  Addition of new header. TM.
%   Spånga,   2021 12 03:  Added proppel inwash
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try
    PW1=propwash(lattice.prop,lattice.COLLOC);
catch
    PW1=zeros(size(lattice.N));
end

[a , ~]=size(PW1);

for i =1:a
    dwp(i)=PW1(i,:)*lattice.N(i,:)';
end






[a, vor_length , ~]=size(lattice.VORTEX);%extracting number of sections in 
										   %"horseshoes"
    %if vor_length < 8
    %   terror(1)
    %   return
    %end
    
 %tic   
[w2 , ~]=fastdw(lattice);
%T1=toc
%fExportLattice2( lattice ) ;  %Generate a textfile "lattice.txt"
%tic
%!downwash.exe
%T2=toc
%[w2 , ~]=DWreader();

results.dwcond=cond(w2);


%plot(a,T1,'+')
%hold on
%plot(a,T2,'o')

%fExportDownwash(w2);    %%Writing downwash to file for piping       
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting up right hand side %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rhs=(setboundary5(lattice,state,geo))';
%disp('rhs... ok')

[a2 , ~]=size(rhs);
for i=1:a2
    rhs_p(i,:)=dwp;
end
    
    
rhs=rhs+rhs_p;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Solving for rhs           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

gamma=w2\rhs';
%disp('gauss... ok')




    if state.pgcorr==1
        %tdisp('Trying PG correction')
        %Prandtl-Glauert compressibility correction
        [state.rho, sos , ~]=ISAtmosphere(state.ALT);
        M=state.AS/sos;
        corr=1/(sqrt(1-M^2));  
        gamma=gamma*corr;   
    end




b1=vor_length/2;

p1(:,:)=lattice.VORTEX(:,b1,:);             	%Calculating panel vortex midpoint	
p2(:,:)=lattice.VORTEX(:,b1+1,:);	%to use as a force locus
lattice.COLLOC(:,:)=(p1+p2)./2;	    % LOCAL control point, vortex midpoint.

c3=lattice.COLLOC-ones(size(lattice.COLLOC,1),1)*geo.ref_point;

[~, DW]=fastdw(lattice);	                    %Calculating downwash on vorticies
w4=sum(DW,2);					                %superpositioning aerodynamic influence

DWX=DW(:,:,1);
DWY=DW(:,:,2);
DWZ=DW(:,:,3);

[~, nofderiv]=size(gamma);
le=(p2-p1);					%Vortex span vector


%for s=1:a
%	Lle(s)=norm(le(s,:));			%length of vortex span vector or panel span
%	lehat(s,:)=le(s,:)./Lle(s);	%
%end 


Lle=sqrt(sum(le.^2,2));
lehat(:,1)=le(:,1)./Lle;
lehat(:,2)=le(:,2)./Lle;
lehat(:,3)=le(:,3)./Lle;


for j=1:nofderiv
    IW(:,j,1)=DWX*gamma(:,j);
    IW(:,j,2)=DWY*gamma(:,j);
    IW(:,j,3)=DWZ*gamma(:,j);
    
    G(:,1)=gamma(:,j).*lehat(:,1);	%Aligning vorticity along panel vortex
    G(:,2)=gamma(:,j).*lehat(:,2);
    G(:,3)=gamma(:,j).*lehat(:,3);

    wind1=state.AS*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]); %Aligning with wind

    for i=1:a
        Wind(i,:)=wind1-squeeze(IW(i,j,:))';
        Rot(i,:)=cross((lattice.COLLOC(i,:)-geo.CG),[state.P state.Q state.R]); %Calculating rotations
    end                                  
%% IMPORTANT PART
    Wind=Wind+Rot+PW1;						   %Adding rotations and propwash
    Fprim(:,j,:)=state.rho*cross(Wind,G);	    %Force per unit length
    
    
        F(:,j,1)=Fprim(:,j,1).*Lle;				%Force per panel
        F(:,j,2)=Fprim(:,j,2).*Lle;				%Force per panel
        F(:,j,3)=Fprim(:,j,3).*Lle;				%Force per panel
    

    C3(:,:,1)=c3(:,1)*ones(1,nofderiv);
    C3(:,:,2)=c3(:,2)*ones(1,nofderiv); 
    C3(:,:,3)=c3(:,3)*ones(1,nofderiv); 
        
end
results.F=F;
results.FORCE=sum(F,1);						%Total force
M=cross(C3,F,3);			                %Moments per panel
results.M=M;
results.MOMENTS=sum(M,1);					%Summing up moments	
results.gamma=gamma;

end%FUNCTION

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%
% NEW DOWNWASH FUNCTION
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5five
function[dw,DW]=fastdw(lattice)
one_by_four_pi=1/(4*pi);

[psize, vsize , ~]=size(lattice.VORTEX);


%disp('running right')
%psize=size(lattice.COLLOC,1);
lemma=ones(1,psize);

LDW=zeros(psize,psize,7,3);

mCOLLOC(:,:,1)=lattice.COLLOC(:,1)*lemma;
mCOLLOC(:,:,2)=lattice.COLLOC(:,2)*lemma;
mCOLLOC(:,:,3)=lattice.COLLOC(:,3)*lemma;

mN(:,:,1)=lattice.N(:,1)*lemma;
mN(:,:,2)=lattice.N(:,2)*lemma;
mN(:,:,3)=lattice.N(:,3)*lemma;

for j=1:(vsize-1)
    
    lr1(:,:,1)=(lattice.VORTEX(:,j,1)*lemma)';
    lr1(:,:,2)=(lattice.VORTEX(:,j,2)*lemma)';
    lr1(:,:,3)=(lattice.VORTEX(:,j,3)*lemma)';
    
    lr2(:,:,1)=(lattice.VORTEX(:,j+1,1)*lemma)';
    lr2(:,:,2)=(lattice.VORTEX(:,j+1,2)*lemma)';
    lr2(:,:,3)=(lattice.VORTEX(:,j+1,3)*lemma)'; 
    
    r1=lr1-mCOLLOC;
    r2=lr2-mCOLLOC;
    
    warning off
    LDW(:,:,j,:)=mega(r1,r2);
    warning on
end
LDW(find((isnan(LDW(:,:,:,:)))))=0;

DW=-squeeze(sum(LDW,3))*one_by_four_pi;

dw=sum(DW.*mN,3);
end


function[DW2]=mega(r1,r2)
%% First part
F1=cross(r1,r2,3);

LF1=(sum(F1.^2,3));



F2(:,:,1)=F1(:,:,1)./(LF1);
F2(:,:,2)=F1(:,:,2)./(LF1);
F2(:,:,3)=F1(:,:,3)./(LF1);
%clear('F1')


%% Next part

Lr1=sqrt(sum(r1.^2,3)); 
Lr2=sqrt(sum(r2.^2,3));


R1(:,:,1)=r1(:,:,1)./Lr1;
R1(:,:,2)=r1(:,:,2)./Lr1;
R1(:,:,3)=r1(:,:,3)./Lr1;

R2(:,:,1)=r2(:,:,1)./Lr2;
R2(:,:,2)=r2(:,:,2)./Lr2;
R2(:,:,3)=r2(:,:,3)./Lr2;



L1=(R2-R1);
%clear('R1','R2')



%% Third part
R0=(r2-r1);

radial_distance=sqrt((LF1./(sum(R0.^2,3))));


%% combinging 2 and 3
L2=  R0(:,:,1).*L1(:,:,1)...
    +R0(:,:,2).*L1(:,:,2)...
    +R0(:,:,3).*L1(:,:,3);



%% Downwash
DW(:,:,1)=F2(:,:,1).*L2;
DW(:,:,2)=F2(:,:,2).*L2;
DW(:,:,3)=F2(:,:,3).*L2;

near=config('near');

DW2(:,:,1)=DW(:,:,1).*(1-(radial_distance<near));
DW2(:,:,2)=DW(:,:,2).*(1-(radial_distance<near));
DW2(:,:,3)=DW(:,:,3).*(1-(radial_distance<near));


end