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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%disp('Running solver 8')
[a vor_length void]=size(lattice.VORTEX);%extracting number of sections in
%"horseshoes"
%if vor_length < 8
%   terror(1)
%   return
%end

%flops(0)
% JO1702
w2 = jodwnwash(lattice.VORTEX,lattice.COLLOC,lattice.N);
% save('w2file','w2')
%[w21 void]=fastdw(lattice);
%disp('check dwnwash')
%norm(w2(:)-w21(:))
%w2
results.dwcond=condest(w2);

%fExportDownwash(w2);    %Writing downwash to file for piping

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting up right hand side %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rhs=(setboundary5(lattice,state,geo))';
%disp('rhs... ok')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Solving for rhs           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gamma=w2\rhs';
if state.pgcorr==1
    %tdisp('Trying PG correction')
    %Prandtl-Glauert compressibility correction
    [state.rho sos p_1]=ISAtmosphere(state.ALT);
    M=state.AS/sos;
    corr=1/sqrt(1-M^2);
    gamma=gamma*corr;
end

b1=vor_length/2;

p1(:,:)=lattice.VORTEX(:,b1,:);		%Calculating panel vortex midpoint
p2(:,:)=lattice.VORTEX(:,b1+1,:);	%to use as a force locus
c33 = lattice.COLLOC;
%lattice.COLLOC(:,:)=(p1+p2)./2;    % LOCAL control point, vortex midpoint.

c3=(p1+p2)/2-ones(size(lattice.COLLOC,1),1)*geo.ref_point;

%[w3 DW]=fastdw(lattice);	        %Calculating downwash on vorticies
%lattice.COLLOC = c33;
%w4=sum(DW,2);					    %superpositioning aerodynamic influence
% JO 1702
[DWX,DWY,DWZ]= jovel3(lattice.VORTEX,(p1+p2)/2);

% DWX=DW(:,:,1);
% DWY=DW(:,:,2);
% DWZ=DW(:,:,3);
% disp('check force ...')
 %norm(DWX(:) - Ax(:))
% norm(DWY(:) - Ay(:))
% norm(DWZ(:) - Az(:))
% DWX
% DWY
% DWZ
%JO190121
%'solver9'
%size(gamma)
[~,nofderiv]=size(gamma);
le=(p2-p1);					%Vortex span vector

Lle       =sqrt(sum(le.^2,2));
lehat(:,1)=le(:,1)./Lle;
lehat(:,2)=le(:,2)./Lle;
lehat(:,3)=le(:,3)./Lle;
CG2col = (p1+p2)/2-ones(a,1)*geo.CG;
pqr    = [state.P state.Q state.R];
wind1  = state.AS*([cos(state.alpha)*cos(state.betha) -cos(state.alpha)*sin(state.betha) sin(state.alpha)]); 
%Aligning with wind
    Rot = [CG2col(:,2)*pqr(3)-CG2col(:,3)*pqr(2), ...
     CG2col(:,3)*pqr(1)-CG2col(:,1)*pqr(3), ...
     CG2col(:,1)*pqr(2)-CG2col(:,2)*pqr(1)];
for j=1:nofderiv
    gam = gamma(:,j);
    IW(:,1)=DWX*gam;
    IW(:,2)=DWY*gam;
    IW(:,3)=DWZ*gam;
    
    G(:,1)=gam.*lehat(:,1);	%Aligning vorticity along panel vortex
    G(:,2)=gam.*lehat(:,2);
    G(:,3)=gam.*lehat(:,3);
   
    Wind=ones(a,1)*wind1-IW;
    
    Wind=Wind+Rot;								%Adding rotations
    Fprim(:,j,:)=state.rho*cross(Wind,G);	    %Force per unit length Kutta-Zhukowski
    
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
%
% JO 1702
% downwash coded fortran-style
function A = jodwnwash(vrtx,target,vect)
% compute downwash influence coeff. matrix A
% from vortices in vrtx to points in target with normals vect
[np,nleg,dum]=size(vrtx);
vrt1 = vrtx(:,:,1);
vrt2 = vrtx(:,:,2);
vrt3 = vrtx(:,:,3);
A    = zeros(np,np);
o4p = 1/(4*pi);
for ic = 1:np
    P = target(ic,:);
    N = vect(ic,:);
    a1 = vrt1(:,1:nleg-1)-P(1);
    a2 = vrt2(:,1:nleg-1)-P(2);
    a3 = vrt3(:,1:nleg-1)-P(3);
    b1 = vrt1(:,2:nleg  )-P(1);
    b2 = vrt2(:,2:nleg  )-P(2);
    b3 = vrt3(:,2:nleg  )-P(3);
    absa = sqrt(a1.^2+a2.^2+a3.^2);
    absb = sqrt(b1.^2+b2.^2+b3.^2);
    adotb = a1.*b1+a2.*b2+a3.*b3;
    acbdotn = (a2.*b3-a3.*b2)*N(1) +...
        (a3.*b1-a1.*b3)*N(2) + ...
        (a1.*b2-a2.*b1)*N(3);
    % formula like Drela, add epsilon to avoid NaN when denominator
    % vanishes
    tmp = sum(acbdotn.*(1./absa+1./absb)./(absa.*absb*(1+1e-12)+adotb),2)';
    %   tmp(isnan(tmp)) = 0;
    A(ic,:) = -tmp*o4p;
end
end
%
%
% JO 1702
function [Ax,Ay,Az] = jovel3(vrtx,target)
% compute downwash matrices from vortices to target points
[np,nleg,~]=size(vrtx);
vrt1 = vrtx(:,:,1);
vrt2 = vrtx(:,:,2);
vrt3 = vrtx(:,:,3);
Ax = zeros(np,np);
Ay = Ax;
Az = Ax;
o4p = 1/(4*pi);
for ic = 1:np
    P = target(ic,:);
    a1 = vrt1(:,1:nleg-1)-P(1);
    a2 = vrt2(:,1:nleg-1)-P(2);
    a3 = vrt3(:,1:nleg-1)-P(3);
    b1 = vrt1(:,2:nleg  )-P(1);
    b2 = vrt2(:,2:nleg  )-P(2);
    b3 = vrt3(:,2:nleg  )-P(3);
    absa = sqrt(a1.^2+a2.^2+a3.^2);
    absb = sqrt(b1.^2+b2.^2+b3.^2);
    adotb = a1.*b1+a2.*b2+a3.*b3;
    acb1 = (a2.*b3-a3.*b2);
    acb2 = (a3.*b1-a1.*b3);
    acb3 = (a1.*b2-a2.*b1);
    ttmp = (1./absa+1./absb)./(absa.*absb+adotb+1e-12);
% 3D downwash velocity influence coeff
    Ax(ic,:) = -sum(acb1.*ttmp,2)'*o4p;
    Ay(ic,:) = -sum(acb2.*ttmp,2)'*o4p;
    Az(ic,:) = -sum(acb3.*ttmp,2)'*o4p;
end
end
function[dw,DW]=fastdw(lattice)
% computes downwash matrix dw, DW from lattice
%==========================================================
% NEW DOWNWASH FUNCTION
% computes downwash matrix dw, DW from lattice
% calls:
%         mega
% Updates
% JOp 090611 - reduce memory rq't
%==========================================================
one_by_four_pi=1/(4*pi);

[psize vsize void]=size(lattice.VORTEX);

lemma=ones(1,psize);
% JOp 0906 - reduce memory
%LDW=zeros(psize,psize,7,3);

mCOLLOC(:,:,1)=lattice.COLLOC(:,1)*lemma;
mCOLLOC(:,:,2)=lattice.COLLOC(:,2)*lemma;
mCOLLOC(:,:,3)=lattice.COLLOC(:,3)*lemma;

mN(:,:,1)=lattice.N(:,1)*lemma;
mN(:,:,2)=lattice.N(:,2)*lemma;
mN(:,:,3)=lattice.N(:,3)*lemma;
% JOp 0906 -
sss = zeros(psize,psize,3);
near1 = config('near');
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
    % JO 090605 - introduce tmp
    megmex = 0;

    if megmex
        tmp = megamex(r1,r2,near1);
    else
        tmp = mega(r1,r2);
    end
    tmp(isnan(tmp))=0;
    sss = sss + tmp;
    %LDW(:,:,j,:)=tmp;
    warning on
end
%JOp 0906 - save memory
%LDW(find((isnan(LDW(:,:,:,:)))))=0;
%DW=-squeeze(sum(LDW,3))*one_by_four_pi;
DW = -sss*one_by_four_pi;

dw=sum(DW.*mN,3);
end

function DW2 = mega(r1,r2)
% vectorized subfunction for downwash
%==========================================================
% helper function for downwash matrix calculation
% input: r1, r2  collocation and vortex points
% output: DW2 - something
% calls: -
% loads: -
% saves: -
% Updates
%   JOp 090611
%=======================================================
[n1 n2 n3]=size(r1);
r1 = reshape(r1,n1*n2,n3);
r2 = reshape(r2,n1*n2,n3);
n1n2 = n1*n2;

% First part
%F1=cross(r1,r2,2);
F1 = [r1(:,2).*r2(:,3)-r1(:,3).*r2(:,2),...
    r1(:,3).*r2(:,1)-r1(:,1).*r2(:,3),...
    r1(:,1).*r2(:,2)-r1(:,2).*r2(:,1)];

LF1=sum(F1.*F1,2);

% Next part
Lr1=1./sqrt(sum(r1.*r1,2));
%(r1.').^2
R1(:,1)=r1(:,1).*Lr1;
R1(:,2)=r1(:,2).*Lr1;
R1(:,3)=r1(:,3).*Lr1;

Lr1=sqrt(sum(r2.*r2,2));
R2(:,1)=r2(:,1)./Lr1;
R2(:,2)=r2(:,2)./Lr1;
R2(:,3)=r2(:,3)./Lr1;

L1=R2-R1;

% Third part
R0=r2-r1;
radial_distance2=LF1./sum(R0.*R0,2);

% combining 2 and 3
near=config('near');
near2 = near*near;
% this seems very strange ... usually fixed by
% adding near to distance ...
L2 = sum(R0.*L1,2).*(1-(radial_distance2<near2));
% Downwash
tmp = F1.*((L2./LF1)*ones(1,3));
DW2 = reshape(tmp,n1,n2,n3);

end
