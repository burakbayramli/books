function[output]=fLattransform(wingno,geo,lattice,fem,structure,mesh)
%
deflect=fem.def;
pos=mesh.GP_SB;



npan=cumsum(sum(((geo.nx+geo.fnx).*geo.ny),2).*(geo.symetric+1)');   %number of panels on each wing
if wingno==1
    index1=1;
else
    index1=npan(wingno-1)+1;        %Start index for panels
end
index2=npan(wingno);            %End index for panels
wingpanels=(index2-index1+1);     %number of panels on the wing to deform

%% Load indicies
[a b]=size(deflect);
n=a/6;              %Number of nodes
dx_index=1:6:(a-5);
dy_index=2:6:(a-4);
dz_index=3:6:(a-3);
dl_index=4:6:(a-2);
dm_index=5:6:(a-1);
dn_index=6:6:(a-0);
%%

def2(:,1)=deflect(dx_index);
def2(:,2)=deflect(dy_index);
def2(:,3)=deflect(dz_index);
def2(:,4)=deflect(dl_index);
def2(:,5)=deflect(dm_index);
def2(:,6)=deflect(dn_index);


DC=lattice.COLLOC(index1:index2,:);
N=lattice.N(index1:index2,:);
XYZ=lattice.XYZ(index1:index2,:,:);
V=lattice.VORTEX(index1:index2,:,:);
V2=V;



Dx=sqrt(pos(:,2).^2+pos(:,3).^2);
Dx=Dx./(max(Dx));
def3=[0 0 0 0 0 0;def2];



%%outer point
A1=(XYZ(:,2,2));
B1=(XYZ(:,2,3));
S1=sqrt(A1.^2+B1.^2);

T=max(S1);

Sp1=S1./T;

Sp1=Sp1.*sign(A1);
[I J]=find(Sp1>0);
Sp1=Sp1(I);           %Only positive side  (Starboard)
oDdef=interp1(Dx,def3,Sp1);



L.COLLOC=DC(I,:);
L.N=N(I,:);
L.XYZ=XYZ(I,:,:);
L.VORTEX=V(I,:,:);

arm=pointmomentarm(wingno,geo,L,structure);



%% Midpoint
A2=(DC(:,2));
B2=(DC(:,3));
S2=sqrt(A2.^2+B2.^2);       %spanwise coordinate
Sp2=S2./T;
Sp2=Sp2.*sign(A2);

Sp2=Sp2(I);           %Only positive side  (Starboard)




cDdef=interp1(Dx,def3,Sp2);

%%Innerpoint
A3=(XYZ(:,1,2));
B3=(XYZ(:,2,3));
S3=sqrt(A3.^2+B3.^2);        %spanwise coordinate
Sp3=S3./T;
Sp3=Sp3.*sign(A3);
Sp3=Sp3(I);           %Only positive side  (Starboard)

iDdef=interp1(Dx,def3,Sp3);




DC=DC(I,:);
VV(:,:,:)=V(I,:,:);
XYZ2(:,:,:)=XYZ(I,:,:);
V=VV;
XYZ=XYZ2;
clear V2 XYZ2;




%%

[a b]=size(cDdef);
for i =1:a    %Looping over panels;
    %Disp(i)
    %Computing rotations

  


    RM=rotmat(cDdef(i,4:6));
    
    
    
    Rcdef=(RM*arm.c(i,:)'-arm.c(i,:)')'; %Collocation point rotation
    Rndef=(RM*arm.n(i,:)'-arm.n(i,:)')'; %Normal point rotation

    RI=rotmat(iDdef(i,4:6));         %This is rotation matrix at this panels inner edge
    RO=rotmat(oDdef(i,4:6));         %This is rotation matrix at this panels outer edge

    Rxyzdef(1,:)=(RI*squeeze(arm.xyz(i,1,:))-squeeze(arm.xyz(i,1,:)));  %PANEL ROTATION
    Rxyzdef(2,:)=(RO*squeeze(arm.xyz(i,2,:))-squeeze(arm.xyz(i,2,:)));  %Rotated vector minus original vector
    Rxyzdef(3,:)=(RO*squeeze(arm.xyz(i,3,:))-squeeze(arm.xyz(i,3,:)));  %Gives the delta due ro rotation.
    Rxyzdef(4,:)=(RI*squeeze(arm.xyz(i,4,:))-squeeze(arm.xyz(i,4,:)));

   
    
    
    RVdef(1,:)=  (RI*squeeze(arm.v(i,1,:))-squeeze(arm.v(i,1,:)));   %VORTEX ROTATION
    RVdef(2,:)=  (RO*squeeze(arm.v(i,2,:))-squeeze(arm.v(i,2,:)));   %Rotated vector minus original vector
    %Gives
    %the delta due ro rotation.

    %Translations and rotation
    %old value___vvv      Translation___vvv               rotation____vvv
    DC2(i,1)=  DC(i,1)   +cDdef(i,1)         +Rcdef(1);
    DC2(i,2)=  DC(i,2)   +cDdef(i,2)         +Rcdef(2);
    DC2(i,3)=  DC(i,3)   +cDdef(i,3)         +Rcdef(3);

    N2(i,:)= RM*N(i,:)';


    XYZ2(i,1,:)= squeeze(XYZ(i,1,:)) +iDdef(i,1:3)'                   +Rxyzdef(1,:)';
    XYZ2(i,2,:)= squeeze(XYZ(i,2,:)) +oDdef(i,1:3)'                   +Rxyzdef(2,:)';
    XYZ2(i,3,:)= squeeze(XYZ(i,3,:)) +oDdef(i,1:3)'                   +Rxyzdef(3,:)';
    XYZ2(i,4,:)= squeeze(XYZ(i,4,:)) +iDdef(i,1:3)'                   +Rxyzdef(4,:)';

    V2(i,2,:)=squeeze(V(i,2,:))      +iDdef(i,1:3)'                   +RVdef(1,:)';
    V2(i,3,:)=squeeze(V(i,3,:))      +oDdef(i,1:3)'                   +RVdef(2,:)';
end

XYZ2(:,5,:)=XYZ2(:,1,:);



%% Stitching port and starboard sides - caution! - new order from before!!
N3(:,1)=N2(:,1);
N3(:,2)=-N2(:,2);
N3(:,3)=N2(:,3);
N4=[N2; N3];

DC3(:,1)=DC2(:,1);
DC3(:,2)=-DC2(:,2);
DC3(:,3)=DC2(:,3);
DC4=[DC2; DC3];

XYZ3(:,:,1)=XYZ2(:,:,1);
XYZ3(:,:,2)=-XYZ2(:,:,2);
XYZ3(:,:,3)=XYZ2(:,:,3);
XYZ4=cat(1,XYZ2,XYZ3);


V2(:,1,:)=V(:,1,:);
V2(:,4,:)=V(:,4,:);
V3(:,:,1)=V2(:,:,1);
V3(:,:,2)=-V2(:,:,2);
V3(:,:,3)=V2(:,:,3);
V4=cat(1,V2,V3);


lattice.COLLOC(1:npan(1),:)=DC4;
lattice.N(1:npan(1),:)=N4;
lattice.VORTEX(1:npan(1),:,:)=V4(:,:,:);
lattice.XYZ(1:npan(1),:,:)=XYZ4;


%output.COLLOC=DC2;
%output.VORTEX=V2;
%output.N=N2;
%output.XYZ=XYZ2;

output=lattice;

%% splatt

end%function


function[arm]=pointmomentarm(wingno,geo,lattice,structure)
[a b]=size(geo.b);

lc=lattice.COLLOC;
ln=lattice.N;
xyz=lattice.XYZ;

[d e f]=size(lattice.VORTEX);
if e==4
    vp=lattice.VORTEX(:,2:3,:);    %vortex midpoint
elseif e==8
    vp=lattice.VORTEX(:,4:5,:);   %vortex midpoint
end

sparfrac=sum(structure.spars(wingno,:))/2; %Average spar pos.

m=0;
I1=0;
I2=0;

parts=geo.nelem(wingno);
for i=wingno:wingno                   %Per wing
    for j=1:parts(i)    %Per partition
        for k=1:(geo.ny(i,j)) %per stri

            I1=I2+1;
            I2=I2+(geo.nx(i,j)+geo.fnx(i,j));

            LCI=squeeze(xyz(I2,4,:)-xyz(I1,1,:));  % Local chord inner
            LSI=squeeze(xyz(I1,1,:))+LCI*sparfrac; % Local spar position

            LCO=squeeze(xyz(I2,3,:)-xyz(I1,2,:));  % Local chord outer
            LSO=squeeze(xyz(I1,2,:))+LCO*sparfrac; % Local spar position

            LSM=(LSO+LSI)/2; % Local spar position midpoint


            %h=plot3(LSM(1),LSM(2),LSM(3),'o');
            %set(h,'LineWidth',3);
            %hold on

            %h=plot3(LSO(1),LSO(2),LSO(3),'gd');
            %set(h,'LineWidth',3);

            %h=plot3(LSI(1),LSI(2),LSI(3),'r^');
            %set(h,'LineWidth',3);


            arm.xyz(I1:I2,1,1)=squeeze(xyz(I1:I2,1,1))-LSI(1);
            arm.xyz(I1:I2,1,2)=squeeze(xyz(I1:I2,1,2))-LSI(2);
            arm.xyz(I1:I2,1,3)=squeeze(xyz(I1:I2,1,3))-LSI(3);

            arm.xyz(I1:I2,2,1)=squeeze(xyz(I1:I2,2,1))-LSO(1);
            arm.xyz(I1:I2,2,2)=squeeze(xyz(I1:I2,2,2))-LSO(2);
            arm.xyz(I1:I2,2,3)=squeeze(xyz(I1:I2,2,3))-LSO(3);

            arm.xyz(I1:I2,3,1)=squeeze(xyz(I1:I2,3,1))-LSO(1);
            arm.xyz(I1:I2,3,2)=squeeze(xyz(I1:I2,3,2))-LSO(2);
            arm.xyz(I1:I2,3,3)=squeeze(xyz(I1:I2,3,3))-LSO(3);

            arm.xyz(I1:I2,4,1)=squeeze(xyz(I1:I2,4,1))-LSI(1);
            arm.xyz(I1:I2,4,2)=squeeze(xyz(I1:I2,4,2))-LSI(2);
            arm.xyz(I1:I2,4,3)=squeeze(xyz(I1:I2,4,3))-LSI(3);

            arm.c(I1:I2,1)=lc(I1:I2,1)-LSM(1);
            arm.c(I1:I2,2)=lc(I1:I2,2)-LSM(2);
            arm.c(I1:I2,3)=lc(I1:I2,3)-LSM(3);


            arm.n(I1:I2,1)=ln(I1:I2,1)-LSM(1);
            arm.n(I1:I2,2)=ln(I1:I2,2)-LSM(2);
            arm.n(I1:I2,3)=ln(I1:I2,3)-LSM(3);

            arm.v(I1:I2,1,1)=vp(I1:I2,1,1)-LSI(1);
            arm.v(I1:I2,1,2)=vp(I1:I2,1,2)-LSI(2);
            arm.v(I1:I2,1,3)=vp(I1:I2,1,3)-LSI(3);

            arm.v(I1:I2,2,1)=vp(I1:I2,2,1)-LSO(1);
            arm.v(I1:I2,2,2)=vp(I1:I2,2,2)-LSO(2);
            arm.v(I1:I2,2,3)=vp(I1:I2,2,3)-LSO(3);



        end
    end
end

end%function

function[R]=rotmat(r)

Rx=[1           0           0
    0           cos(r(1))   -sin(r(1))
    0           sin(r(1))   cos(r(1))];

Ry=[cos(r(2))   0           sin(r(2))
    0           1           0
    -sin(r(2))  0           cos(r(2))];

Rz=[cos(r(3))   -sin(r(3))  0
    sin(r(3))   cos(r(3))   0
    0           0           1];


R=Rz*Ry*Rx;

end% function rotmat