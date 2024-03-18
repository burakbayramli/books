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
index2=npan(wingno);                %End index for panels
wingpanels=(index2-index1+1);       %Number of panels on the wing to deform

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

def3=[0 0 0 0 0 0; def2];


%Ripping starboard wing
start=1;
INDX=[];
pans=(geo.nx+geo.fnx).*geo.ny;



for i=1:geo.nelem
       INDX=[INDX start:(pans(i)+start-1)]; %HÄR!!!!
       start=pans*2+1;
       
end

%for i=1:geo.nelem
%    pans(i)=(geo.nx(i)+geo.fnx(i))*geo.ny(i);    
%end


DC=lattice.COLLOC(INDX,:);          %INDX is wrong
N=lattice.N(INDX,:);
XYZ=lattice.XYZ(INDX,:,:);
V=lattice.VORTEX(INDX,:,:);




%%
for i =1:wingpanels/2    %Looping over panels;
    %Disp(i)
    %Computing rotations

    
    %Finding closest beam point
    for j=1:structure.nx
       R=pos(j,:)-squeeze(XYZ(i,1,:))';
       R2(j,:)=R;
       LR(j)=sqrt(sum(R.^2));         
    end  
    [I J]=(min(LR));   %Find closest beampoint
    
    M=rotmat(def3(J,4:6));                          %This is rotation matrix at this panels inner edge
    dR=M*R2(J,:)'-R2(J,:)';                         %Delta from rotation
    
    XYZ2(i,1,1)=XYZ(i,1,1)+def3(J,1)+dR(1);
    XYZ2(i,1,2)=XYZ(i,1,2)+def3(J,2)+dR(2);
    XYZ2(i,1,3)=XYZ(i,1,3)+def3(J,3)+dR(3);
    
    
    for j=1:structure.nx
       R=pos(j,:)-squeeze(XYZ(i,2,:))';
       R2(j,:)=R;
       LR(j)=sqrt(sum(R.^2));         
    end  
    [I J]=(min(LR));   %Find closest beampoint
    
    M=rotmat(def3(J,4:6));                          %This is rotation matrix at this panels inner edge
    dR=M*R2(J,:)'-R2(J,:)';                         %Delta from rotation
    
    XYZ2(i,2,1)=XYZ(i,2,1)+def3(J,1)+dR(1);
    XYZ2(i,2,2)=XYZ(i,2,2)+def3(J,2)+dR(2);
    XYZ2(i,2,3)=XYZ(i,2,3)+def3(J,3)+dR(3);
    
    
    
    
    for j=1:structure.nx
       R=pos(j,:)-squeeze(XYZ(i,3,:))';
       R2(j,:)=R;
       LR(j)=sqrt(sum(R.^2));         
    end  
    [I J]=(min(LR));   %Find closest beampoint
    
     M=rotmat(def3(J,4:6));                          %This is rotation matrix at this panels inner edge
    dR=M*R2(J,:)'-R2(J,:)';                         %Delta from rotation
    
    XYZ2(i,3,1)=XYZ(i,3,1)+def3(J,1)+dR(1);
    XYZ2(i,3,2)=XYZ(i,3,2)+def3(J,2)+dR(2);
    XYZ2(i,3,3)=XYZ(i,3,3)+def3(J,3)+dR(3);
    
    
    
    for j=1:structure.nx
       R=pos(j,:)-squeeze(XYZ(i,4,:))';
       R2(j,:)=R;
       LR(j)=sqrt(sum(R.^2));         
    end  
    [I J]=(min(LR));   %Find closest beampoint  
    
    M=rotmat(def3(J,4:6));                          %This is rotation matrix at this panels inner edge
    dR=M*R2(J,:)'-R2(J,:)';                         %Delta from rotation
    
    XYZ2(i,4,1)=XYZ(i,4,1)+def3(J,1)+dR(1);
    XYZ2(i,4,2)=XYZ(i,4,2)+def3(J,2)+dR(2);
    XYZ2(i,4,3)=XYZ(i,4,3)+def3(J,3)+dR(3);
    
    
    

    
   
    
end

 

%% Vortex and collocation points are set by panel corners
VORTEX(:,1,:)=lattice.VORTEX(1:npan/2,1,:);
VORTEX(:,2,:)=(3*XYZ2(:,1,:)+1*XYZ2(:,4,:))/4;
VORTEX(:,3,:)=(3*XYZ2(:,2,:)+1*XYZ2(:,3,:))/4;
VORTEX(:,4,:)=lattice.VORTEX(1:npan/2,1,:);

L1=(XYZ2(:,1,:)+3*XYZ2(:,4,:))./4;
L2=(XYZ2(:,2,:)+3*XYZ2(:,3,:))./4;
DC2=(L1+L2)./2;

NL=squeeze(DC2)+lattice.N(1:npan/2,:);

for i =1:wingpanels/2    %Looping over panels;
    
    
    %%Normals
    for j=1:structure.nx
       R=pos(j,:)-(NL(i,:));
       R2(j,:)=R;
       LR(j)=sqrt(sum(R.^2));         
    end  
    [I J]=(min(LR));   %Find closest beampoint  
    
    M=rotmat(def3(J,4:6));                          %This is rotation matrix at this panels inner edge
    dR=M*R2(J,:)'-R2(J,:)';                         %Delta from rotation
    
    N2(i,:)=NL(i,:)+def3(J,1:3)+dR';  
end
N3=N2-squeeze(DC2);






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




%end
XYZ2(:,5,:)=XYZ2(:,1,:);

%% Stitching port and starboard sides - caution! - new order from before!!
N4(:,1)=N3(:,1);
N4(:,2)=-N3(:,2);
N4(:,3)=N3(:,3);

N5=[N3; N2];

DC3(:,1)=DC2(:,1);
DC3(:,2)=-DC2(:,2);
DC3(:,3)=DC2(:,3);
DC4=[squeeze(DC2); DC3];

XYZ3(:,:,1)=XYZ2(:,:,1);
XYZ3(:,:,2)=-XYZ2(:,:,2);
XYZ3(:,:,3)=XYZ2(:,:,3);
XYZ4=cat(1,XYZ2,XYZ3);



V3(:,:,1)=VORTEX(:,:,1);
V3(:,:,2)=-VORTEX(:,:,2);
V3(:,:,3)=VORTEX(:,:,3);
V4=cat(1,VORTEX,V3);

% lattice.COLLOC(1:npan(1),:)=DC4;
% lattice.N(1:npan(1),:)=N4;
% lattice.VORTEX(1:npan(1),:,:)=V4(:,:,:);
% lattice.XYZ(1:npan(1),:,:)=XYZ4;

output.COLLOC=DC4;
output.VORTEX=V4;
output.N=N5;
output.XYZ=XYZ4;



%% splatt

end%function
% 
% 
% function[arm]=pointmomentarm(wingno,geo,lattice,structure)
% [a b]=size(geo.b);
% 
% lc=lattice.COLLOC;
% ln=lattice.N;
% xyz=lattice.XYZ;
% 
% [d e f]=size(lattice.VORTEX);
% if e==4
%     vp=lattice.VORTEX(:,2:3,:);    %vortex midpoint
% elseif e==8
%     vp=lattice.VORTEX(:,4:5,:);   %vortex midpoint
% end
% 
% sparfrac=sum(structure.spars(wingno,:))/2; %Average spar pos.
% 
% m=0;
% I1=0;
% I2=0;
% 
% parts=geo.nelem(wingno);
% for i=wingno:wingno                   %Per wing
%     for j=1:parts(i)    %Per partition
%         for k=1:(geo.ny(i,j)) %per stri
% 
%             I1=I2+1;
%             I2=I2+(geo.nx(i,j)+geo.fnx(i,j));
% 
%             LCI=squeeze(xyz(I2,4,:)-xyz(I1,1,:));  % Local chord inner
%             LSI=squeeze(xyz(I1,1,:))+LCI*sparfrac; % Local spar position
% 
%             LCO=squeeze(xyz(I2,3,:)-xyz(I1,2,:));  % Local chord outer
%             LSO=squeeze(xyz(I1,2,:))+LCO*sparfrac; % Local spar position
% 
%             LSM=(LSO+LSI)/2; % Local spar position midpoint
% 
% 
%             %h=plot3(LSM(1),LSM(2),LSM(3),'o');
%             %set(h,'LineWidth',3);
%             %hold on
% 
%             %h=plot3(LSO(1),LSO(2),LSO(3),'gd');
%             %set(h,'LineWidth',3);
% 
%             %h=plot3(LSI(1),LSI(2),LSI(3),'r^');
%             %set(h,'LineWidth',3);
% 
% 
%             arm.xyz(I1:I2,1,1)=squeeze(xyz(I1:I2,1,1))-LSI(1);
%             arm.xyz(I1:I2,1,2)=squeeze(xyz(I1:I2,1,2))-LSI(2);
%             arm.xyz(I1:I2,1,3)=squeeze(xyz(I1:I2,1,3))-LSI(3);
% 
%             arm.xyz(I1:I2,2,1)=squeeze(xyz(I1:I2,2,1))-LSO(1);
%             arm.xyz(I1:I2,2,2)=squeeze(xyz(I1:I2,2,2))-LSO(2);
%             arm.xyz(I1:I2,2,3)=squeeze(xyz(I1:I2,2,3))-LSO(3);
% 
%             arm.xyz(I1:I2,3,1)=squeeze(xyz(I1:I2,3,1))-LSO(1);
%             arm.xyz(I1:I2,3,2)=squeeze(xyz(I1:I2,3,2))-LSO(2);
%             arm.xyz(I1:I2,3,3)=squeeze(xyz(I1:I2,3,3))-LSO(3);
% 
%             arm.xyz(I1:I2,4,1)=squeeze(xyz(I1:I2,4,1))-LSI(1);
%             arm.xyz(I1:I2,4,2)=squeeze(xyz(I1:I2,4,2))-LSI(2);
%             arm.xyz(I1:I2,4,3)=squeeze(xyz(I1:I2,4,3))-LSI(3);
% 
%             arm.c(I1:I2,1)=lc(I1:I2,1)-LSM(1);
%             arm.c(I1:I2,2)=lc(I1:I2,2)-LSM(2);
%             arm.c(I1:I2,3)=lc(I1:I2,3)-LSM(3);
% 
% 
%             arm.n(I1:I2,1)=ln(I1:I2,1)-LSM(1);
%             arm.n(I1:I2,2)=ln(I1:I2,2)-LSM(2);
%             arm.n(I1:I2,3)=ln(I1:I2,3)-LSM(3);
% 
%             arm.v(I1:I2,1,1)=vp(I1:I2,1,1)-LSI(1);
%             arm.v(I1:I2,1,2)=vp(I1:I2,1,2)-LSI(2);
%             arm.v(I1:I2,1,3)=vp(I1:I2,1,3)-LSI(3);
% 
%             arm.v(I1:I2,2,1)=vp(I1:I2,2,1)-LSO(1);
%             arm.v(I1:I2,2,2)=vp(I1:I2,2,2)-LSO(2);
%             arm.v(I1:I2,2,3)=vp(I1:I2,2,3)-LSO(3);
% 
% 
% 
%         end
%     end
% end
% 
% end%function
% 
% function[R]=rotmat(r)
% 
% Rx=[1           0           0
%     0           cos(r(1))   -sin(r(1))
%     0           sin(r(1))   cos(r(1))];
% 
% Ry=[cos(r(2))   0           sin(r(2))
%     0           1           0
%     -sin(r(2))  0           cos(r(2))];
% 
% Rz=[cos(r(3))   -sin(r(3))  0
%     sin(r(3))   cos(r(3))   0
%     0           0           1];
% 
% 
% R=Rz*Ry*Rx;
% 
% end% function rotmat