function [l]=fmultLattice(lattice,DP)


[void nAircraft]=size(DP);


l.COLLOC=sparse(lattice.COLLOC);
l.N=lattice.N;
l.XYZ=lattice.XYZ;
l.VORTEX=lattice.VORTEX;


for i=1:nAircraft
dx=DP(1,i);
dy=DP(2,i);
dz=DP(3,i);


l2(:,1)=lattice.COLLOC(:,1)+dx;
l2(:,2)=lattice.COLLOC(:,2)+dy;
l2(:,3)=lattice.COLLOC(:,3)+dz;

XYZ2(:,:,1)=lattice.XYZ(:,:,1)+dx;
XYZ2(:,:,2)=lattice.XYZ(:,:,2)+dy;
XYZ2(:,:,3)=lattice.XYZ(:,:,3)+dz;

V2(:,:,1)=lattice.VORTEX(:,:,1)+dx;
V2(:,:,2)=lattice.VORTEX(:,:,2)+dy;
V2(:,:,3)=lattice.VORTEX(:,:,3)+dz;

l.COLLOC=[l.COLLOC;l2];
l.N=[l.N;lattice.N];
l.XYZ=[l.XYZ;XYZ2];
l.VORTEX=[l.VORTEX;V2];


end