
ke=zeros(8,8);
DN=zeros(2,8);
element_nodes=[0 1; 0 .5; 1 .5; 1 1; 0 .75; .5 .5; 1 .75; .5 1];
[gauss_points, gauss_weights]=GetQuadGauss(3,3);
n_gauss_points=size(gauss_points,1);
[N,Nx,Ny]=CompNDNatPointsQuad8(gauss_points(:,1), gauss_points(:,2));
for g=1:n_gauss_points
    J=CompJacobian2DatPoint(element_nodes, Nx(:,g), Ny(:,g));
    detJ=det(J);
    Jinv=inv(J);
    DN(1,:)=Nx(:,g);
    DN(2,:)=Ny(:,g);
    ke=ke+DN'*Jinv'*Jinv*DN*detJ*gauss_weights(g);
end
ke