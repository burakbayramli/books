function [gauss]=Gauss_parameters_2D(n,gauss)

% Parameters for Gauss integration rule in [-1,+1]
switch n
    case 1 % Gauss with 1 nodes in 1D
        csi_1D=[0];
        w_1D=[2];
    case 2 % Gauss with 2 nodes in 1D
        csi_1D=[-1/sqrt(3),+1/sqrt(3)];
        w_1D=[1,1];
    case 3 % Gauss with 3 nodes in 1D
        csi_1D=[-sqrt(3/5),0,+sqrt(3/5)];
        w_1D=[5/9,8/9,5/9];
    case 4 % Gauss with 4 nodes in 1D
        csi_1D=[-1/35*sqrt(525+70*sqrt(30)),-1/35*sqrt(525-70*sqrt(30)),1/35*sqrt(525-70*sqrt(30)),1/35*sqrt(525+70*sqrt(30))];
        w_1D=[1/36*(18-sqrt(30)),1/36*(18+sqrt(30)),1/36*(18+sqrt(30)),1/36*(18-sqrt(30))];
end

% Parameters for Gauss integration rule in [-1,+1]x[-1,+1]
csi_2D=[];
eta_2D=[];
for i=1:n
    csi_2D=[csi_2D,csi_1D];
    eta_2D=[eta_2D,csi_1D(i)*ones(1,n)];
end
for k=1:n^2
    w_i=find(csi_1D==csi_2D(k));
    w_j=find(csi_1D==eta_2D(k));
    w_2D(k)=w_1D(w_i)*w_1D(w_j);
end
for k=1:n^2
    gauss(k).csi=csi_2D(k);
    gauss(k).eta=eta_2D(k);
    gauss(k).w=w_2D(k);
end

end