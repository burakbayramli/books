% test new assembly
clear all
close all
format compact
%
% a cantilever beam w rectangular cross section
w = 0.03
h = 0.06
pois = 0.3
A = w*h;
E = 1e11
G = E/(2*(1+pois))
Iy = w*h^3/12
Iz = h*w^3/12
Ip = 0.5*Iy

% beam geometry
xyze = [1 1 1];
l  = norm(xyze);

% elements
nel = 3;
elNodes = zeros(nel,2);
% this is for elements along a single line
elNodes(:,1) = 1:nel;
elNodes(:,2) = 2:nel+1;

% and coordinates
nodeCoord = (0:1:nel)'/(nel)*xyze;
nNodes    = nel+1;

xxx = [0 -1 0];
% need a local z-axis since only x direction is defined
locz = cross(xxx,xyze);
ll   = norm(locz);
% same for all ele along the line
locz = ones(nel,1)*locz/ll;

% assemble elements
stiffness=...
    formStiffness3DframeJO(elNodes,locz,nodeCoord,...
        E*ones(1,nel),A*ones(1,nel),Iz*ones(1,nel),Iy*ones(1,nel),G*ones(1,nel),Ip*ones(1,nel));
    
% apply a force to node k
    nk = nel+1;
    f  = [0 1 1];
    plot3([nodeCoord(nk,1) nodeCoord(nk,1) + f(1)],...
    [nodeCoord(nk,2) nodeCoord(nk,2) + f(2)],...
    [nodeCoord(nk,3) nodeCoord(nk,3) + f(3)],'o-k');
    hold on
    
% set in RHS
    rhs = zeros(nel+1,6);
    rhs(nk,:) = [f zeros(1,3)]
    tmp = rhs';
    tmp = tmp(:);
% not the clamped DOFs (1--6)
    rhs = tmp(7:end);
% clamp first node (all 6 DOF)
    stiffnessc = stiffness(7:end,7:end);
    nsc  = size(stiffnessc,1);
    defl = stiffnessc\rhs;
    defl = [zeros(1,6);reshape(defl,6,nsc/6)'];
    deflxyz = defl(:,1:3); % we only plot xyz-deflections
    defn = sqrt(sum(deflxyz.^2,2));
    f = 0.9/max(defn);
%node coordinates plus deflections
    xyz = nodeCoord + f*deflxyz; 
    plot3(nodeCoord(:,1),nodeCoord(:,2),nodeCoord(:,3),'.-k','linewidth',2)
    hold on
    plot3(xyz(:,1),xyz(:,2),xyz(:,3),'.-r','linewidth',2)
    axis equal
    xlabel('X')
    ylabel('Y')
    zlabel('Z')