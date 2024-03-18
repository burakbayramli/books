function stiffness=...
    formStiffness3DframeJO(elNodes,locz,nodeCoord,E,A,Iz,Iy,G,J)
% use sparse to assemble sparse stiffness matrix
nEl = size(elNodes,1);
% computation of the system stiffness matrix
ilist = zeros(nEl,144);
jlist = zeros(nEl,144);
alist = zeros(nEl,144);
for e=1:nEl
    indx=elNodes(e,:) ;
    i1 = 6*indx(1)-[5:-1:0];
    i2 = 6*indx(2)-[5:-1:0];
    elDof = [i1 i2] ;
    jj = ones(12,1)*elDof;
    ii = jj';
    jj = jj(:);
    ii = ii(:); 
    xyz1 = nodeCoord(indx(1),:);
    xyz2 = nodeCoord(indx(2),:);
    L    = norm(xyz1-xyz2);
    % 
    k = elem_K_matJO(A(e),E(e),G(e),Iy(e),Iz(e),J(e),L);
    CXYZx = (xyz2-xyz1)/L;
    CXYZz = locz(e,:);
    CXYZy = [CXYZz(2)*CXYZx(3) - CXYZz(3)*CXYZx(2),...
             CXYZz(3)*CXYZx(1) - CXYZz(1)*CXYZx(3),...
             CXYZz(1)*CXYZx(2) - CXYZz(2)*CXYZx(1)];
    Lambda = [CXYZx ;CXYZy ;CXYZz]
    %108 8 Analysis of 3D frames
    R = [Lambda                  zeros(3,9);...
         zeros(3,3) Lambda       zeros(3,6);...
         zeros(3,6)       Lambda zeros(3,3);...
         zeros(3,9)              Lambda];
    tmp   = R'*k*R;
    ilist(e,:) = ii';
    jlist(e,:) = jj';
    alist(e,:) = tmp(:)';
end
stiffness = sparse(ilist(:),jlist(:),alist(:));