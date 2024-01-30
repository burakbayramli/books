function [stiffness] = ...
	 formStiffness3Dframe(GDof,numberElements, ...			      
			      elementNodes,numberNodes,nodeCoordinates,E,A,Iz,Iy,G,J)
  
  stiffness = zeros(GDof);
			  % computation of the system stiffness matrix
  for e = 1:numberElements
			% elementDof: element degrees of freedom (Dof)
    indice = elementNodes(e,:);
    elementDof = [6*indice(1)-5 6*indice(1)-4 6*indice(1)-3 6*indice(1)-2 6*indice(1)-1 6*indice(1) 6*indice(2)-5 6*indice(2)-4 6*indice(2)-3 6*indice(2)-2 6*indice(2)-1 6*indice(2)] ;
    x1 = nodeCoordinates(indice(1),1);
    y1 = nodeCoordinates(indice(1),2);
    z1 = nodeCoordinates(indice(1),3);
    x2 = nodeCoordinates(indice(2),1);
    y2 = nodeCoordinates(indice(2),2);
    z2 = nodeCoordinates(indice(2),3);
    L = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + ...
	       (z2-z1)*(z2-z1));
    k1 = E*A/L;
    k2 = 12*E*Iz/(L*L*L);
    k3 = 6*E*Iz/(L*L);
    k4 = 4*E*Iz/L;
    k5 = 2*E*Iz/L;
    k6 = 12*E*Iy/(L*L*L);
    k7 = 6*E*Iy/(L*L);
    k8 = 4*E*Iy/L;
    k9 = 2*E*Iy/L;
    k10 = G*J/L;
    k = [k1 0 0 0 0 0 -k1 0 0 0 0 0;
	 0 k2 0 0 0 k3 0 -k2 0 0 0 k3;
	 0 0 k6 0 -k7 0 0 0 -k6 0 -k7 0;
	 0 0 0 k10 0 0 0 0 0 -k10 0 0;
	 0 0 -k7 0 k8 0 0 0 k7 0 k9 0;
	 0 k3 0 0 0 k4 0 -k3 0 0 0 k5;
	 -k1 0 0 0 0 0 k1 0 0 0 0 0;
	 0 -k2 0 0 0 -k3 0 k2 0 0 0 -k3;
	 0 0 -k6 0 k7 0 0 0 k6 0 k7 0;
	 0 0 0 -k10 0 0 0 0 0 k10 0 0;
	 0 0 -k7 0 k9 0 0 0 k7 0 k8 0;
	 0 k3 0 0 0 k5 0 -k3 0 0 0 k4];
    if x1 == x2 && y1 == y2
      if z2 > z1
	Lambda = [0 0 1 ; 0 1 0 ; -1 0 0];
      else
	Lambda = [0 0 -1 ; 0 1 0 ; 1 0 0];
      end
    else
      CXx = (x2-x1)/L;
      CYx = (y2-y1)/L;
      CZx = (z2-z1)/L;
      D = sqrt(CXx*CXx + CYx*CYx);
      CXy = -CYx/D;
      CYy = CXx/D;
      CZy = 0;
      CXz = -CXx*CZx/D;
      CYz = -CYx*CZx/D;
      CZz = D;
      Lambda = [CXx CYx CZx ;CXy CYy CZy ;CXz CYz CZz];
    end
    R = [Lambda zeros(3,9); zeros(3) Lambda zeros(3,6);
	 zeros(3,6) Lambda zeros(3);zeros(3,9) Lambda];
    stiffness(elementDof,elementDof) = ...
    stiffness(elementDof,elementDof) + R'*k*R;
  end
end	     
