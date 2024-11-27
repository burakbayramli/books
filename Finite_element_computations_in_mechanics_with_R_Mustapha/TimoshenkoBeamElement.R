

#' Element stiffness matrix (Timoshenko beam)
#'
#' This function generates the 4 by 4 stiffness matrix for
#' a Timoshenko beam element.
#'
#' @param DOF            Degree of freedom.
#' @param YoungModulus  Young's modulus.
#' @param MomentInertia Moment of inertia.
#' @param ShearArea     Shear Area.
#' @param PoissonRatio  Poisson's ratio.
#' @param L             Length.
#'
#' @return              Stiffness matrix of a Timoshenko beam element.
#' @export
Timoshenko_Element_Matrix=function(DOF=4,YoungModulus,MomentInertia,ShearArea,PoissonRatio,L=1)
{
  shearmodulus=YoungModulus/(2*(1+PoissonRatio));
  ele_DOF=DOF;
  phi=(12*YoungModulus*MomentInertia)/(ShearArea*shearmodulus*L^2);
  flexuralstiff=YoungModulus*MomentInertia/(L^3);
  TBmatrix=flexuralstiff*matrix(c(12,6*L,-12,6*L,6*L,(4+phi)*L^2,-6*L,
                                  (2-phi)*L^2,-12,-6*L,12,-6*L,6*L,(2-phi)*L^2,
                                  -6*L,(4+phi)*L^2),nrow=ele_DOF,byrow=T);
  return(TBmatrix)

  }



#' Expanded stiffness matrix (Timoshenko)
#'
#' This function returns the expanded
#' stiffness matrix of a Timoshenko element.
#'
#' @param TDOF Total degree of freedom in a connected system of beams.
#' @param eMatrix The 4 by 4 stiffness matrix of a
#'                          specific beam element.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return The expanded matrix of a Timoshenko beam element.
#' @export
Timoshenko_ExpandedElement_Matrix = function(TDOF, eMatrix, i, j) {
  r1 = (i - 1) + i;
  r2 = (i - 1) + (i + 1);
  r3 = (j - 2) + (j + 1);
  r4 = (j - 2) + (j + 2);

  timobigMatrix = matrix(vector(l = TDOF * TDOF), nrow = TDOF, byrow = T);

  timobigMatrix[c(r1, r2, r3, r4), c(r1, r2, r3, r4)] = eMatrix;

  return (timobigMatrix)
}




#' Local forces and moments (Timoshenko)
#'
#' This function generates the nodal local forces
#'                    and moments for the element.
#'
#' @param DOF Degree of freedom.
#' @param YoungModulus Young's modulus.
#' @param MomentInertia Moment of inertia.
#' @param ShearArea     Shear Area.
#' @param PoissonRatio  Poisson's ratio.
#' @param Length        Length.
#' @param vec_globalnodaldisp Vector of global nodal displacements.
#' @param vec_distriload      Vector of equivalent loads, if any.
#' @param i             Index of the first node.
#' @param j             Index of the second node.
#'
#' @return              Local forces and moments (Timoshenko).
#' @export

Timoshenko_Local_ForcesMoments=function(DOF=4,YoungModulus,MomentInertia,
                                        ShearArea,
                                        PoissonRatio,Length,
                                        vec_globalnodaldisp,
                                        vec_distributedLoad,i,j)

{
  shearmodulus=YoungModulus/(2*(1+PoissonRatio));
  r1 = (i - 1) + i;
  r2 = (i - 1) + (i + 1);
  r3 = (j - 2) + (j + 1);
  r4 = (j - 2) + (j + 2);
  nodaldisp=vec_globalnodaldisp[c(r1,r2,r3,r4)];
  L=Length;ele_DOF=DOF;
  phi=(12*YoungModulus*MomentInertia)/(ShearArea*shearmodulus*L^2);
  flexuralstiff=YoungModulus*MomentInertia/(L^3);
  TBmatrix=flexuralstiff*matrix(c(12,6*L,-12,6*L,6*L,(4+phi)*L^2,-6*L,
                                  (2-phi)*L^2,-12,-6*L,12,-6*L,6*L,(2-phi)*L^2,
                                  -6*L,(4+phi)*L^2),nrow=ele_DOF,byrow=T)
  local_loads=TBmatrix %*% matrix(nodaldisp,ncol=1)-vec_distributedLoad
  return(local_loads)
}



#' Global nodal forces and moments
#'
#' This function generates the nodal global forces
#'                    and moments for a Timoshenko beam element.
#'
#' @param bigKmatrix Global stiffness matrix.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#'
#' @return Vector of global nodal forces and moments (Timoshenko beam element).
#' @export
Timoshenko_Global_ForcesMoments = function(bigKmatrix,vec_globalnodaldisp)
{
  columndof=matrix(vec_globalnodaldisp,byrow = T)
  global_forces = bigKmatrix %*% vec_globalnodaldisp
  return(round(global_forces))
}


