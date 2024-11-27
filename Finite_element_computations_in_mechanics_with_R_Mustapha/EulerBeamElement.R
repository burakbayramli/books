

#' Element stiffness matrix (Euler-Bernoulli Beam)
#'
#' This function generates the 4 by 4 stiffness matrix for
#' an Euler-Bernoulli beam element.
#'
#' @param DOF Degree of freedom (4 for a beam).
#' @param YoungMod Young modulus.
#' @param MomentI Principal moment of inertia of the cross-section.
#' @param Length Length
#'
#' @return Stiffness matrix of an Euler-Bernoulli beam element.
#' @export
EulerBeam_Element_Matrix=function(DOF = 4,YoungMod,MomentI,Length)
{

  L=Length;
  stiffness=YoungMod*MomentI/Length^3;
  coefficients=c(12,6*L,-12,6*L,6*L,4*L^2,-6*L,2*L^2,-12,
                 -6*L,12,-6*L,6*L,2*L^2,-6*L,4*L^2);
  ematrix=stiffness*matrix(coefficients,nrow=DOF,byrow=T);
  return(ematrix)
}


#' Expanded stiffness matrix (Euler-Bernoulli beam)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of beams.
#'
#' @param TDOF Total degree of freedom in a connected system of beams.
#' @param eMatrix The 4 by 4 stiffness matrix of a
#'                          specific beam element.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return The expanded matrix of an Euler-Bernoulli beam element.
#' @export
EulerBeam_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{

  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
  bigMatrix[c(r1,r2,r3,r4),c(r1,r2,r3,r4)]=eMatrix;
  return (bigMatrix)
}



#' Equivalent load vector for a UDL (beam element)
#'
#' Generates the load column matrix for an element with a
#'                             uniformly distributed load.
#'
#' @param DOF Degree of freedom (4 for a beam).
#' @param LoadMagnitude Magnitude of the UDL, e.g. q in N/m.
#' @param Length Length
#'
#' @return A column matrix of the equivalent nodal loads.
#' @export
BeamUDL_Matrix= function(DOF=4,LoadMagnitude,Length)
{
  L=Length;
  DistributedLoad=LoadMagnitude*
    matrix(c(-L/2,-(L^2)/12,-L/2,(L^2)/12),nrow=DOF,byrow=T)
  return (DistributedLoad)
}


#' Expanded vector of the equivalent load
#'
#' This function generates the expanded vector of the equivalent load
#'                                             (Euler-Bernoulli beam).
#'
#' @param TDOF Total degree of freedom.
#' @param LoadColumnMatrix The unexpanded vector of equivalent loads.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return Expanded vector (a column matrix) of equivalent loads.
#' @export
BeamUDL_ExpandedMatirx = function(TDOF,LoadColumnMatrix,i,j)
{
  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  bigColumnMatrix=matrix(vector(l=TDOF),nrow=TDOF,byrow=T);
  bigColumnMatrix[c(r1,r2,r3,r4)]=LoadColumnMatrix;
  return (bigColumnMatrix)
}



#' Global nodal forces and moments
#'
#' This function generates the nodal global forces
#'                    and moments for the element.
#'
#' @param bigKmatrix Global stiffness matrix.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#'
#' @return Vector of global nodal forces and moments.
#' @export
EulerBeam_Global_ForcesMoments = function(bigKmatrix,vec_globalnodaldisp)
{
  columndof=matrix(vec_globalnodaldisp,byrow = T)
  global_forces = bigKmatrix %*% vec_globalnodaldisp
  return(round(global_forces))
}


#' Local forces and moments (for an element)
#'
#' This function generates the nodal local forces
#'                    and moments for the element.
#'
#' @param YoungMod Young's modulus.
#' @param MomentI Principal moment of inertia.
#' @param Length Length of the element.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#' @param vec_distriload Vector of equivalent loads, if any.
#'
#' @return Vector of local forces and moments for an element.
#' @export
EulerBeam_Local_ForcesMoments = function(YoungMod,MomentI,Length,vec_globalnodaldisp,vec_distriload)
{
  L=Length;
  DOF=4;
  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  nodaldisp=vec_globalnodaldisp[c(r1,r2,r3,r4)];
  bendingstiff=YoungMod*MomentI/Length^3;
  stiffcoeff=c(12,6*L,-12,6*L,6*L,4*L^2,-6*L,2*L^2,-12,
               -6*L,12,-6*L,6*L,2*L^2,-6*L,4*L^2);
  ematrix=bendingstiff*matrix(stiffcoeff,nrow=DOF,byrow=T);
  local_loads = (ematrix %*%nodaldisp)-vec_distributedLoad;
  return(local_loads)
}


#' Bending stress (Euler-Bernoulli beam)
#'
#' This function computes the maximum
#'    and minimum bending stress for a beam element.
#'
#' @param YoungMod Young's modulus.
#' @param Length Length of an element.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#'
#' @return A vector of bending stresses.
#' @export
EulerBeamMaxMin_BendingStress = function(YoungMod,Length,vec_globalnodaldisp)
{

  disp_vec=matrix(vec_globalnodaldisp,nrow=length(vec_globalnodaldisp),byrow=T,ncol = 1);
  Tmatrix=matrix(c(-m,-n,m,n),nrow=1,byrow=T);
  local_stress=(YoungMod/elem_len)*Tmatrix%*%disp_vec;
  return(local_stress)
}

