

#' Element stiffness matrix (Dynamic beam)
#'
#' This function generates the 4 by 4 stiffness matrix of a beam element.
#'
#'
#' @param DOF         Degree of freedom.
#' @param YoungMod    Young's modulus.
#' @param MomentI     Second moment of area.
#' @param Length      Length.
#'
#' @return            Stiffness matrix of a dynamic bar element.
#' @export
DynamicEulerBeam_StiffnessMatrix=function(DOF = 4,YoungMod,MomentI,Length)
{
  L=Length;
  stiffness=YoungMod*MomentI/Length^3;
  coefficients=c(12,6*L,-12,6*L,
                 6*L,4*L^2,-6*L,2*L^2,
                 -12,-6*L,12,-6*L,
                 6*L,2*L^2,-6*L,4*L^2);
  ematrix=stiffness*matrix(coefficients,nrow=DOF,byrow=T);
  return(ematrix)
}



#' Element mass matrix (Dynamic beam)
#'
#' This function generates the 4 by 4 consistent mass matrix of a beam element.
#'
#' @param DOF         Degree of freedom.
#' @param Density     Material density
#' @param Area        Cross-sectional area
#' @param Length      Length.
#'
#' @return           Mass matrix of a dynamic beam element.
#' @export
DynamicEulerBeam_MassMatrix=function(DOF = 4,Density,Area,Length)
{

  L=Length;
  masscoeff=c(156,22*L,54,-13*L,
              22*L,4*L^2,13*L,-3*L^2,
              54,13*L,156,-22*L,
              -13*L,-3*L^2,-22*L,4*L^2);
  massmatrix=(Density*Area*Length/420)*matrix(masscoeff,nrow=DOF,byrow=T);
  return(massmatrix)
}



#' Expanded element matrix (dynamic beam)
#'
#' This function returns the expanded
#' stiffness/mass matrix of a dynamic beam element.
#'
#' @param TDOF        Total degree of freedom in a connected system of beams.
#' @param eMatrix     The 4 by 4 stiffness or mass matrix of a
#'                          specific beam element.
#' @param i           Index of the first node.
#' @param j           Index of the second node.
#'
#' @return            The expanded matrix of a bar element.
#' @export
DynamicEulerBeam_ExpandedMatrix = function(TDOF,eMatrix,i,j)
{
  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
  bigMatrix[c(r1,r2,r3,r4),c(r1,r2,r3,r4)]=eMatrix;
  return (bigMatrix)
}


#' Reduced stiffness matrix
#'
#' This a generic function to obtain the reducted stiffness/mass
#'  matrix after applying boundary condition on the global stiffness matrix.
#'
#' @param bigKmatrix        Global stiffness/mass matrix
#' @param knownloadnodes    Nodes of the applied zero load.
#'
#' @return                  Reduced stiffness/mass matrix.
#' @export
Dynamic_ReduceMatrix = function(bigKmatrix,knownloadnodes)
{
  reducedK = bigKmatrix[c(knownloadnodes),(knownloadnodes)]
  return(reducedK)
}



#' Natural frequencies (beam element)
#'
#' This function computes the natural frequencies, in radians per seconds.
#'
#' @param reducedM      Reduced mass matrix obtained by applying
#'                      boundary condition on the global mass matrix
#' @param reducedK      Reduced mass matrix obtained by applying
#'                      boundary condition on the global stiffness matrix
#'
#' @return              Natural frequencies in radian/s
#' @export
Dynamic_NaturalFrequencies=function(reducedM,reducedK)
{
  massinv=solve(reducedM);
  productMK=massinv%*%reducedK
  syseigen=eigen(productMK)
  sortedfreq=sort(syseigen$values)
  return(sqrt(sortedfreq))
}

