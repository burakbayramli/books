

#' Element stiffness matrix (Dynamic bar)
#'
#' This function generates the 2 by 2 stiffness matrix of a bar element.
#'
#' @param DOF           Degree of freedom.
#' @param YoungMod      Young's modulus.
#' @param Area          Cross-sectional area
#' @param Length        Length.
#'
#' @return              Stiffness matrix of a dynamic bar element.
#' @export
DynamicBar_StiffnessMatrix=function(DOF=2,YoungMod,Area,Length)
{
  stiffbar=YoungMod*Area/Length
  ematrix=stiffbar*matrix(c(1,-1,-1,1),nrow=DOF,byrow=T);
  return (ematrix)
}

#' Element mass matrix (Dynamic bar)
#'
#' This function generates the 2 by 2 consistent
#'                 mass matrix of a beam element.
#'
#' @param DOF             Degree of freedom.
#' @param Density         Material density.
#' @param Area            Cross-sectional area.
#' @param Length          Length.
#'
#' @return                Stiffness matrix of a dynamic bar element.
#' @export
DynamicBar_MassMatrix=function(DOF=2,Density,Area,Length)
{
  massmatrix=(Density*Area*Length/6)*matrix(c(2,1,1,2),nrow=DOF,byrow=T);
  return (massmatrix)
}




#' Expanded element matrix (dynamic bar)
#'
#' This function returns the expanded
#' stiffness/mass matrix of a dynamic bar element.
#'
#' @param TDOF       Total degree of freedom in a connected system of bars.
#' @param eMatrix    The 2 by 2 stiffness or mass matrix of a
#'                          specific bar element.
#' @param i          Index of the first node.
#' @param j          Index of the second node.
#'
#' @return           The expanded matrix of a bar element.
#' @export

DynamicBar_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{
  bigMatrix = matrix(vector(l = TDOF * TDOF),nrow = TDOF,byrow = T);
  bigMatrix[c(i,j),c(i,j)] = eMatrix;
  return (bigMatrix)
}


ReducedK=function(bigKmatrix,knownforcenodes)
{
  reducedK=bigKmatrix[c(knownforcenodes),(knownforcenodes)];
  return(reducedK)
}

ReducedM=function(bigMmatrix,knownforcenodes)
{
  reducedM=bigMmatrix[c(knownforcenodes),(knownforcenodes)];
  return(reducedM)
}



#' Natural frequencies (bar element)
#'
#' This function computes the natural frequencies, in radians per seconds.
#'
#' @param reducedM  Reduced mass matrix obtained by applying
#'                  boundary condition on the global mass matrix
#' @param reducedK  Reduced mass matrix obtained by applying
#'                  boundary condition on the global stiffness matrix
#'
#' @return          Natural frequencies in radian/s
#' @export
DynamicBar_NaturalFrequencies=function(reducedM,reducedK)
{
  massinv=solve(reducedM);
  productMK=massinv%*%reducedK
  syseigen=eigen(productMK)
  sortedfreq=sort(syseigen$values)
  return(sqrt(sortedfreq))
}


