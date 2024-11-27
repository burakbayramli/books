
#' Element stiffness matrix (for a spring)
#'
#'This function generates the matrix for a spring element.
#'The axis of the spring is assumed to coicide with the global x-axis.
#'
#' @param stiffk Spring stiffness k in N/m
#' @return Stiffness matrix of a spring element
#' @export
LinearSpring_Element_Matrix = function(DOF=2,stiffk)
{
  ematrix=stiffk*matrix(c(1,-1,-1,1),nrow = DOF,byrow = T)
  return(ematrix)
}


#' Expanded stiffness matrix (for a spring)
#'
#'This function generates the expanded matrix for each element in a connected system
#'of springs
#'
#' @param TDOF Total degree of freedom in a connected system of springs
#' @param eMatrix Pre-expanded matrix of a specific spring element
#' @param i Index of the first node
#' @param j Index of the second node
#'
#' @return The expanded matrix (conforming to the total degree of freedom of the system)
#' @export
LinearSpring_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{
  bigMatrix = matrix(vector(l = TDOF * TDOF),nrow = TDOF,byrow = T);
  bigMatrix[c(i,j),c(i,j)] = eMatrix;
  return (bigMatrix)
}


#' Reduced stiffness matrix
#'
#' This function computes the reduced stiffness matrix
#' from the global stiffness matrix using the boundary condition of the system.
#'
#' @param bigKmatrix Global stiffness matrix
#' @param knownloadnodes Vector of nodal index of known loads
#'                                       in the form c(i,j,k)
#'
#' @return Reduced stiffness matrix
#' @export
#'
ReducedStiffnessMatrix = function(bigKmatrix,knownloadnodes)
{
  reducedk = bigKmatrix[c(knownloadnodes),(knownloadnodes)]
  return(reducedk)
}


#' Reduced force vector (a column matrix of known force)
#'
#' @param forcevector Vector of known loads (be consistent with units)
#'
#' @return Reduced force vector
#' @export
#'
ReducedLoadVector = function(forcevector)
{
  reducedforcevector = matrix(forcevector,ncol = 1)
  return(reducedforcevector)
}

#' Global nodal displacements
#'
#' @param reducedmatrix Reduced stiffness matrix
#' @param reducedforcevec Reduced force vector
#'
#' @return Global nodal displacements, that is:
#'                      extensions (spring and bars);
#'                      vertical displacements and slopes (beams);
#'                      vertical/horizontal displacements (2D traingle);
#'                      radial and vertical displacements (2D axisymmetric);
#'                      Be mindful of units.
#'
#'
#' @export

NodalDisplacement = function(reducedmatrix,reducedforcevec)
{
  return(solve(reducedmatrix,reducedforcevec))
}


#' Local element forces (spring)
#'
#' @param DOF Degree of freedom for a spring element (2 by default)
#' @param stiffk Spring stiffness k in N/m
#' @param vec_globalnodaldisp Vector of global displacements
#' @param i Index of the first node
#' @param j Index of the second node
#'
#' @return Local nodal forces (spring)
#' @export
#'
LinearSpring_Element_Forces = function(DOF=2,stiffk,vec_globalnodaldisp,
                                       i,j)
{
  nodaldisplacment=vec_globalnodaldisp[c(i,j)]
  stiffnessmatrix = stiffk * matrix(c(1,-1,-1,1),nrow = DOF,byrow = T)
  nodal_forces = stiffnessmatrix %*% nodaldisplacment
  return(nodal_forces)
}


#' Global element forces (spring)
#'
#' @param bigKmatrix Global stiffness matrix
#' @param vec_globalnodaldisp Global nodal displacements
#'
#' @return Global nodal forces (spring)
#' @export
#'
LinearSpring_Global_Forces = function(bigKmatrix,vec_globalnodaldisp)
{
  global_forces = bigKmatrix %*% vec_globalnodaldisp
  return(round(global_forces))
}



