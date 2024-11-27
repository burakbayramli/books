
#' Element stiffness matrix (quadratic bar)
#'
#' This function generates the matrix for a quadratic bar element.
#' The axis of the bar element is assumed to coicide with the global x-axis.
#'
#' @param DOF Degree of freedom for a bar element (3 for a linear bar).
#' @param stiffbar Stiffness of the bar in the form of AE/L.
#'
#' @return The 3 by 3 stiffness matrix.
#' @export
#'
QuadraticBar_Element_Matrix = function(DOF = 3,stiffbar)
{
  ematrix = (1/3)*stiffbar*matrix(c(7,-8,1,-8,16,-8,1,-8,7),
                                        nrow = DOF,byrow =T);
  return(ematrix)
}



#' Expanded stiffness matrix (for the quadratic bar element)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of bars.
#'
#' @param TDOF Total degree of freedom in a connected system of bars.
#' @param eMatrix The 3 by 3 stiffness matrix of a specific quadratic bar element.
#' @param i Index of the first node.
#' @param j Index of the second node.
#' @param k Index of the third node.
#'
#' @return The expanded matrix (conforming to the total degree of freedom of the system).
#' @export
#'
QuadraticBar_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j,k)
{
  bigMatrix = matrix(vector(l = TDOF * TDOF),nrow = TDOF,byrow = T);
  bigMatrix[c(i,j,k),c(i,j,k)] = eMatrix;
  return (bigMatrix)
}


#' Local element forces (quadratic bar)
#'
#' This function generates the local forces at the nodes of
#' a quadratic bar element.
#'
#' @param DOF Degree of freedom.
#' @param stiff Stiffness of the bar in the form of AE/L.
#' @param vec_globalnodaldisp Vector of global displacements.
#' @param i Index of the first node.
#' @param j Index of the second node.
#' @param k Index of the third node.
#'
#' @return Local nodal forces (quadratic bar).
#' @export
#'
QuadraticBar_Element_Forces = function(DOF,stiff,vec_globalnodaldisp,i,j,k)
{
  nodaldisplacment=vec_globalnodaldisp[c(i,j,k)]
  stiffnessmatrix = (1/3)*stiff*matrix(c(7,-8,1,-8,16,-8,1,-8,7),nrow =
                                               DOF,byrow = T)
  nodal_forces = stiffnessmatrix %*% nodaldisplacment
  return(nodal_forces)
}


#' Global element forces (quadratic bar)
#'
#' This function generates the global forces at the nodes of a quadratic bar element.
#'
#' @param bigKmatrix Global stiffness matrix.
#' @param vec_globalnodaldisp Vector of global nodal displacements.
#'
#' @return Global nodal forces (quadratic bar)
#' @export
#'
QuadraticBar_Global_Forces = function(bigKmatrix,vec_globalnodaldisp)
{
  global_forces = bigKmatrix %*% vec_globalnodaldisp
  return(round(global_forces))
}


