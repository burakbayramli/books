

#' Element stiffness matrix (for a linear bar)
#'
#' This function generates the matrix for a linear bar element.
#' The axis of the bar element is assumed to coicide with the global x-axis.
#'
#' @param DOF Degree of freedom for a bar element (2 for a linear bar).
#' @param stiffbar Stiffness of the bar in the form of AE/L.
#'
#' @return The 2 by 2 stiffness matrix.
#' @export
#'
Bar_Element_Matrix=function(DOF=2,stiffbar)
{
  ematrix=stiffbar*matrix(c(1,-1,-1,1),nrow=DOF,byrow=T);
  return (ematrix)
}



#' Expanded stiffness matrix (for the linear bar)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of bars.
#'
#' @param TDOF Total degree of freedom in a connected system of bars.
#' @param eMatrix The 2 by 2 stiffness matrix of a specific bar element.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return The expanded matrix (conforming to the total degree of freedom of the system).
#' @export
#'
Bar_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{
  bigMatrix = matrix(vector(l = TDOF * TDOF),nrow = TDOF,byrow = T);
  bigMatrix[c(i,j),c(i,j)] = eMatrix;
  return (bigMatrix)
}


#' Equivalent load vector for a UDL (bar element)
#'
#' Generates the load column matrix for an element with a
#'                             uniformly distributed load.
#'
#' @param DOF Degree of freedom (2 for a bar).
#' @param LoadMagnitude Magnitude of the UDL, e.g. q in N/m.
#' @param Length Length
#'
#' @return A column matrix of the equivalent nodal loads.
BarUDL_Matrix= function(DOF=2,LoadMagnitude,Length)
{
  L=Length;
  DistributedLoad=LoadMagnitude*
    matrix(c(-L/2,-L/2),nrow=DOF,byrow=T)
  return (DistributedLoad)
}


#' Expanded vector of the equivalent load
#'
#' This function generates the expanded 
#' vector of the equivalent load (bar).
#'
#' @param TDOF               Total degree of freedom.
#' @param LoadColumnMatrix   The unexpanded vector of equivalent 
#'                           loads.
#'                           
#' @param i                  Index of the first node.
#' @param j                  Index of the second node.
#'
#' @return                   Expanded vector (a column matrix) of 
#'                           equivalent loads.
#' @export
BarUDL_ExpandedMatirx = function(TDOF,LoadColumnMatrix,i,j)
{
  r1=i;r2=j;
  bigColumnMatrix=matrix(vector(l=TDOF),nrow=TDOF,byrow=T);
  bigColumnMatrix[c(r1,r2)]=LoadColumnMatrix;
  return (bigColumnMatrix)
}


#' Local element forces (linear bar)
#'
#'This function generates the local forces at the nodes of a bar element.
#'
#' @param DOF Degree of freedom.
#' @param stiffbar Stiffness of the bar in the form of AE/L.
#' @param vec_globalnodaldisp Vector of global displacements.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return Local nodal forces (linear bar).
#' @export
#'
Bar_Element_Forces=function(DOF=2,stiffbar,vec_globalnodaldisp,i,j)
{
  nodaldisplacment=vec_globalnodaldisp[c(i,j)]
  stiffnessmatrix=stiffbar*matrix(c(1,-1,-1,1),nrow=DOF,byrow=T)
  nodal_forces=stiffnessmatrix %*% nodaldisplacment
  return(nodal_forces)
}


#' Global element forces (linear bar)
#'
#' This function generates the global forces at the nodes of a bar element.
#'
#' @param bigKmatrix Global stiffness matrix.
#' @param vec_globalnodaldisp Vector of global nodal displacements.
#'
#' @return Global nodal forces (linear bar)
#' @export
#'
Bar_Global_Forces = function(bigKmatrix,vec_globalnodaldisp)
{
  global_forces = bigKmatrix %*% vec_globalnodaldisp
  return(round(global_forces))
}



