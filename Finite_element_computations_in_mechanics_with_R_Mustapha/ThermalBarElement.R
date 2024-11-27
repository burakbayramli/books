

#' Element stiffness matrix (Bar element for thermal analysis)
#'
#' This function generates the 2 by 2 stiffness matrix for
#' a thermal analysis involving bar elements.
#'
#' @param DOF       Degree of freedom.
#' @param stiffbar  Young's modulus.
#'
#' @return         Stiffness matrix of a thermal bar element.
#' @export
ThermalBar_Element_Matrix=function(DOF=2,stiffbar)
{
  ematrix=stiffbar*matrix(c(1,-1,-1,1),nrow=DOF,byrow=T);
  return (ematrix)
}


#' Thermal body force (thermal bar element)
#'
#' This function generates a vector of body force
#' for a bar element in a thermal analysis.
#'
#' @param DOF         Degree of freedom.
#' @param YoungMod    Young's modulus.
#' @param area        Cross section area of a bar element.
#' @param alpha       Thermal conductivity.
#' @param tempchange  Temperature change.
#'
#' @return           A vector of thermal body force for a bar element.
#' @export
ThermalBar_BF= function(DOF=2,YoungMod,area,alpha,tempchange)
{
  nodalthermal=area*alpha*tempchange*YoungMod
  equivalentbodyload=matrix(c(-nodalthermal,nodalthermal),nrow=DOF,byrow=T)
  return (equivalentbodyload)
}

#' Expanded vector of thermal body force (thermal bar element)
#'
#' This function generates the expanded vector of body force
#' for a bar element in a thermal analysis.
#'
#' @param TDOF                Total degree of freedom in a connected system of bars.
#' @param LoadColumnMatrix    Vector of body force returned by ThermalBar_BF()
#' @param i                   Index of the first node.
#' @param j                   Index of the second node.
#'
#' @return                    The expanded vector of a bar element.
#' @export
#'
ThermalBar_ExpandedBF = function(TDOF,LoadColumnMatrix,i,j)
{

  bigColumnMatrix=matrix(vector(l=TDOF),nrow=TDOF,byrow=T);
  bigColumnMatrix[c(i,j)]=LoadColumnMatrix;
  return (bigColumnMatrix)
}


#' Expanded stiffness matrix (thermal bar element)
#'
#' This function generates the expanded stiffness matrix
#'                                     for a bar element.
#'
#' @param TDOF      Total degree of freedom in a
#'                      connected system of bars.
#'
#' @param eMatrix   The 2 by 2 stiffness matrix of a
#'                          specific bar element.
#'
#' @param i         Index of the first node.
#' @param j         Index of the second node.
#'
#' @return          The expanded stiffness matrix of a
#'                                 thermal bar element.
#' @export

ThermalBar_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{
  bigMatrix = matrix(vector(l = TDOF * TDOF),nrow = TDOF,byrow = T);
  bigMatrix[c(i,j),c(i,j)] = eMatrix;
  return (bigMatrix)
}



#' Local nodal forces (thermal bar element)
#'
#' This function generates the nodal forces for a bar element
#'                                        in a thermal analysis.
#'
#' @param DOF                Degree of freedom.
#' @param stiffbar           Stiffness of a bar element.
#' @param nodaldisp          Element nodal displacement
#'
#' @return                  A vector of the nodal forces for
#'                          a bar element in a thermal analysis.
#' @export
#'
ThermalBar_Element_Forces=function(DOF=2,stiffbar,nodaldisp)
{
  stiffnessmatrix=stiffbar*matrix(c(1,-1,-1,1),nrow=DOF,byrow=T)
  nodal_forces=stiffnessmatrix %*% matrix(nodaldisp,ncol=1)
  return(nodal_forces)
}


#' Global nodal forces (thermal bar element)
#'
#' This function generates the global nodal forces for a bar element
#'                                        in a thermal analysis.
#'
#' @param bigKmatrix              Global stiffness matrix.
#' @param vec_globalnodaldisp     Vector of all global nodal displacements.
#'
#' @return                        Vector of global nodal forces
#'                                (thermal bar element).
#' @export
#'
ThermalBar_Global_Forces = function(bigKmatrix,vec_globalnodaldisp)
{
  global_forces = bigKmatrix %*% vec_globalnodaldisp
  return(round(global_forces))
}



#' Axial Stress (thermal bar element)
#'
#' This function generates the axial stress in a bar element
#' in a thermal analysis.
#'
#' @param YoungMod               Young's modulus.
#' @param alpha                  Thermal conductivity.
#' @param tempchange             Temperature change.
#' @param Length                 Length of a bar element.
#' @param vec_globalnodaldisp    Vector of all global nodal displacements.
#' @param i                      Index of the first node.
#' @param j                      Index of the second node.
#'
#' @return                       A vector of thermal stress in a bar element.
#' @export
ThermalBar_Stresses = function(YoungMod,alpha,tempchange,Length,vec_globalnodaldisp,i,j)
{
  r1=i; r2=j
  BMatrix=matrix(c(-1/Length,1/Length),nrow=1,byrow=T);
  localstresses=(YoungMod%*%BMatrix%*%vec_globalnodaldisp[c(r1,r2)])-(YoungMod*alpha*tempchange)
  return(localstresses)
}

