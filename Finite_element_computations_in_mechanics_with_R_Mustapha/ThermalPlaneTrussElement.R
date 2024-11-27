

#' Geometric length and orientation from xy coordinates (thermal plane truss)
#'
#' This function generates the geometric length  and orientation
#' from the nodal coordinates of an element.
#'
#' @param vec_nodalcoordinates Vector of nodal coordinates
#'                              in the form c(x1,y1,x2,y2).
#'
#' @return                      Vector of the geometric length and orientation angle.
#' @export
#'
PlaneTruss_LengthOrientation =function(vec_nodalcoordinates)
  {
  ysquared=(vec_nodalcoordinates[4]-vec_nodalcoordinates[2])^2
  xsquared=(vec_nodalcoordinates[3]-vec_nodalcoordinates[1])^2
  length_of_element=sqrt(ysquared+xsquared)
  ydiff=(vec_nodalcoordinates[4]-vec_nodalcoordinates[2])
  xdiff=(vec_nodalcoordinates[3]-vec_nodalcoordinates[1])
  orientation_of_element=(180/pi)*atan(ydiff/xdiff)
  return(round(c(length_of_element,orientation_of_element),3))

}




#' Element stiffness matrix (thermal plane truss)
#'
#' This function generates the 4 by 4 stiffness matrix for
#' a plane truss elements in a thermal analysis.
#'
#' @param DOF              Degree of freedom (4 for a truss).
#' @param stifftruss       Axial stiffness of an element (N/m, i.e EA/L).
#' @param theta            Angular orientation of a truss element.
#'
#' @return                 Stiffness matrix of a plane truss element
#'                         for thermal analysis.
#' @export
#'
ThermalPlaneTruss_Element_Matrix=function(DOF = 4,stifftruss,theta)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  stiffnesscoefficients=c(m^2,m*n,-m^2,-m*n,m*n,n^2,
                          -m*n,-n^2,-m^2,-m*n,m^2,m*n,-m*n,-n^2,m*n,n^2)
  ematrix=stifftruss*matrix(stiffnesscoefficients,nrow=DOF,byrow=T)
}




#' Thermal body force (thermal plane truss element)
#'
#' This function generates a vector of the body force
#' for a plane truss element in a thermal analysis.
#'
#' @param DOF             Degree of freedom.
#' @param YoungMod        Young's modulus.
#' @param area            Cross section area of a plane truss element.
#' @param alpha           Thermal conductivity.
#' @param tempchange      Temperature change.
#' @param theta           Angular orientation of a truss element.
#'
#' @return                A vector of thermal body force
#'                            for a plane truss element.
#' @export
ThermalPlaneTruss_BF= function(DOF=4,YoungMod,area,alpha,tempchange,theta)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  nodalthermal=area*alpha*tempchange*YoungMod
  equivalentbodyload=matrix(c(-nodalthermal*m,
                              -nodalthermal*n,nodalthermal*m,nodalthermal*n),
                            nrow=DOF,byrow=T)
  return (equivalentbodyload)
}




#' Expanded vector of thermal body force (thermal plane truss element)
#'
#' This function generates the expanded vector of body force
#' for a plane truss element in a thermal analysis.
#'
#' @param TDOF             Total degree of freedom in a connected
#'                         system of plane truss element.
#' @param LoadColumnMatrix Vector of body force returned
#'                          by ThermalPlaneTruss_BF()
#' @param i               Index of the first node.
#' @param j               Index of the second node.
#'
#' @return                The expanded vector of a plane truss element.
#' @export
ThermalPlaneTruss_ExpandedBF = function(TDOF,LoadColumnMatrix,i,j)
{
  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  bigColumnMatrix=matrix(vector(l=TDOF),nrow=TDOF,byrow=T);
  bigColumnMatrix[c(r1,r2,r3,r4)]=LoadColumnMatrix;
  return (bigColumnMatrix)
}


#' Expanded stiffness matrix (thermal plane element)
#'
#' This function generates the expanded stiffness matrix
#'         for plane truss element in a thermal analysis.
#'
#' @param TDOF             Total degree of freedom in a
#'                          connected system of plane truss elements.
#' @param eMatrix          The 4 by 4 stiffness matrix of a
#'                          specific bar element.
#' @param i                Index of the first node.
#' @param j                Index of the first node.
#'
#' @return                 The expanded stiffness matrix of a
#'                                thermal plane truss element.
#' @export
#'
ThermalPlaneTruss_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{

  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
  bigMatrix[c(r1,r2,r3,r4),c(r1,r2,r3,r4)]=eMatrix;
  return (bigMatrix)
}



#' Global nodal forces (thermal plane truss)
#'
#' This function generates the global nodal
#' forces for a plane truss element in a thermal analysis.
#'
#' @param bigKmatrix          Global stiffness matrix.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#'
#' @return                     Vector of global nodal forces
#'                                (thermal plane truss element).
#' @export
#' @export
ThermalPlaneTruss_Global_Forces = function(bigKmatrix,vec_globalnodaldisp)
{
  global_forces = bigKmatrix %*% vec_globalnodaldisp
  return(round(global_forces))
}


#' Local nodal forces (thermal plane truss)
#'
#' This function generates the local nodal forces for
#' a plane truss element in a thermal analysis.
#'
#' @param stifftruss            Axial stiffness of the truss.
#' @param theta                 Angular orientation of an element.
#' @param vec_globalnodaldisp   Vector of global nodal displacements.
#'
#' @return                      A vector of local nodal forces
#'                              for a plane truss element in a thermal analysis.
#' @export
ThermalPlaneTruss_Local_Forces = function(stifftruss,theta,vec_globalnodaldisp)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  localstiffnessmatrix=matrix(c(1,-1,-1,1), byrow=T,nrow=2);
  transformationmatrix=matrix(c(m,n,0,0,0,0,m,n),byrow=T,nrow=2);
  local_forces = (stifftruss)*localstiffnessmatrix %*%transformationmatrix %*%
    vec_globalnodaldisp
  return(round(local_forces))
}


#' Axial Stress (thermal plane truss)
#'
#' This function generates the axial stress in a plane truss element
#' in a thermal analysis.
#'
#' @param YoungMod            Young's modulus.
#' @param elem_len            Length of an element.
#' @param theta               Angular orientation of an element.
#' @param alpha               Thermal conductivity
#' @param tempchange          Temperature change.
#' @param vec_globalnodaldisp Vector of global nodal displacement.
#' @param i                   Index of the first node.
#' @param j                   Index of the second node.
#'
#' @return                    Axial stress of a plane truss element
#'                            in a thermal analysis.
#' @export
#'
ThermalPlaneTruss_AxialStress = function(YoungMod,elem_len,theta,alpha,tempchange,vec_globalnodaldisp,i,j)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  localdisp=vec_globalnodaldisp[c(r1,r2,r3,r4)];
  disp_vec=matrix(localdisp,nrow=length(localdisp),byrow=T,ncol = 1);
  transformationmatrix=matrix(c(-m,-n,m,n),nrow=1,byrow=T);
  local_stress=((YoungMod/elem_len)*transformationmatrix%*%disp_vec)-(YoungMod*alpha*tempchange);
  return(local_stress)
}


