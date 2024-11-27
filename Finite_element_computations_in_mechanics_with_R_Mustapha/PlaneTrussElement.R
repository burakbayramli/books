
#' Geometric length and orientation from xy coordinates (plane truss)
#'
#' This function is used to get the geometric length  and orientation
#' from the nodal coordinates of an element.
#' See also PlaneTruss_GeometricLengths() for lengths
#'                               of multiple elements.
#'
#' @param vec_nodalcoordinates Vector of nodal coordinates
#'                              in the form c(x1,y1,x2,y2).
#'
#' @return Vector of the geometric length and orientation angle.
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
  return(c(length_of_element,orientation_of_element))

}

#' Vector of geometric lengths
#'
#' This function returns a vector of geometric lengths for
#' a set of elements.
#'
#' @param num_ele Number of elements.
#' @param vec_xnodalcoordinates Vector of x coordinates.
#' @param vec_ynodalcoordinates Vector of y coordinates.
#'
#' @return Geometric lengths of elements from a set of coordinates.
#' @export
#'
PlaneTruss_GeometricLengths =function(num_ele,vec_xnodalcoordinates,vec_ynodalcoordinates)
{
  length_of_nodalcoordinates=length(vec_xnodalcoordinates)
  numberofelement=length_of_nodalcoordinates/2
  length_of_element=vector(mode="numeric",length=numberofelement)
  counter=1;
  for (j in seq(1,length_of_nodalcoordinates-1,by=2)){
    ysquared=(vec_ynodalcoordinates[j+1]-vec_ynodalcoordinates[j])^2
    xsquared=(vec_xnodalcoordinates[j+1]-vec_xnodalcoordinates[j])^2
    length_of_element[counter]=sqrt(ysquared+xsquared)
    counter=counter+1

  }

  return(length_of_element)

}


#' Vector of angular orientations (plane truss)
#'
#' This function returns a vector of angular orientations for
#' a set of elements.
#'
#' @param num_ele Number of elements
#' @param vec_xnodalcoordinates Vector of x coordinates.
#' @param vec_ynodalcoordinates Vector of y coordinates.
#'
#' @return Angle of elements from a set of coordinates.
#' @export
#'
PlaneTruss_Orientations=function(num_ele,vec_xnodalcoordinates,vec_ynodalcoordinates)
{
  length_of_nodalcoordinates=length(vec_xnodalcoordinates)
  numberofelement=length_of_nodalcoordinates/2
  orientation_of_element=vector(mode="numeric",length=numberofelement)
  counter=1
  for (j in seq(1,length_of_nodalcoordinates-1,by=2)){
    ydiff=(vec_ynodalcoordinates[j+1]-vec_ynodalcoordinates[j])
    xdiff=(vec_xnodalcoordinates[j+1]-vec_xnodalcoordinates[j])
    orientation_of_element[counter]=(180/pi)*atan(ydiff/xdiff)
    counter=counter+1
  }

  return(round(orientation_of_element,0))

}


#' Element stiffness matrix (plane truss)
#'
#' This function generates the 4 by 4 stiffness matrix for
#' a plane truss element.
#'
#' @param DOF            Degree of freedom (4 for a truss).
#' @param stifftruss     Axial stiffness of an element (N/m, i.e EA/L).
#' @param theta          Angular orientation of a truss element.
#'
#' @return               Stiffness matrix of a plane truss element.
#' @export
#'
PlaneTruss_Element_Matrix=function(DOF = 4,stifftruss,theta)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  stiffnesscoefficients=c(m^2,m*n,-m^2,-m*n,m*n,n^2,-m*n,-n^2,-m^2,-m*n,m^2,m*n,-m*n,-n^2,m*n,n^2)
  ematrix=stifftruss*matrix(stiffnesscoefficients,nrow=DOF,byrow=T)
}



#' Expanded stiffness matrix (plane truss)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of plane truss structure.
#'
#' @param TDOF       Total degree of freedom of a connected system of plane trusses.
#' @param eMatrix    The 4 by 4 stiffness matrix of a
#'                          specific plane truss element.
#' @param i          Index of the first node.
#' @param j          Index of the second node.
#'
#' @return          The expanded matrix of a plane truss element.
#' @export
#'
PlaneTruss_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{

  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
  bigMatrix[c(r1,r2,r3,r4),c(r1,r2,r3,r4)]=eMatrix;
  return (bigMatrix)
}


#' Local element forces (plane truss)
#'
#'This function generates the local forces at the nodes of
#' a plane truss element.
#'
#' @param stifftruss Axial stiffness of the truss.
#' @param theta      Angular orientation of an element.
#' @param vec_globalnodaldisp Vector of global nodal displacements.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return Local nodal forces (truss).
#' @export
PlaneTruss_Local_Forces = function(stifftruss,theta,vec_globalnodaldisp,i,j)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  elementdisp=vec_globalnodaldisp[c(r1,r2,r3,r4)];
  localstiffnessmatrix=matrix(c(1,-1,-1,1), byrow=T,nrow=2);
  transformationmatrix=matrix(c(m,n,0,0,0,0,m,n),byrow=T,nrow=2);
  local_forces = (stifftruss)*localstiffnessmatrix %*%transformationmatrix %*%elementdisp

  return(round(local_forces))
}


#' Axial Stress (plane truss)
#'
#'This function generates the axial stress in a plane truss element.
#'
#' @param YoungMod            Young's modulus.
#' @param elem_len            Length of an element.
#' @param theta               Angular orientation of an element.
#' @param vec_globalnodaldisp Vector of global nodal displacement.
#' @param i                   Index of the first node.
#' @param j                   Index of the second node.
#'
#' @return                    Axial stress of a plane truss element.
#' @export
#'
PlaneTruss_AxialStress = function(YoungMod,elem_len,theta,vec_globalnodaldisp,i,j)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  elementdisp=vec_globalnodaldisp[c(r1,r2,r3,r4)];
  transformationmatrix=matrix(c(-m,-n,m,n),nrow=1,byrow=T);
  local_stress=(YoungMod/elem_len)*transformationmatrix%*%elementdisp;
  return(local_stress)
}
