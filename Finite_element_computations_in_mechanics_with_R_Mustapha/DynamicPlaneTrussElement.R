


#' Geometric length and orientation from the xy coordinates (plane truss)
#'
#' This function is used to get the geometric length  and orientation
#' from the nodal coordinates of an element.
#'
#' @param vec_nodalcoordinates    Vector of nodal coordinates
#'                                in the form c(x1,y1,x2,y2).
#'
#' @return                        Vector of the geometric length and orientation angle.
#' @export
#'
DynamicPlaneTruss_LengthOrientation =function(vec_nodalcoordinates)
{
  ysquared=(vec_nodalcoordinates[4]-vec_nodalcoordinates[2])^2
  xsquared=(vec_nodalcoordinates[3]-vec_nodalcoordinates[1])^2
  length_of_element=sqrt(ysquared+xsquared)
  ydiff=(vec_nodalcoordinates[4]-vec_nodalcoordinates[2])
  xdiff=(vec_nodalcoordinates[3]-vec_nodalcoordinates[1])
  orientation_of_element=(180/pi)*atan(ydiff/xdiff)
  return(round(c(length_of_element,orientation_of_element),3))

}


#' Element stiffness matrix (dynamic plane truss)
#'
#' This function generates the 4 by 4 stiffness matrix for
#' a plane truss element.
#'
#' @param DOF          Degree of freedom (4 for a truss).
#' @param stifftruss   Axial stiffness of an element (N/m, i.e EA/L).
#' @param theta        Angular orientation of a truss element.
#'
#' @return             Stiffness matrix of a plane truss element.
#' @export
DynamicPlaneTruss_StiffnessMatrix=function(DOF = 4,stifftruss,theta)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  stiffnesscoefficients=c(m^2,m*n,-m^2,-m*n,
                          m*n,n^2,-m*n,-n^2,
                          -m^2,-m*n,m^2,m*n,
                          -m*n,-n^2,m*n,n^2)
  ematrix=stifftruss*matrix(stiffnesscoefficients,nrow=DOF,byrow=T);
  return(ematrix)
}


#' Element mass matrix (dynamic plane truss)
#'
#' This function generates the mass matrix for a plane truss element.
#'
#' @param DOF        Degree of freedom (4 for a truss).
#' @param Density    Material density
#' @param Area       Cross-sectional area
#' @param Length     Length
#' @param theta      Angular orientation of a truss element.
#'
#' @return           Mass matrix of a plane truss element.
#' @export

DynamicPlaneTruss_MassMatrix=function(DOF = 4,Density,Area,Length,theta)
{
  m=cos(theta*pi/180);
  n=sin(theta*pi/180);
  masscoefficients=c(2,0,1,0,
                     0,2,0,1,
                     1,0,2,0,
                     0,1,0,2)
  mmatrix=(Density*Area*Length/6)*matrix(masscoefficients,nrow=DOF,byrow=T);
  return(mmatrix)
}



#' Expanded stiffness/mass matrix (plane truss)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of plane truss structure.
#'
#' @param TDOF        Total degree of freedom of a connected system of plane trusses.
#' @param eMatrix     The 4 by 4 stiffness matrix of a
#'                          specific plane truss element.
#' @param i           Index of the first node.
#' @param j           Index of the second node.
#'
#' @return            The expanded matrix of a plane truss element.
#' @export
DynamicPlaneTruss_ExpandedMatrix = function(TDOF,eMatrix,i,j)
{

  r1=(i-1)+i;r2=(i-1)+(i+1);r3=(j-2)+(j+1);r4=(j-2)+(j+2);
  bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
  bigMatrix[c(r1,r2,r3,r4),c(r1,r2,r3,r4)]=eMatrix;
  return (bigMatrix)
}


