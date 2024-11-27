

#' Geometric length and orientations from xyz coordinates (space truss)
#'
#' This function is used to get the geometric length  and orientation
#' from the nodal coordinates of a space truss element.
#'
#' @param vec_nodalcoordinates Vector of nodal coordinates
#'                              in the form c(x1,y1,z1,x2,y2,z3).
#'
#' @return Vector of the geometric length and orientation angleector of the geometric length and orientation angle.
#' @export
SpaceTruss_LengthOrientations =function(vec_nodalcoordinates)
{
  xsquared=(vec_nodalcoordinates[4]-vec_nodalcoordinates[1])^2
  ysquared=(vec_nodalcoordinates[5]-vec_nodalcoordinates[2])^2
  zsquared=(vec_nodalcoordinates[6]-vec_nodalcoordinates[3])^2
  length_of_element=sqrt(xsquared+ysquared+zsquared)

  cx=(1/length_of_element)*(vec_nodalcoordinates[4]-
                              vec_nodalcoordinates[1])
  cy=(1/length_of_element)*(vec_nodalcoordinates[5]-
                              vec_nodalcoordinates[2])
  cz=(1/length_of_element)*(vec_nodalcoordinates[6]-
                              vec_nodalcoordinates[3])
  orientation_of_element=(180/pi)*acos(c(cx,cy,cz))
  return(c(length_of_element,orientation_of_element))

}


#' Element stiffness matrix (space truss)
#'
#' This function generates the 6 by 6 stiffness matrix for
#' a space truss element.
#'
#' @param DOF Degree of freedom (6 for a space truss).
#' @param stifftruss Axial stiffness of an element (N/m, i.e EA/L).
#' @param thetas Angular orientation of a truss element.
#'
#' @return Stiffness matrix of a space truss element.
#' @export
SpaceTruss_Element_Matrix=function(DOF=6,stifftruss,thetas){
  cx=cos(thetas[1]*pi/180);
  cy=cos(thetas[2]*pi/180);
  cz=cos(thetas[3]*pi/180);
  submatrix=stifftruss*matrix(c(cx^2,cx*cy,cx*cz,cy*cx,cy^2,
                                cy*cz,cz*cx,cz*cy,cz^2),nrow=3,byrow=T);
  eMatrix=matrix(vector(l=DOF*DOF),nrow=DOF,ncol=DOF);
  eMatrix[1:3,1:3]=submatrix;
  eMatrix[4:6,1:3]=-submatrix;
  eMatrix[1:3,4:6]=-submatrix;
  eMatrix[4:6,4:6]=submatrix;
  return(round(eMatrix,2))
}


#' Expanded stiffness matrix (space truss)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of space truss structure.
#'
#' @param TDOF Total degree of freedom in a connected system of truss.
#' @param eMatrix The 6 by 6 stiffness matrix of a specific truss element.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return The expanded matrix of a space truss element.
#' @export

SpaceTruss_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{

  r1=2*(i-1)+i;
  r2=2*(i-1)+(i+1);
  r3=2*(i-1)+(i+2);
  r4=(j-1)+(j+2)+(j-3);
  r5=(j-1)+(j+3)+(j-3);
  r6=(j-1)+(j+4)+(j-3);
  bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
  bigMatrix[c(r1,r2,r3,r4,r5,r6),c(r1,r2,r3,r4,r5,r6)]=eMatrix;
  return (bigMatrix)

}



#' Local element forces (space truss)
#'
#' This function generates the local forces at the nodes of
#'  a space truss element.
#'
#' @param YoungMod Young modulus.
#' @param Area     Cross-sectional area.
#' @param thetas   Angular orientation of an element.
#' @param vec_globalnodaldisp Vector of global nodal displacements.
#' @param i Index of the first node.
#' @param j Index of the first node.
#'
#'
#' @return Local nodal forces (space truss).
#' @export

SpaceTruss_Local_Forces = function(YoungMod,Area,thetas,vec_globalnodaldisp,i,j)
{
  cx=cos(thetas[1]*pi/180);
  cy=cos(thetas[2]*pi/180);
  cz=cos(thetas[3]*pi/180);
  r1=2*(i-1)+i;
  r2=2*(i-1)+(i+1);
  r3=2*(i-1)+(i+2);
  r4=(j-1)+(j+2)+(j-3);
  r5=(j-1)+(j+3)+(j-3);
  r6=(j-1)+(j+4)+(j-3);
  elementdisp=vec_globalnodaldisp[c(r1,r2,r3,r4,r5,r6)];
  localstiffnessmatrix=matrix(c(1,-1,-1,1), byrow=T,nrow=2);
  transformationmatrix=matrix(c(cx,cy,cz,0,0,0,0,0,0,cx,cy,cz),byrow=T,nrow=2);
  local_forces = (YoungMod/Area)*localstiffnessmatrix %*%transformationmatrix %*%
    elementdisp
  return(round(local_forces))
}


#' Global forces (space truss)
#'
#' This function generates the global forces at the nodes of space truss element.
#'
#' @param bigKmatrix Global stiffness matrix.
#' @param vec_globalnodaldisp Vector of global nodal displacements.
#'
#' @return Global nodal forces (space truss).
#' @export

SpaceTruss_Global_Forces = function(bigKmatrix,vec_globalnodaldisp)
{
  global_forces = bigKmatrix %*% vec_globalnodaldisp
  return(round(global_forces))
}


#' Axial Stress (space truss)
#'
#' This function generates the axial stress in a space truss element.
#'
#' @param YoungMod Young modulus.
#' @param elem_len Length of an element.
#' @param thetas Angular orientations of the element.
#' @param vec_globalnodaldisp Vector of global nodal displacement.
#'
#' @return Axial stress of a space truss element.
#' @export
Spacetruss_AxialStress = function(YoungMod,elem_len,thetas,vec_globalnodaldisp)
{
  cx=cos(thetas[1]*pi/180);
  cy=cos(thetas[2]*pi/180);
  cz=cos(thetas[3]*pi/180);
  disp_vec=matrix(vec_globalnodaldisp,nrow=length(vec_globalnodaldisp),byrow=T,ncol = 1);
  transformationmatrix=matrix(c(-cx,-cy,-cz,cx,cy,cz),nrow=1,byrow=T);
  local_stress=(YoungMod/elem_len)*transformationmatrix%*%disp_vec;
  return(local_stress)
}


