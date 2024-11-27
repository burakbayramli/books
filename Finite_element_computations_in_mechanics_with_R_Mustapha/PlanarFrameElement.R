
#' Geometric length and orientation from xy coordinates (plane frame)
#'
#'This function is used to get the geometric length  and angular orientation
#' from the nodal coordinates of an element.
#'
#' @param vec_nodalcoordinates Vector of nodal coordinates
#'                              in the form c(x1,y1,x2,y2).
#'
#' @return Vector of the geometric length and orientation angle.
#' @export

PlaneFrame_LengthOrientation =function(vec_nodalcoordinates)
{
  xsquared=(vec_nodalcoordinates[3]-vec_nodalcoordinates[1])^2
  ysquared=(vec_nodalcoordinates[4]-vec_nodalcoordinates[2])^2
  length_of_element=sqrt(xsquared+ysquared)

  ydiff=(vec_nodalcoordinates[4]-vec_nodalcoordinates[2])
  xdiff=(vec_nodalcoordinates[3]-vec_nodalcoordinates[1])
  orientation_of_element=(180/pi)*atan(ydiff/xdiff)
  return(c(length_of_element,orientation_of_element))
}



#' Element stiffness matrix (plane frame)
#'
#'  This function generates the 6 by 6 stiffness matrix for
#' a plane frame element.
#'
#' @param DOF Degree of freedom (6 for a plane frame).
#' @param YoungMod Young's modulus.
#' @param Area Cross-sectional area.
#' @param momentI Principal moment of inertia.
#' @param Length Element's geometric length.
#' @param theta Element's angular orientation.
#'
#' @return Stiffness matrix of a plane frame element.
#' @export
PlaneFrame_Element_Matrix=function(DOF=6,YoungMod,Area,momentI,Length,theta)
{
  cx=cos(theta*pi/180);sx=sin(theta*pi/180);
  A=Area;I=momentI;B1=(12*momentI)/Length^2;B2=(6*momentI)/Length;

  submatrix1=matrix(c((A*cx^2+B1*sx^2),(A-B1)*cx*sx,
                      (-B2*sx),-(A*cx^2+B1*sx^2),
                      -(A-B1)*cx*sx,(-B2*sx)),nrow=1,byrow=T);
  submatrix2=matrix(c(submatrix1[,2],(A*sx^2+B1*cx^2),(B2*cx),-(A-B1)*cx*sx,
                      -(A*sx^2+B1*cx^2),(B2*cx)),nrow=1,byrow=T);
  submatrix3=matrix(c((-B2*sx),(B2*cx),(4*I),(B2*sx),(-B2*cx),
                      (2*I)),nrow=1,byrow=T);

  submatrix4=matrix(c(submatrix1[,4],submatrix2[,4],submatrix3[,4],
                      submatrix1[,1],submatrix1[,2],B2*sx),nrow=1,byrow=T);
  submatrix5=matrix(c(submatrix1[,5],submatrix2[,5],submatrix3[,5],
                      submatrix4[,5],submatrix2[,2],-B2*cx),nrow=1,byrow=T);
  submatrix6=matrix(c(submatrix1[,6],submatrix2[,6],submatrix3[,6],
                      submatrix4[,6],submatrix5[,6],4*I),nrow=1,byrow=T);
  eMatrix=matrix(vector(l=DOF*DOF),nrow=DOF,ncol=DOF);
  eMatrix[1,]=submatrix1;eMatrix[2,]=submatrix2;
  eMatrix[3,]=submatrix3;eMatrix[4,]=submatrix4;
  eMatrix[5,]=submatrix5;eMatrix[6,]=submatrix6;
  return((YoungMod/Length)*eMatrix)
}


#' Expanded stiffness matrix (plane frame)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of plane frame structures.
#'
#' @param TDOF Total degree of freedom of a connected system of frame structures.
#' @param eMatrix The 6 by 6 stiffness matrix of a
#'                          specific plane frame element.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return The expanded matrix of a plane frame element.
#' @export
PlaneFrame_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{
  r1=2*(i-1)+i;r2=2*(i-1)+(i+1);
  r3=2*(i-1)+(i+2);r4=(j-1)+(j+2)+(j-3);
  r5=(j-1)+(j+3)+(j-3);r6=(j-1)+(j+4)+(j-3);
  bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
  bigMatrix[c(r1,r2,r3,r4,r5,r6),c(r1,r2,r3,r4,r5,r6)]=eMatrix;
  return (bigMatrix)

}


#' Equivalent load vector for a UDL (frame element)
#'
#' Generates the load column matrix for a plane frame element with a
#'                             uniformly distributed load.
#'
#' @param DOF Degree of freedom (4 for a beam).
#' @param AxialUDLMagnitude Magnitude of the axial UDL, e.g. q in N/m.
#' @param TransUDLMagnitude Magnitude of the transverse UDL, e.g. q in N/m.
#' @param Length Element's geometric length.
#' @param theta Element's orientation angle.
#'
#' @return A column matrix of the equivalent nodal loads.
#' @export
#'
 PlaneFrameUDL_Matrix= function(DOF=6,AxialUDLMagnitude,TransUDLMagnitude,Length,theta)
  {
    L=Length;
    cx=cos(theta*pi/180);sx=sin(theta*pi/180);
    w1=AxialUDLMagnitude;w2=TransUDLMagnitude;
    row1=0.5*L*(-cx*w1+sx*w2);row2=-0.5*L*(sx*w1+cx*w2)
    row3=-(1/12)*(L^2)*w2;row4=0.5*L*(-cx*w1+sx*w2);
    row5=-0.5*L*(sx*w1+cx*w2);row6=(1/12)*(L^2)*w2
    equivalentload=matrix(c(row1,row2,row3,row4,row5,row6),nrow=DOF,byrow=T)
    return (equivalentload)
  }


#' Expanded vector of the equivalent load
#'
#' This function generates the expanded vector of the equivalent load
#'                                             (plane frame).
#'
#' @param TDOF             Total degree of freedom.
#' @param LoadColumnMatrix The unexpanded vector of equivalent loads.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return Expanded vector (a column matrix) of equivalent loads.
#' @export
#'
PlaneFrameUDL_ExpandedMatirx = function(TDOF,LoadColumnMatrix,i,j)
{
  r1=2*(i-1)+i;
  r2=2*(i-1)+(i+1);
  r3=2*(i-1)+(i+2);
  r4=(j-1)+(j+2)+(j-3);
  r5=(j-1)+(j+3)+(j-3);
  r6=(j-1)+(j+4)+(j-3);
  bigColumnMatrix=matrix(vector(l=TDOF),nrow=TDOF,byrow=T);
  bigColumnMatrix[c(r1,r2,r3,r4,r5,r6)]=LoadColumnMatrix;
  return (bigColumnMatrix)
}



#' Global nodal forces and moments
#'
#' This function generates the nodal global forces
#'                    and moments for plane frame element.
#'
#' @param bigKmatrix           Global stiffness matrix.
#' @param vec_globalnodaldisp  Vector of all global nodal displacements.
#'
#' @return Global nodal forces and moments.
#' @export
#'
PlaneFrame_GlobalForcesMoments = function(bigKmatrix,vec_globalnodaldisp)
{
  columndof=matrix(vec_globalnodaldisp,byrow = T)
  globalforces = bigKmatrix %*% vec_globalnodaldisp
  return(round(globalforces))

}



#' Local element forces (plane frame)
#'
#' @param YoungMod         Young's modulus.
#' @param Area             Cross-sectional area.
#' @param momentI          Principal moment of inertia.
#' @param Length           Element's geometric length.
#' @param theta            Element's orientation angle.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#' @param i                Index of the first node.
#' @param j                Index of the second node.
#'
#' @return                 Local nodal forces (plane frame).
#' @export
PlaneFrame_LocalForcesMoments = function(YoungMod,Area,momentI,Length,theta,vec_globalnodaldisp,i,j)
{

  r1=2*(i-1)+i;r2=2*(i-1)+(i+1);
  r3=2*(i-1)+(i+2);r4=(j-1)+(j+2)+(j-3);
  r5=(j-1)+(j+3)+(j-3);r6=(j-1)+(j+4)+(j-3);
  elementdisp=vec_globalnodaldisp[c(r1,r2,r3,r4,r5,r6)]
  E1=YoungMod;A=Area;I=momentI;L=Length;
  cx=cos(theta*pi/180);sx=sin(theta*pi/180);
  row1=c(cx,sx,0,0,0,0);row2=c(-sx,cx,0,0,0,0);
  row3=c(0,0,1,0,0,0);row4=c(0,0,0,cx,sx,0);
  row5=c(0,0,0,-sx,cx,0);row6=c(0,0,0,0,0,1);
  tmatrix=matrix(c(row1,row2,row3,row4,row5,row6),nrow=6,byrow=T)
  localstiffness=tmatrix%*%PlaneFrame_Element_Matrix(6,E1,A,I,L,theta);
  local_forces = localstiffness%*%elementdisp
  return(round(local_forces))
}



