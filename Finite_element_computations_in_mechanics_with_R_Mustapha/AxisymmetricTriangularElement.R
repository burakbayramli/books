
#' Area of an axisymmetric triangular element
#'
#' This function computes the area of an axisymmetric triangular element
#'                                     from the coordinates of its nodes.
#'
#' @param vec_nodalcoordinates           A vector of nodal coordinates
#'                                       in the form c(x1,y1,x2,y2,x3,y3).
#'
#' @return                               Area of an axisymmetric triangular triangular element.
#' @export
AxisymmetricT_Area =function(vec_nodalcoordinates)
{
  r1 = vec_nodalcoordinates[1];
  r2 = vec_nodalcoordinates[3];
  r3 = vec_nodalcoordinates[5];
  z1 = vec_nodalcoordinates[2];
  z2 = vec_nodalcoordinates[4];
  z3 = vec_nodalcoordinates[6]

  Area=abs(r3*(z1-z2)+r2*(z3-z1)+r1*(z2-z3))
  return(Area/2)
}


#' Centroid of an axisymmetric triangular element
#'
#' @param vec_nodalcoordinates   A vector of nodal coordinates
#'                               in the form c(x1,y1,x2,y2,x3,y3).
#'
#' @return                       A vector (rbar and zbar) describing the centroid
#'                                          of an axisymmetric triangular element.
#' @export
AxisymmetricT_Centroid =function(vec_nodalcoordinates)
{
  r1 = vec_nodalcoordinates[1];
  r2 = vec_nodalcoordinates[3];
  r3 = vec_nodalcoordinates[5];
  z1 = vec_nodalcoordinates[2];
  z2 = vec_nodalcoordinates[4];
  z3 = vec_nodalcoordinates[6]

  rbar = (1/3)*(r1+r2+r3);
  zbar = (1/3)*(z1+z2+z3);
  return(c(rbar,zbar))
}


#' Element stiffness matrix (axisymmetric triangular element)
#'
#' @param DOF                  Degree of freedom (6 for an axisymmetric triangular element).
#' @param YoungMod             Young's modulus.
#' @param Nu                   Poisson's ratio.
#' @param vec_nodalcoordinates Vector of nodal coordinates
#'                               in the form c(x1,y1,x2,y2,x3,y3).
#'
#' @return                     Stiffness matrix of an axisymmetric triangular element.
#' @export
AxisymmetricT_Element_Matrix=function(DOF=6,YoungMod,Nu,vec_nodalcoordinates)
{
  r1=vec_nodalcoordinates[1];r2=vec_nodalcoordinates[3];
  r3=vec_nodalcoordinates[5];z1=vec_nodalcoordinates[2];
  z2=vec_nodalcoordinates[4];z3=vec_nodalcoordinates[6];

  Area=abs(r3*(z1-z2)+r2*(z3-z1)+r1*(z2-z3))*0.5;
  rbar=(1/3)*(r1+r2+r3);zbar=(1/3)*(z1+z2+z3);

  a1=r2*z3-z2*r3;
  a2=r3*z1-z3*r1;
  a3=r1*z2-z1*r2;
  B1=z2-z3;B2=z3-z1;B3=z1-z2;
  G1=r3-r2;G2=r1-r3;G3=r2-r1;
  row1=c(B1,0,B2,0,B3,0);
  row2=c(0,G1,0,G2,0,G3);
  row3=c(((a1/rbar)+B1+(G1*zbar/rbar)),0,((a2/rbar)+B2+(G2*zbar/rbar)),0,((a3/rbar)+B3+(G3*zbar/rbar)),0);
  row4=c(G1,B1,G2,B2,G3,B3);
  constitutiveprop=c(1-Nu,Nu,Nu,0,Nu,1-Nu,Nu,0,Nu,Nu,1-Nu,0,0,0,0,(1-2*Nu)/2)

  BMatrix=(1/(2*Area))*matrix(c(row1,row2,row3,row4),nrow=4,byrow=T);
  DMatrix=YoungMod/((1+Nu)*(1-2*Nu))*matrix(constitutiveprop,nrow=4,byrow=T);
  eMatrix=(2*pi*rbar*Area)*t(BMatrix)%*%DMatrix%*%BMatrix
  return(eMatrix)
}


#' Expanded stiffness matrix (axisymmetric triangular element)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of an axisymmetric triangular element elements.
#'
#' @param TDOF        Total degree of freedom of a discretized structure.
#' @param eMatrix     The 6 by 6 stiffness matrix of a
#'                          specific axisymmetric triangular element.
#' @param i           Index of the first node.
#' @param j           Index of the second node.
#' @param k           Index of the third node.
#'
#' @return            The expanded matrix of an axisymmetric triangular element.
#' @export
AxisymmetricT_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j,k)
{
  r1=2*i-1; r2=2*i
  r3=2*j-1; r4=2*j
  r5=2*k-1; r6=2*k
  bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
  bigMatrix[c(r1,r2,r3,r4,r5,r6),c(r1,r2,r3,r4,r5,r6)]=eMatrix;
  return (bigMatrix)
}


#' Equivalent surface load for an element with distributed load
#'
#' @param DOF                     Degree of freedom of the element (6 by default)
#' @param pr                      Magnitude of a uniform radial load
#' @param pz                      Magnitude of a uniform axial load
#' @param case                    Use 1 (if load is on side i-j,
#'                                    2 if on side j-k, &
#'                                    3 if on side i-k)
#' @param vec_nodalcoordinates   Vector of nodal coordinates
#'                               in the form c(x1,y1,x2,y2,x3,y3).
#'
#' @return                       A vector of equivalent nodal loads for an axisymmetric
#'                                                 triangular element with surface load.
#' @export
AxisymmetricT_SF= function(DOF=6,pr,pz,case,vec_nodalcoordinates)
{
  r1=vec_nodalcoordinates[1];
  r2=vec_nodalcoordinates[3];
  r3=vec_nodalcoordinates[5];
  z1=vec_nodalcoordinates[2];
  z2=vec_nodalcoordinates[4];
  z3=vec_nodalcoordinates[6];
  if(case==1){
    equivalentsfload=pi*r2*abs((z3-z2))*matrix(c(0,0,pr,pz,pr,pz),nrow=DOF,byrow=T)
  }
  if(case==2){
    equivalentsfload=pi*r1*abs((z3-z1))*matrix(c(pr,pz,0,0,pr,pz),nrow=DOF,byrow=T)
  }
  if(case==3){
    equivalentsfload=pi*r1*abs((z2-z1))*matrix(c(pr,pz,pr,pz,0,0),nrow=DOF,byrow=T)
  }
  return (equivalentsfload)
}



#' Equivalent body load for an element with body force
#'
#' @param DOF                      Degree of freedom of the element (6 by default)
#' @param Rb                       Radial body force.
#' @param Zb                       Axial body force
#' @param vec_nodalcoordinates     Vector of nodal coordinates
#'                                 in the form c(x1,y1,x2,y2,x3,y3).
#'
#' @return                         A vector of equivalent nodal loads for an axisymmetric
#'                                                      triangular element with body load.
#' @export
AxisymmetricT_BF= function(DOF=6,Rb,Zb,vec_nodalcoordinates)
{
  r1=vec_nodalcoordinates[1];
  r2=vec_nodalcoordinates[3];
  r3=vec_nodalcoordinates[5];
  z1=vec_nodalcoordinates[2];
  z2=vec_nodalcoordinates[4];
  z3=vec_nodalcoordinates[6];
  rbar=(1/3)*(r1+r2+r3);
  Area=abs(r3*(z1-z2)+r2*(z3-z1)+r1*(z2-z3))*0.5;

  equivalentbodyload=(2/3)*pi*rbar*Area*matrix(c(Rb,Zb,Rb,Zb,Rb,Zb),nrow=DOF,byrow=T)

  return (equivalentbodyload)
}


#' Expanded vector of equivalent nodal loads (surface/body)
#'
#' @param TDOF                  Total degree of freedom.
#' @param LoadColumnMatrix      The unexpanded vector of equivalent loads,
#'                              from either AxisymmetricT_BF() or AxisymmetricT_SF().
#' @param i                     Index of the first node.
#' @param j                     Index of the second node.
#' @param k                     Index of the third node.
#'
#' @return                      Expanded vector (a column matrix) of equivalent loads.
#' @export
AxisymmetricT_ExpandedSFBF = function(TDOF,LoadColumnMatrix,i,j,k)
{
  r1=2*i-1; r2=2*i
  r3=2*j-1; r4=2*j
  r5=2*k-1; r6=2*k
  bigColumnMatrix=matrix(vector(l=TDOF),nrow=TDOF,byrow=T);
  bigColumnMatrix[c(r1,r2,r3,r4,r5,r6)]=LoadColumnMatrix;
  return (bigColumnMatrix)
}



#' Global nodal forces
#'
#' This function generates the nodal global forces for linear triangular element.
#'
#' @param bigKmatrix               Global stiffness matrix.
#' @param vec_globalnodaldisp      Vector of all global nodal displacements.
#'
#' @return                         Global nodal forces.
#' @export
AxisymmetricT_GlobalForces= function(bigKmatrix,vec_globalnodaldisp)
{
  columndof=matrix(vec_globalnodaldisp,byrow = T)
  globalforces = bigKmatrix %*% vec_globalnodaldisp
  return(globalforces)

}



#' Local element forces (axisymmetric triangular element)
#'
#' @param ematrix                  Element matrix
#' @param vec_globalnodaldisp      Vector of all global nodal displacements.
#' @param i                        Index of the first node.
#' @param j                        Index of the second node.
#' @param k                        Index of the third node.
#'
#' @return                         Local nodal forces (axisymmetric triangular element).
#' @export
AxisymmetricT_LocalForces = function(ematrix,vec_globalnodaldisp,i,j,k)
{
  r1=2*i-1; r2=2*i
  r3=2*j-1; r4=2*j
  r5=2*k-1; r6=2*k
  localforces = ematrix%*%vec_globalnodaldisp[c(r1,r1,r3,r4,r5,r6)]
  return(round(localforces))
}


#' Centroidal stress
#'
#' @param YoungMod                 Young's modulus.
#' @param Nu                       Poisson's ratio.
#' @param vec_nodalcoord           Vector of nodal coordinates
#'                                 in the form c(x1,y1,x2,y2,x3,y3).
#' @param vec_globalnodaldisp      Vector of all global nodal displacements.
#' @param i                        Index of the first node.
#' @param j                        Index of the second node.
#' @param k                        Index of the third node.
#'
#' @return                         Centroidal stresses in an axisymmetric triangular element.
#' @export
AxisymmetricT_Stresses = function(YoungMod,Nu,vec_nodalcoord,vec_globalnodaldisp,i,j,k)
{
  r1=vec_nodalcoord[1];
  r2=vec_nodalcoord[3];
  r3=vec_nodalcoord[5];
  z1=vec_nodalcoord[2];
  z2=vec_nodalcoord[4];
  z3=vec_nodalcoord[6]

  Area=abs(r3*(z1-z2)+r2*(z3-z1)+r1*(z2-z3))*0.5;
  rbar=(1/3)*(r1+r2+r3);
  zbar=(1/3)*(z1+z2+z3);

  a1=r2*z3-z2*r3;
  a2=r3*z1-z3*r1;
  a3=r1*z2-z1*r2;
  B1=z2-z3;B2=z3-z1;B3=z1-z2;
  G1=r3-r2;G2=r1-r3;G3=r2-r1;
  row1=c(B1,0,B2,0,B3,0);
  row2=c(0,G1,0,G2,0,G3);
  row3=c(((a1/rbar)+B1+(G1*zbar/rbar)),0,((a2/rbar)+B2+(G2*zbar/rbar)),0,((a3/rbar)+B3+(G3*zbar/rbar)),0);
  row4=c(G1,B1,G2,B2,G3,B3);
  constitutiveprop=c(1-Nu,Nu,Nu,0,Nu,1-Nu,Nu,0,Nu,Nu,1-Nu,0,0,0,0,(1-2*Nu)/2)

  BMatrix=(1/(2*Area))*matrix(c(row1,row2,row3,row4),nrow=4,byrow=T);
  DPlaneStress=YoungMod/((1+Nu)*(1-2*Nu))*matrix(constitutiveprop,nrow=4,byrow=T);
  r1=2*i-1; r2=2*i
  r3=2*j-1; r4=2*j
  r5=2*k-1; r6=2*k

  localstresses=DPlaneStress%*%BMatrix%*%vec_globalnodaldisp[c(r1,r2,r3,r4,r5,r6)]

  return(localstresses)
}



