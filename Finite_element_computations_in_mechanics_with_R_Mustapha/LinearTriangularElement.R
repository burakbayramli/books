
#' Area of triangular element
#'
#' This function computes the area of a CST element
#'                from the coordinates of its nodes.
#'
#'
#' @param vec_nodalcoordinates Vector of nodal coordinates
#'                              in the form c(x1,y1,x2,y2,x3,y3).
#'
#' @return Area of a triangular element.
#' @export

CSTriangular_Area =function(vec_nodalcoordinates)
{
  x1=vec_nodalcoordinates[1];x2=vec_nodalcoordinates[3];
  x3=vec_nodalcoordinates[5];
  y1=vec_nodalcoordinates[2];y2= vec_nodalcoordinates[4];
  y3=vec_nodalcoordinates[6]

  Area=abs(x3*(y1-y2)+x2*(y3-y1)+x1*(y2-y3))
  return(Area/2)
}


#' Element stiffness matrix (linear triangular element)
#'
#' @param DOF Degree of freedom (6 for a linear triangular element).
#' @param YoungMod  Young's modulus.
#' @param Nu        Poisson's ratio.
#' @param thickness Thickness
#' @param vec_nodalcoordinates Vector of nodal coordinates
#'                              in the form c(x1,y1,x2,y2,x3,y3).
#' @param case Use 1 (for plane stress) or 2 for plane strain.
#'
#' @return Stiffness matrix of a linear triangular element.
#' @export
CSTriangular_Element_Matrix=function(DOF=6,YoungMod,Nu,thickness,
                                     vec_nodalcoordinates,case)
{
  x1=vec_nodalcoordinates[1];x2=vec_nodalcoordinates[3];
  x3=vec_nodalcoordinates[5];
  y1=vec_nodalcoordinates[2];y2=vec_nodalcoordinates[4];
  y3=vec_nodalcoordinates[6];
  A=abs((x3*(y1-y2)+x2*(y3-y1)+x1*(y2-y3))/2);

  B1=y2-y3;B2=y3-y1;B3=y1-y2;
  G1=x3-x2;G2=x1-x3;G3=x2-x1;
  row1=c(B1,0,B2,0,B3,0);
  row2=c(0,G1,0,G2,0,G3)
  row3=c(G1,B1,G2,B2,G3,B3)
  p1=c(1,Nu,0,Nu,1,0,0,0,(1-Nu)/2)
  p2=c(1-Nu,Nu,0,Nu,1-Nu,0,0,0,(1-2*Nu)/2)

  BMatrix=(1/(2*A))*matrix(c(row1,row2,row3),nrow=3,byrow=T);
  DPlaneStress=(YoungMod/(1-Nu^2))*matrix(p1,nrow=3,byrow=T);
  DPlaneStrain=(YoungMod/((1+Nu)*(1-2*Nu)))*matrix(p2,nrow=3,byrow=T)

  eMatrix=matrix(vector(l=DOF*DOF),nrow=DOF,ncol=DOF);
  if(case==1){
    eMatrix=(thickness*A)*t(BMatrix)%*%DPlaneStress%*%BMatrix
  }
  if(case==2){
    eMatrix=(thickness*A)*t(BMatrix)%*%DPlaneStress%*%BMatrix
  }else{eMatrix=(thickness*A)*t(BMatrix)%*%DPlaneStress%*%BMatrix}

  return(eMatrix)
}


#' Expanded stiffness matrix (linear triangular)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of linear triangular elements.
#'
#' @param TDOF Total degree of freedom of a discretized structure.
#' @param eMatrix The 6 by 6 stiffness matrix of a
#'                          specific linear triangular element.
#' @param i Index of the first node.
#' @param j Index of the second node.
#' @param k Index of the third node.
#'
#' @return The expanded matrix of a linear triangular element.
#' @export
CSTriangular_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j,k)
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
#' @param DOF          Degree of freedom of the element (6 by default)
#' @param SFtensile    Magnitude of a uniform surface tensile pressure, e.g. q in N/m^2
#' @param SFshear      Magnitude of a uniform surface shear pressure.
#' @param Length       Lenth of the side on which load is acting.
#' @param thickness    Thickness of the element
#' @param case         Use 1 (if load is on side i-j,
#'                         2 if on side j-k, &
#'                         3 if on side i-k)
#'
#' @return             A vector of equivalent loads for a CST element with distributed load.
#' @export
CSTriangular_SF= function(DOF=6,SFtensile,SFshear,Length,thickness,case)
{
  px=SFtensile;py=SFshear;
  L=Length;b=thickness;
  if(case==1){
    equivalentload=matrix(c(px*L*b/2,py*L*b/2,px*L*b/2,py*L*b/2,0,0),nrow=DOF,byrow=T)
  }
  if(case==2){
    equivalentload=matrix(c(0,0,px*L*b/2,py*L*b/2,px*L*b/2,py*L*b/2),nrow=DOF,byrow=T)
  }
  if(case==3){
    equivalentload=matrix(c(px*L*b/2,py*L*b/2,0,0,px*L*b/2,py*L*b/2),nrow=DOF,byrow=T)
  }
  #equivalentload=matrix(c(0,0,px*L*b/2,py*L*b/2,px*L*b/2,py*L*b/2),nrow=DOF,byrow=T)
  return (equivalentload)
}


#' Expanded vector of equivalent load
#'
#' This function generates the expanded vector of equivalent nodal loads
#'                                             (linear triangular element).
#'
#' @param TDOF             Total degree of freedom.
#' @param LoadColumnMatrix The unexpanded vector of equivalent loads.
#' @param i                Index of the first node.
#' @param j                Index of the second node.
#' @param k                Index of the third node.
#'
#' @return                 Expanded vector (a column matrix) of equivalent loads.
#' @export
CSTriangular_ExpandedSF = function(TDOF,LoadColumnMatrix,i,j,k)
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
#' @param bigKmatrix           Global stiffness matrix.
#' @param vec_globalnodaldisp  Vector of all global nodal displacements.
#'
#' @return                     Global nodal forces.
#' @export
CSTriangular_GlobalForces= function(bigKmatrix,vec_globalnodaldisp)
{
  columndof=matrix(vec_globalnodaldisp,byrow = T)
  globalforces = bigKmatrix %*% vec_globalnodaldisp
  return(globalforces)

}


#' Local element forces (linear triangular element)
#'
#' @param ematrix             Element matrix.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#' @param i                   Index of the first node.
#' @param j                   Index of the second node.
#' @param k                   Index of the third node.
#'
#' @return                    Local nodal forces (linear triangular element).
#' @export
CSTriangular_LocalForces = function(ematrix,vec_globalnodaldisp,i,j,k)
{
  r1=2*i-1; r2=2*i;
  r3=2*j-1; r4=2*j;
  r5=2*k-1; r6=2*k;
  localforces = ematrix%*%vec_globalnodaldisp[c(r1,r1,r3,r4,r5,r6)]
  return(round(localforces))
}


#' Centroidal stress
#'
#' @param YoungMod           Young's modulus.
#' @param Nu                 Poisson's ratio.
#' @param thickness          Thickness.
#' @param vec_nodalcoord     Vector of nodal coordinates
#'                              in the form c(x1,y1,x2,y2,x3,y3).
#' @param case               Use 1 (for plane stress) or 2 for plane strain.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#' @param i                   Index of the first node.
#' @param j                   Index of the second node.
#' @param k                   Index of the third node.
#'
#' @return                    Centroidal stresses in a linear triangular element.
#' @export
CSTriangular_Stresses = function(YoungMod,Nu,thickness,
                                 vec_nodalcoord,case,vec_globalnodaldisp,i,j,k)
{
  x1=vec_nodalcoord[1];x2=vec_nodalcoord[3];
  x3=vec_nodalcoord[5];
  y1=vec_nodalcoord[2];y2=vec_nodalcoord[4];
  y3=vec_nodalcoord[6];
  A=abs((x3*(y1-y2)+x2*(y3-y1)+x1*(y2-y3))/2);

  B1=y2-y3;B2=y3-y1;B3=y1-y2;
  G1=x3-x2;G2=x1-x3;G3=x2-x1;
  row1=c(B1,0,B2,0,B3,0);
  row2=c(0,G1,0,G2,0,G3)
  row3=c(G1,B1,G2,B2,G3,B3)
  p1=c(1,Nu,0,Nu,1,0,0,0,(1-Nu)/2)
  p2=c(1-Nu,Nu,0,Nu,1-Nu,0,0,0,(1-2*Nu)/2)

  BMatrix=(1/(2*A))*matrix(c(row1,row2,row3),nrow=3,byrow=T);
  DPlaneStress=(YoungMod/(1-Nu^2))*matrix(p1,nrow=3,byrow=T);
  DPlaneStrain=(YoungMod/((1+Nu)*(1-2*Nu)))*matrix(p2,nrow=3,byrow=T)
  r1=2*i-1; r2=2*i
  r3=2*j-1; r4=2*j
  r5=2*k-1; r6=2*k

  if(case==1){
    localstresses=DPlaneStress%*%
      BMatrix%*%vec_globalnodaldisp[c(r1,r2,r3,r4,r5,r6)]
  }
  if(case==2){
    localstresses=DPlaneStrain%*%
      BMatrix%*%vec_globalnodaldisp[c(r1,r2,r3,r4,r5,r6)]
  }else{localstresses=DPlaneStress%*%
          BMatrix%*%vec_globalnodaldisp[c(r1,r2,r3,r4,r5,r6)]
  }

  return(localstresses)
}


