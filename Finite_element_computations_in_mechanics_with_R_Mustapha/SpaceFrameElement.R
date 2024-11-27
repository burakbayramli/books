

#' Element stiffness matrix (space frame)
#'
#' This function generates the 12 by 12 stiffness matrix for
#' a space frame element.
#'
#' @param DOF Degree of freedom (12 for a space frame).
#' @param vec_Modulus          Vector of Young's modulus.
#' @param Area                 Area.
#' @param PolarMoment          Polar moment.
#' @param vec_MomentI          Vector of moment of inertia
#' @param vec_nodalcoordinates Vector of nodal coordinates
#'
#' @return                    Stiffness matrix of a plane frame element.
#' @export

SpaceFrame_Element_Matrix=function(DOF=12,vec_Modulus,Area,PolarMoment,vec_MomentI,vec_nodalcoordinates)
{
  xsquared=(vec_nodalcoordinates[4]-vec_nodalcoordinates[1])^2
  ysquared=(vec_nodalcoordinates[5]-vec_nodalcoordinates[2])^2
  zsquared=(vec_nodalcoordinates[6]-vec_nodalcoordinates[3])^2
  L=sqrt(xsquared+ysquared+zsquared)
  E=vec_Modulus[1];G=vec_Modulus[2];Imax=vec_MomentI[1];Imin=vec_MomentI[2];
  coord=vec_nodalcoordinates;
  A1=E*Area/L;G1=G*PolarMoment/L;
  B1=12*(E*Imax)/L^3;B2=6*(E*Imax)/L^2;
  B3=12*(E*Imin)/L^3;B4=6*(E*Imin)/L^2;
  B5=4*(E*Imax)/L;B6=4*(E*Imin)/L;
  row1=c(A1,0,0,0,0,0);row2=c(0,B1,0,0,0,B2);row3=c(0,0,B3,0,-B4,0);
  row4=c(0,0,0,G1,0,0);row5=c(0,0,-B4,0,B6,0);row6=c(0,B2,0,0,0,B5)
  submatrix1=matrix(c(row1,row2,row3,row4,row5,row6),nrow=6, byrow=T);
  row12=c(-A1,0,0,0,0,0);row22=c(0,-B1,0,0,0,B2);row32=c(0,0,-B3,0,-B4,0);
  row42=c(0,0,0,-G1,0,0);row52=c(0,0,B4,0,B6/2,0);row62=c(0,-B2,0,0,0,B5/2)
  submatrix2=matrix(c(row12,row22,row32,row42,row52,row62),nrow=6, byrow=T);
  row13=c(-A1,0,0,0,0,0);row23=c(0,-B1,0,0,0,-B2);row33=c(0,0,-B3,0,B4,0);
  row43=c(0,0,0,-G1,0,0);row53=c(0,0,-B4,0,B6/2,0);row63=c(0,B2,0,0,0,B5/2)
  submatrix3=matrix(c(row13,row23,row33,row43,row53,row63),nrow=6, byrow=T);
  localstiffness=matrix(rep(0,DOF*DOF),nrow=DOF,ncol=DOF);
  row14=c(A1,0,0,0,0,0);row24=c(0,B1,0,0,0,-B2);row34=c(0,0,B3,0,B4,0);
  row44=c(0,0,0,G1,0,0);row54=c(0,0,B4,0,B6,0);row64=c(0,-B2,0,0,0,B5)
  submatrix4=matrix(c(row14,row24,row34,row44,row54,row64),nrow=6, ncol=6, byrow=T)
  localstiffness[1:6,1:6]=submatrix1;
  localstiffness[1:6,7:12]=submatrix2;
  localstiffness[7:12,1:6]=submatrix3;
  localstiffness[7:12,7:12]=submatrix4;

  transformationmatrix=matrix(rep(0,DOF*DOF),nrow=DOF,ncol=DOF);
  if(coord[1]==coord[4] && coord[2]==coord[5]){

    if(coord[6]>coord[3]) {
          Tsubmatrix=matrix(c(0,0,1,0,1,0,-1,0,0),nrow=3, byrow=T)

       } else
         Tsubmatrix=matrix(c(0,0,-1,0,1,0,1,0,0),nrow=3, byrow=T)
    }else {

        l=(1/L)*(coord[4]-coord[1]);
        m=(1/L)*(coord[5]-coord[2]);
        n=(1/L)*(coord[6]-coord[3]);
        D=sqrt(l^2+m^2);
        Tsubmatrix=matrix(c(l,m,n,-m/D,l/D,0,-l*n/D,-m*n/D,D),nrow=3,byrow=T);
    }
  transformationmatrix[1:3,1:3]=Tsubmatrix;
  transformationmatrix[4:6,4:6]=Tsubmatrix;
  transformationmatrix[7:9,7:9]=Tsubmatrix;
  transformationmatrix[10:12,10:12]=Tsubmatrix;
  globalstiffness=t(transformationmatrix)%*%localstiffness%*%transformationmatrix
  return(globalstiffness)
}


#' Expanded stiffness matrix (space frame)
#'
#' This function generates the expanded matrix for each element in a
#' connected system of space frame structures.
#'
#' @param TDOF Total degree of freedom of a connected system
#'                                      of space frame structures.
#' @param eMatrix The 12 by 12 stiffness matrix of a
#'                          specific space frame element.
#' @param i Index of the first node.
#' @param j Index of the second node.
#'
#' @return The expanded matrix of a space frame element.
#' @export

SpaceFrame_ExpandedElement_Matrix = function(TDOF,eMatrix,i,j)
{
      r=vector(length=12);
      for(k in 6:1){
        r[k]=6*i+(k-6);
      }
      for(k in 12:7){
        r[k]=6*j+(k-12);
      }
    bigMatrix=matrix(vector(l=TDOF*TDOF),nrow=TDOF,byrow=T);
    bigMatrix[r[1:12],r[1:12]]=eMatrix;
    return (bigMatrix)

}




#' Global nodal forces and moments
#'
#' This function generates the nodal global forces
#'                    and moments for space frame element.
#'
#' @param bigKmatrix          Global stiffness matrix.
#' @param vec_globalnodaldisp Vector of all global nodal displacements.
#'
#' @return                    Global nodal forces and moments.
#' @export

SpaceFrame_GlobalForcesMoments = function(bigKmatrix,vec_globalnodaldisp)
{
  columndof=matrix(vec_globalnodaldisp,byrow = T)
  globalforces = bigKmatrix %*% vec_globalnodaldisp
  return(round(globalforces))

}


