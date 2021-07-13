############################################################################
#  This Python file is part of PyFEM-1.0, released on Aug. 29, 2012.       #
#  The PyFEM code accompanies the book:                                    #
#                                                                          #
#    'Non-Linear Finite Element Analysis of Solids and Structures'         #
#    R. de Borst, M.A. Crisfield, J.J.C. Remmers and C.V. Verhoosel        #
#    John Wiley and Sons, 2012, ISBN 978-0470666449                        #
#                                                                          #
#  The code is written by J.J.C. Remmers, C.V. Verhoosel and R. de Borst.  #
#  Comments and suggestions can be sent to:                                #
#     PyFEM-support@tue.nl                                                 #
#                                                                          #
#  The latest version can be downloaded from the web-site:                 #                                                                          
#     http://www.wiley.com/go/deborst                                      #
#                                                                          #
#  The code is open source and intended for educational and scientific     #
#  purposes only. If you use PyFEM in your research, the developers would  #
#  be grateful if you could cite the book.                                 #  
#                                                                          #
#  Disclaimer:                                                             #
#  The authors reserve all rights but do not guarantee that the code is    #
#  free from errors. Furthermore, the authors shall not be liable in any   #
#  event caused by the use of the program.                                 #
############################################################################

from .Element import Element
from pyfem.util.shapeFunctions  import getElemShapeData
from pyfem.util.kinematics      import Kinematics

from numpy import zeros, dot, outer, ones, eye, sqrt
from scipy.linalg import eigvals

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

class FiniteStrainContinuum( Element ):

  #dofs per element
  dofTypes = [ 'u' , 'v' ]
  
  def __init__ ( self, elnodes , props ):
    Element.__init__( self, elnodes , props )
    
  def __type__ ( self ):
    return name

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getTangentStiffness ( self, elemdat ):

    n = self.dofCount()

    sData = getElemShapeData( elemdat.coords )
    
    elemdat.outlabel.append("stresses")
    elemdat.outdata  = zeros( shape=(len(elemdat.nodes),3) )
   
    for iData in sData:

      kin = self.getKinematics( iData.dhdx , elemdat.state ) 
      B   = self.getBmatrix   ( iData.dhdx , kin.F )
      
      sigma,tang = self.mat.getStress( kin )
        
      elemdat.stiff += dot ( B.transpose() , dot ( tang , B ) ) * iData.weight

      T   = self.stress2matrix( sigma )
      Bnl = self.getBNLmatrix ( iData.dhdx )
   
      elemdat.stiff += dot ( Bnl.transpose() , dot( T , Bnl ) ) * iData.weight
      elemdat.fint  += dot ( B.transpose() , sigma ) * iData.weight
      
      elemdat.outdata += outer( ones(len(elemdat.nodes)), sigma )
      
    elemdat.outdata *= 1.0 / len(sData)  

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getInternalForce ( self, elemdat ):
   
    n = self.dofCount()
   
    sData = getElemShapeData( elemdat.coords )

    elemdat.outlabel.append("stresses")
    elemdat.outdata  = zeros( shape=(len(elemdat.nodes),3) )

    for iData in sData:
            
      kin = self.getKinematics( iData.dhdx , elemdat.state ) 
      B   = self.getBmatrix   ( iData.dhdx , kin.F )
      
      sigma,tang = self.mat.getStress( kin )
       
      elemdat.fint    += dot ( B.transpose() , sigma ) * iData.weight
      elemdat.outdata += outer( ones(len(elemdat.nodes)), sigma )
      
    elemdat.outdata *= 1.0 / len(sData)  

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getMassMatrix ( self, elemdat ):
      
    sData = getElemShapeData( elemdat.coords )

    rho = elemdat.matprops.rho

    for iData in sData:
      N  = self.getNmatrix( iData.h )
      elemdat.mass += dot ( N.transpose() , N ) * rho * iData.weight
     
    elemdat.lumped = sum(elemdat.mass)
            
#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getKinematics( self , dphi , elstate ):
  
    kin = Kinematics(2,3)

    kin.F = eye(2)
  
    for i in range(len(dphi)):
      for j in range(2):
        for k in range(2):
          kin.F[j,k] += dphi[i,k]*elstate[2*i+j]

    kin.E = 0.5*(dot(kin.F.transpose(),kin.F)-eye(2))

    kin.strain[0] = kin.E[0,0]
    kin.strain[1] = kin.E[1,1]
    kin.strain[2] = 2.0*kin.E[0,1]
    
    return kin

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getBmatrix( self , dphi , F ):

    B = zeros( shape=( 3 , 2*len(dphi) ) )

    for i,dp in enumerate( dphi ):
      B[0,2*i  ] = dp[0]*F[0,0]
      B[0,2*i+1] = dp[0]*F[1,0]
	
      B[1,2*i  ] = dp[1]*F[0,1]
      B[1,2*i+1] = dp[1]*F[1,1]

      B[2,2*i  ] = dp[1]*F[0,0]+dp[0]*F[0,1]
      B[2,2*i+1] = dp[0]*F[1,1]+dp[1]*F[1,0]
   
    return B

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def stress2matrix( self , stress ):

    T = zeros( shape=( 4 , 4 ) )

    T[0,0] = stress[0]
    T[1,1] = stress[1]
    T[0,1] = stress[2]
    T[1,0] = stress[2]

    T[2:,2:] = T[:2,:2]
       
    return T

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getBNLmatrix( self , dphi ):

    Bnl = zeros( shape=( 4 , 2*len(dphi) ) )

    for i,dp in enumerate( dphi ):
      Bnl[0,2*i  ] = dp[0]
      Bnl[1,2*i  ] = dp[1]
      Bnl[2,2*i+1] = dp[0]
      Bnl[3,2*i+1] = dp[1]
   
    return Bnl

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getNmatrix( self , h ):

    N = zeros( shape=( 2 , 2*len(h) ) )

    for i,a in enumerate( h ):
      N[0,2*i  ] = a
      N[1,2*i+1] = a
   
    return N
