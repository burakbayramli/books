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
from numpy import zeros, dot, outer, ones , eye

class SmallStrainContinuum( Element ):

  #dofs per element
  dofTypes = [ 'u' , 'v' ]
  
  def __init__ ( self, elnodes , props ):
    Element.__init__( self, elnodes , props )

  def __type__ ( self ):
    return name

#------------------------------------------------------------------------

  def getTangentStiffness ( self, elemdat ):

    sData = getElemShapeData( elemdat.coords )

    kin = Kinematics(2,3)
    
    elemdat.outlabel.append("stresses")
    elemdat.outdata  = zeros( shape=(len(elemdat.nodes),3) )

    for iData in sData:
      
      b = self.getBmatrix( iData.dhdx )

      kin.strain  = dot ( b , elemdat.state )
      kin.dstrain = dot ( b , elemdat.Dstate )
      
      sigma,tang = self.mat.getStress( kin )

      elemdat.stiff += dot ( b.transpose() , dot ( tang , b ) ) * iData.weight
      elemdat.fint  += dot ( b.transpose() , sigma ) * iData.weight

      elemdat.outdata += outer( ones(len(elemdat.nodes)), sigma )
    
    elemdat.outdata *= 1.0 / len(sData) 

  
     
#-------------------------------------------------------------------------

  def getInternalForce ( self, elemdat ):
      
    sData = getElemShapeData( elemdat.coords )

    elemdat.outlabel.append("stresses")
    elemdat.outdata  = zeros( shape=(len(elemdat.nodes),3) )

    kin = Kinematics(2,3)

    for iData in sData:
      b = self.getBmatrix( iData.dhdx )

      kin.strain  = dot ( b , elemdat.state )
      kin.dstrain = dot ( b , elemdat.Dstate )

      sigma,tang = self.mat.getStress( kin )

      elemdat.fint    += dot ( b.transpose() , sigma ) * iData.weight
      elemdat.outdata += outer( ones(len(self)), sigma )
      
    elemdat.outdata *= 1.0 / len(sData)  

#----------------------------------------------------------------------
    
  def getMassMatrix ( self, elemdat ):
      
    sData = getElemShapeData( elemdat.coords )

    rho = self.rho * eye(2)

    for iData in sData:
      N  = self.getNmatrix( iData.h )
      Nt = N.transpose()

      elemdat.mass += dot ( Nt , dot( rho , N ) ) * iData.weight
      
    elemdat.lumped = sum(elemdat.mass)
   
#--------------------------------------------------------------------------

  def getBmatrix( self , dphi ):

    b = zeros( shape=( 3 , self.dofCount() ) )

    for i,dp in enumerate(dphi):
      b[0,i*2  ] = dp[0]
      b[1,i*2+1] = dp[1]
      b[2,i*2  ] = dp[1]
      b[2,i*2+1] = dp[0]
   
    return b

#------------------------------------------------------------------------------

  def getNmatrix( self , h ):

    N = zeros( shape=( 2 , 2*len(h) ) )

    for i,a in enumerate( h ):
      N[0,2*i  ] = a
      N[1,2*i+1] = a
   
    return N
