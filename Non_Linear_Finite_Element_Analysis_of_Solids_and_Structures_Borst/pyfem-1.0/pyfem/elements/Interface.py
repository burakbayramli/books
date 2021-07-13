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
from scipy.linalg import norm

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

class Interface( Element ):

  dofTypes = [ 'u' , 'v' ]
  
  def __init__ ( self, elnodes , props ):

    self.intMethod = "NewtonCotes"

    Element.__init__( self, elnodes , props )

    #Initialize the history parameter

    self.setHistoryParameter( 'normal' , zeros(2) )
  
    self.commitHistory()

  def __type__ ( self ):
    return name

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getTangentStiffness ( self, elemdat ):

    rot = self.getRotation( elemdat.coords , elemdat.state )

    sData = getElemShapeData( elemdat.coords[:2,:] , method = self.intMethod , elemType = "Line2" )
    
    elemdat.outlabel.append("tractions")
    elemdat.outdata  = zeros( shape=(len(elemdat.nodes),2) )

    kin = Kinematics(2,2)

    for iData in sData:
      B              = self.getBmatrix( iData.h , rot )
      kin.strain     = dot( B , elemdat.state )

      sigma,tang = self.mat.getStress( kin )
     
      elemdat.stiff += dot ( B.transpose() , dot ( tang , B ) ) * iData.weight
      elemdat.fint  += dot ( B.transpose() , sigma ) * iData.weight
      
      elemdat.outdata += outer( ones(len(elemdat.nodes)), sigma )
  
#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getInternalForce ( self, elemdat ):
   
    rot = self.getRotation( elemdat.coords , elemdat.state )

    sData = getElemShapeData( elemdat.coords[:2,:] , method = self.intMethod , elemType = "Line2" )
    
    elemdat.outlabel.append("tractions")
    elemdat.outdata  = zeros( shape=(len(elemdat.nodes),2) )

    kin = Kinematics(2,2)

    for iData in sData:
      B              = self.getBmatrix( iData.h , rot )
      kin.strain     = dot( B , elemdat.state )

      sigma,tang = self.mat.getStress( kin )
     
      elemdat.fint  += dot ( B.transpose() , sigma ) * iData.weight
      
      elemdat.outdata += outer( ones(len(elemdat.nodes)), sigma )

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getBmatrix( self , phi , rot ):

    B = zeros( shape=( 2 , self.dofCount() ) )

    B[:,:2]  = -rot * phi[0]
    B[:,2:4] = -rot * phi[1]
    B[:,4:6] =  rot * phi[0]
    B[:,6:]  =  rot * phi[1]

    return B

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getRotation( self , coords , state ):

    rot = zeros( shape=(2,2) )

    midCoords = zeros( shape=(2,2) )
    midCoords = 0.5 * ( coords[:2,:] + coords[2:,:] )

    midCoords[0,0] += 0.5 * ( state[0] + state[4] )
    midCoords[0,1] += 0.5 * ( state[1] + state[5] )
    midCoords[1,0] += 0.5 * ( state[2] + state[6] )
    midCoords[1,1] += 0.5 * ( state[3] + state[7] )

    ds = midCoords[1,:]-midCoords[0,:]

    normal = self.getHistoryParameter('normal')

    if norm(normal) < 0.5:
      normal[0] = ds[1]/norm(ds)
      normal[1] = ds[0]/norm(ds)
    else:
      newnormal = zeros(2)
      newnormal[0] = ds[1]/norm(ds)
      newnormal[1] = ds[0]/norm(ds)

      if dot(newnormal,normal) < 0 :
        normal = -newnormal
      else:
        normal = newnormal

    self.setHistoryParameter( 'normal' , normal )
    
    rot[0,0]=  normal[0]
    rot[0,1]=  normal[1]
    rot[1,0]=  normal[1]
    rot[1,1]= -normal[0]

    return rot


