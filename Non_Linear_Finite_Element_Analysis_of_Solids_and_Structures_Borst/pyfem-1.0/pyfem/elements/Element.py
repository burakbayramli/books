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

from numpy import outer, ones, zeros
from pyfem.materials.MaterialManager import MaterialManager

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

class Element ( list ):

  dofTypes = []

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def __init__ ( self, elnodes , props ):
    list.__init__( self, elnodes )

    self.history = {}
    self.current = {}

    for name,val in props:
      if name is "material":
        self.matProps = val
        self.mat = MaterialManager( self.matProps )
      else:
        setattr( self, name, val )

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def dofCount ( self ):

    return len( self ) * len( self.dofTypes )

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getNodes ( self ):
    return self

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getType ( self ):
    return self.elemType

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def appendNodalOutput ( self, outputName, globdat, outmat, outw = None ):

    if outw == None:
      outw = ones( outmat.shape[0] )

    if not hasattr( globdat, outputName ):

      globdat.outputNames.append( outputName )

      setattr( globdat, outputName, zeros( ( len(globdat.nodes), outmat.shape[1] ) ) )
      setattr( globdat, outputName + 'Weights', zeros( len(globdat.nodes) ) )

    outMat     = getattr( globdat, outputName )
    outWeights = getattr( globdat, outputName + 'Weights' )

    if outmat.shape[1] != outMat.shape[1] or outmat.shape[0] != len(self):
      raise RuntimeError("Appended output vector has incorrect size.")

    indi = globdat.nodes.getIndices( self )

    outMat[ indi ]     += outer( outw, ones(outmat.shape[1]) ) * outmat
    outWeights[ indi ] += outw

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def setHistoryParameter ( self, name, val ):
    self.current[name] = val

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def getHistoryParameter ( self, name ):
    return self.history[name]

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def commitHistory ( self ):
    self.history = self.current.copy()
    self.current = {}

    if hasattr( self , "mat" ):
      self.mat.commitHistory()
