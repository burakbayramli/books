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

from pyfem.util.dataStructures import Properties

class MaterialManager ( list ):

  def __init__ ( self, matProps ):

    matType = matProps.type

    cmdStr = 'from pyfem.materials.' + matType + ' import ' + matType + ' as material'

    exec cmdStr
    
    self.mat = material( matProps )
    self.iIter = -1

  def reset( self ):

    self.iIter  = -1

  def getStress ( self, kinematic , iSam = -1 ):

    if iSam == -1:
      self.iIter += 1
      iSam = self.iIter

    self.mat.setIter( iSam )
     
    return self.mat.getStress( kinematic )

  def commitHistory( self ):
    self.mat.commitHistory()
