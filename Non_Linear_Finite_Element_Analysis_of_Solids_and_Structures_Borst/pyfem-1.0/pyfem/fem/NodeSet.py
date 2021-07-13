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

from numpy import array
from pyfem.util.itemList import itemList
import re

class NodeSet( itemList ):

  def getNodeCoords( self, nodeIDs ):
    return array( self.get( nodeIDs ) )
    
  def readFromFile( self, fname ):
    
    fin = open( fname )
  
    while True:
    
      line = fin.readline()  
  
      if line.startswith('<Nodes>') == True:
      
        while True:
          line = fin.readline()  

          if line.startswith('</Nodes>') == True:
            return
  
          line = re.sub('\s{2,}',' ',line)
          a = line.split(';')
     
          for a in a[:-1]:
            b = a.strip().split(' ')
            
            if b[0].startswith("//") or b[0].startswith("#"):
              break
            if len(b) > 1 and type(eval(b[0])) == int:
              self.add( eval(b[0]), [eval(crd) for crd in b[1:]] )          




