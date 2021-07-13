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

from pyfem.util.BaseModule import BaseModule

class ContourWriter( BaseModule ):
 
  def __init__( self , props , globdat ):

    self.prefix       = globdat.prefix
    self.interval     = 1
    self.k            = 0

    self.stresslabels = [ "sxx" , "syy" , "sxy" ]

    BaseModule.__init__( self , props )

    self.columndata = []

    for i,col in enumerate ( self.columns ):

      self.columndata.append( colProps )
	
  def run( self , props , globdat ):

    if not globdat.cycle%self.interval == 0:
      return
      
    outfile = open( self.prefix + '-contour-' + str(self.k) + '.gnu' ,'w' )
        
    stress = globdat.getData( "stresses" , range(len(globdat.nodes)) )

    for iNod in self.nodes:
      crd = globdat.nodes.getNodeCoords(iNod)
      outfile.write( str(crd[0]) + ' ' + str(crd[1]) + ' ' )
       
      for dofType in globdat.dofs.dofTypes:
        outfile.write(str(globdat.state[globdat.dofs.getForType(iNod,dofType)])+' ' )
          
      outfile.write(str(stress[iNod][0])+' '+str(stress[iNod][1])+' '+str(stress[iNod][2]))
        
      outfile.write('\n')  
        
    outfile.close()
  
    self.k = self.k+1
