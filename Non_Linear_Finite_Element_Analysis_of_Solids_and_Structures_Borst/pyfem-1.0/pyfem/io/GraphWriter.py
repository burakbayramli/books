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
from pylab import plot, show, xlabel, ylabel, draw, ion, figure
from numpy import ndarray,zeros

class GraphWriter( BaseModule ):

  def __init__ ( self, props , globdat ):

    self.prefix    = globdat.prefix
    self.extension = ".out"
    self.onScreen  = False

    BaseModule.__init__( self , props )

    if not hasattr( self , "filename" ):
      self.filename  = self.prefix + self.extension
    
    self.columndata = []

    for i,col in enumerate ( self.columns ):

      colProps = getattr( self , col )
      
      if not hasattr( colProps , "factor" ):
        colProps.factor = 1.0

      self.columndata.append( colProps )

    if self.onScreen and hasattr( globdat , "onScreen" ):
      self.onScreen = False
    else:
      globdat.onScreen = True

    self.outfile = open( self.filename ,'w' )

    if self.onScreen:
      self.output = []

      xlabel(self.columns[0])
      ylabel(self.columns[1])
  
      ion()

    self.run( props , globdat ) 

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------

  def run( self , props , globdat ):

    a = []

    for i,col in enumerate(self.columndata):
      if col.type in globdat.outputNames:
        data = globdat.getData( col.type , col.node )
      elif hasattr(globdat,col.type):
        b = getattr( globdat , col.type )
        if type(b) is ndarray:
          data = b[globdat.dofs.getForType(col.node,col.dof)]
        else:
          data = b
      else:
        data = globdat.getData( col.type , col.node )
        
      data = data * col.factor

      a.append(data)
   
      self.outfile.write(str(data)+' ',)

    self.outfile.write('\n')

    if self.onScreen: 
      self.output.append( a )

      plot( [x[0] for x in self.output], [x[1] for x in self.output], 'ro-' )

      draw()
    
    if not globdat.active:
      self.outfile.close
