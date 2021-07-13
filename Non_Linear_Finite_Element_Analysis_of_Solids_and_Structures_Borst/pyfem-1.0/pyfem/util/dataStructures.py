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

from numpy import zeros

class Properties:
  
  def __init__ ( self, dictionary = {} ):

    for key in dictionary.iterkeys():
      setattr( self, key, dictionary[key] )

  def __str__ ( self ):

    myStr  = ''
    for att in dir( self ):
      
      #Ignore private members and standard routines
      if att.startswith('__'):
        continue
      
      myStr += 'Attribute: ' + att + '\n'
      myStr += str( getattr(self,att) ) + '\n'

    return myStr

  def __iter__ ( self ):

    propsList = []
    for att in dir( self ):
      
      #Ignore private members and standard routines
      if att.startswith('__'):
        continue
      
      propsList.append( ( att, getattr(self,att) ) )

    return iter(propsList)

  def store ( self , key , val ):
    setattr( self , key , val )

class GlobalData ( Properties ):
  
  def __init__ ( self, nodes, elements, dofs ):

    Properties.__init__( self, { 'nodes' : nodes, 'elements' : elements, 'dofs' : dofs } )

    self.state  = zeros( len( self.dofs ) )
    self.Dstate = zeros( len( self.dofs ) )
    self.fint   = zeros( len( self.dofs ) )
    self.fhat   = zeros( len( self.dofs ) )

    self.velo   = zeros( len( self.dofs ) )
    self.acce   = zeros( len( self.dofs ) )

    self.cycle  = 0
    self.iiter  = 0
    self.time   = 0.0
   
    self.outputNames = []

  def readFromFile( self , fname ):

    fin = open( fname )
  
    while True:
      line = fin.readline()  
  
      if line.startswith('<ExternalForces>') == True:
        while True:
          line = fin.readline()  

          if line.startswith('</ExternalForces>') == True:
            return
        
          a = line.strip().split(';')
          
          if len(a) == 2:
            b = a[0].split('=')
        
            if len(b) == 2:
              c = b[0].split('[')
              
              dofType = c[0]
              nodeID  = eval(c[1].split(']')[0])
              
              self.fhat[self.dofs.getForType(nodeID,dofType)] = eval(b[1])

#---------------------------------------------------------------------------------
#
#---------------------------------------------------------------------------------

  def printNodes( self , inodes=None ):

    if inodes is None:
      inodes = self.nodes.keys() 
	
    print '   Node | ',
    
    for dofType in self.dofs.dofTypes:
      print "  '%s'        " % dofType,

    if hasattr( self , 'fint' ):
      for dofType in self.dofs.dofTypes:
        print " fint'%s'   " % dofType,

    for name in self.outputNames:
      print "       '%s'             " % name,

    print 
    print '-------------------------------------------------------',
    print '-------------------------------------------------------'

    for nodeID in inodes:
      print '  %4i  | ' % (nodeID+1),
      for dofType in self.dofs.dofTypes:
        print ' %10.3e ' % self.state[self.dofs.getForType(nodeID,dofType)],
      for dofType in self.dofs.dofTypes:
        print ' %10.3e ' % self.fint[self.dofs.getForType(nodeID,dofType)],

      for name in self.outputNames:
        data = self.getData( name , nodeID )
      
        for a in data:
          print ' %10.3e ' % a,
      print
    print

#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------
      
  def getData( self , outputName , inodes ):

    data    = getattr( self, outputName )
    weights = getattr( self, outputName + 'Weights' )

    if type(inodes) is int:
      i = self.nodes.keys().index(inodes)
      return data[i,:] / weights[i]
    else:
      outdata=[]

      for row,w in zip(data[inodes,:],weights[inodes]):
        if w != 0:
          outdata.append(row/w)
        else:
          outdata.append(row)

      return outdata

#---------------	------------------------------------------------------

  def resetNodalOutput ( self ):

    for outputName in self.outputNames:
      delattr( self , outputName )
      delattr( self , outputName + 'Weights' )

    self.outputNames=[]

#-----------------------------------------------------

class elementData():

  def __init__( self , elstate , elDstate ):

    nDof        = len(elstate)

    self.state  = elstate
    self.Dstate = elDstate
    self.stiff  = zeros( shape=( nDof,nDof ) )
    self.fint   = zeros( shape=(nDof) )
    self.mass   = zeros( shape=( nDof,nDof ) )
    self.lumped = zeros( shape=(nDof) )

    self.outlabel = []
   
  def __str__( self ):

    return self.state
     



