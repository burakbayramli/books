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

from pyfem.materials.BaseMaterial import BaseMaterial
from numpy import zeros, dot

class MixedModeCohzone( BaseMaterial ):

  def __init__ ( self, props ):

    self.eta = 2.0

    #Call the BaseMaterial constructor
    BaseMaterial.__init__( self, props )

    self.dnorm    = self.Tnorm / self.dummy
    self.dshear   = self.Tshear / self.dummy

#-------------
#
#------------------------------

  def getStress( self, deformation ):

    deformation.sigma = zeros(2)
    deformation.tang  = zeros( shape=(2,2) )

    jump = deformation.strain

    lam = sqrt( macauley(jump[0]) * macauley( jump[0] ) + jump[1] * jump[1] )
    
    delta_shear = abs(jump[1]) 

    if macauley(jump[0]) == 0.0:
      beta = 1.0
    elif jump[0] == 0.0 and deltaMax == 0.0:
      beta = 0.0
    else:
      beta = delta_shear / ( delta_shear + macauley(jump[0]) )
 
    B = pow( beta * beta / ( 1.0 + 2.0 * beta * beta - 2.0 * beta) , self.eta )
   
    d0 = sqrt ( self.dnorm * self.dnorm + ( self.dshear * self.dshear - self.dnorm * self.dnorm )*B )
    df = 2.0 * ( self.GIc + B * ( self.GIIc - self.GIc ) ) / ( self.dummy * d0 )
    
    if lam > df:
      lam = df

    if lam >= deltaMax:
      deltaMax = lam
      
    #-------------Elastic regime

      if lam < d0: 
        
        trac      = self.dummy * jump
        tang[0,0] = self.dummy
        tang[1,1] = self.dummy
    
      elif lam >= d0 and lam < df:
         
        damage = df * ( lam - d0 )/( lam * ( df-d0 ))
        H      = df * d0 /(( df - d0) * lam * lam * lam )
   
        if damage >= 1.0:
          damage=1.0
 
        deformation.sigma[0] = ( 1.0 - damage ) * self.dummy * jump[0] - damage*self.dummy*macauley( -1.0 * jump[0] )
        deformation.sigma[1] = ( 1.0 - damage ) * self.dummy * jump[1]
               
        deformation.tang[0,0] = self.dummy * ( 1.0 - damage * ( 1.0 + sign( jump[0] ) ) )
        deformation.tang[1,1] = self.dummy * ( 1.0 - damage )
         
        deformation.tang[0,0] +=  -self.dummy * H * jump[0] * jump[0] * ( 1.0 + sign(jump[0]) ) * ( 1.0 + sign( jump[0] ) )
        deformation.tang[0,1] +=  -self.dummy * H * jump[0] * jump[1] * ( 1.0 + sign(jump[0]) )          
        deformation.tang[1,0] +=  -self.dummy * H * jump[1] * jump[0] * ( 1.0 + sign(jump[0]) )
        deformation.tang[1,1] +=  -self.dummy * H * jump[1] * jump[1]

      else:
        damage = 1.0
      
    else:
      
      deformation.trac[0] = (1.0-damage)*self.dummy*jump[0] - damage*self.dummy*macauley( -1.0 * jump[0] ) ;
      deformation.trac[1] = (1.0-damage)*self.dummy*jump[1] ;
   
      deformation.tang[0,0] = ( 1.0 - damage *( 1.0 + sign( jump[0] ) ) ) * self.dummy ;
      deformation.tang[1,1] = ( 1.0 - damage ) * self.dummy;
