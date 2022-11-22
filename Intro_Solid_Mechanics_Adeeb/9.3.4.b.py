import sympy as sp
from sympy import Matrix
s = Matrix([[20,30,0],[30,-10,0],[0,0,25]])
display("\u03C3 =",s)
Nu = 0.1
Ee = 10000
G = Ee/2/(1+Nu)
U = (1+Nu)/2/Ee*(sum([s[i]*s[i] for i in range(9)]))-Nu/2/Ee*(
    sum([s[i,i] for i in range(3)]))**2 
display("U =",U)
p = sum([s[i,i] for i in range(3)])/3
display("p =",p)
Udeviatoric = 1/12/G*((s[0,0]-s[1,1])**2 + (s[2,2]- 
      s[1,1])**2 + (s[0,0] - s[2,2])**2 + 
   6*(s[0,1]**2 + s[0,2]**2 + s[1,2]**2))
display("U_deviatoric =",Udeviatoric)
Uvolumetric = 3*(1-2*Nu)/2/Ee*p**2
display("U_volumetric",Uvolumetric)
