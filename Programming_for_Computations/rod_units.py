from PhysicalQuantities import PhysicalQuantity as PQ
rho = PQ('2.7E+3 kg/m**3')
kappa = PQ('200 W/(m*K)')
c = PQ('900 J/(K*kg)')
beta = kappa/(rho*c)
beta = PQ('%g m**2/s' % beta.getValue())
print beta
