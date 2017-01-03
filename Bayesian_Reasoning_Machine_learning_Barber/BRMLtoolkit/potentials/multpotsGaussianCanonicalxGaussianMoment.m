function newpot=multpotsGaussianCanonicalxGaussianMoment(pot1,pot2)
tmppot=convertGaussianMomentToGaussianCanonical(pot2);
newpot=multpotsGaussianCanonical(pot1,tmppot);