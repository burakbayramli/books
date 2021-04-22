% Integration using 5 point Gaussian quadrature
gaussPoints=[-0.906179845938664, -0.5384693101056831, 0.,... 
    0.5384693101056831, 0.906179845938664];

gaussWeights=[0.2369268850561894, 0.47862867049936625, 0.5688888888888889,...
    0.47862867049936625, 0.2369268850561894];
int=0;
for i=1:length(gaussWeights)
    s = gaussPoints(i);
    fs = exp(s)*sin(s)/(1+s^2);
    int = int + gaussWeights(i)*fs;
end
int