function D_PHI = d_phi(TT,Parmeter,d3,D3)
T1    = Parmeter(1); T3   = Parmeter(2);
gl    = Parmeter(3); m    = Parmeter(4);
if d3^2 ~= D3^2
   D_PHI = (d3 - D3*cos(TT))./(T1*sin(TT).^2);
end
if d3 == D3
   D_PHI = d3./(T1*(1 + cos(TT)));
end
if d3 == - D3
   D_PHI = d3./(T1*(1 - cos(TT)));
end
