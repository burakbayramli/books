function V_EFF = v_eff(TT,Parmeter,d3,D3)
T1    = Parmeter(1); T3   = Parmeter(2);
gl    = Parmeter(3); m    = Parmeter(4);
if d3^2 ~= D3^2
   V_EFF = (d3 - D3*cos(TT)).^2./(2*T1*sin(TT).^2) ...
         + m*gl*cos(TT) + D3^2/(2*T3);
end
if d3 == D3
   V_EFF = D3^2*(1 - cos(TT))./(2*T1*(1 + cos(TT))) ...
         + m*gl*cos(TT) + D3^2/(2*T3);
end
if d3 == - D3
   V_EFF = d3^2*(1 + cos(TT))./(2*T1*(1 - cos(TT))) ...
         + m*gl*cos(TT) + D3^2/(2*T3);
end
