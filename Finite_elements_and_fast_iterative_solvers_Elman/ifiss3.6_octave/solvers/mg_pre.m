function xs = mg_pre(A,xs0,f,ns,Qs,level,sweeps)
%MG_PRE presmoothing for GMG
%   mg_pre(A,xs0,f,ns,Qs,level,sweeps)
%   input
%          A            coefficient matrix 
%          xs0          initial iterate
%          f            right-hand side
%          ns           number of smoothing steps
%          Qs           structure containing smoothing operators 
%          level        grid level
%          sweeps       type of sweeping strategy used for Gauss-Seidel
%                       smoothing
%   output
%          xs           result of presmoothing applied to xs0
%
%   matched with mg_post, which performs multi-directional
%   Gauss-Seidel smoothing in reverse order
%
%   IFISS function: AR; 28 January 2003.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage
xs=xs0;
if ns==0, return; end
%
L1 = Qs(level).L1;  U1 = Qs(level).U1;
L2 = Qs(level).L2;  U2 = Qs(level).U2;
L3 = Qs(level).L3;  U3 = Qs(level).U3;
L4 = Qs(level).L4;  U4 = Qs(level).U4;
%
r = f - A*xs;
xs = xs + U1\(L1\r);   r = f - A*xs;
if sweeps>=2
   xs = xs + U2\(L2\r);   r = f - A*xs;
   if sweeps>=3
      xs = xs + U3\(L3\r);   r = f - A*xs;
      if sweeps==4
         xs = xs + U4\(L4\r);   
      end
   end
end
%
if ns>1,
   k = 1;
   while k<ns,
      r = f - A*xs;
      xs = xs + U1\(L1\r);   r = f - A*xs;
      if sweeps>=2
         xs = xs + U2\(L2\r);   r = f - A*xs;
         if sweeps>=3
            xs = xs + U3\(L3\r);   r = f - A*xs;
         if sweeps==4
            xs = xs + U4\(L4\r);     
            end
         end
      end
      k = k + 1;   
   end
end
