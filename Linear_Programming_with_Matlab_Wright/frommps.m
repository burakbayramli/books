function [A,b,c,lbounds,ubounds] = frommps(pname)
% FROMMPS read in mps file from mpslib directory

MPSNAME = [pname '.mps'];

if exist([MPSNAME '.gz']) 
   unix(['gunzip -c ' MPSNAME '.gz > in.mps']);
elseif exist([MPSNAME '.Z'])
   unix(['uncompress -c ' MPSNAME '.Z > in.mps']);
elseif exist(MPSNAME) 
   unix(['/bin/cp ' MPSNAME ' in.mps']);
end
if exist('in.mps')
   [ignore, UNAME] = unix('uname'); last = length(UNAME);
   if abs(UNAME(last)) == 10 UNAME = UNAME(1:last-1); end
   MPS2MAT = ['mps2mat.' UNAME];
   unix([MPS2MAT ' >/dev/null; /bin/rm in.mps']);
   load default;
   return;
end

