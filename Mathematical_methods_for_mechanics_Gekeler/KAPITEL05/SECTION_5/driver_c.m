function [antc,antp,ants,antm,muefix,tol] = driver_c(muefix,tol);
disp('interactive driver');
ant = 2;
while ~((ant == 0) | (ant == 1))
   muefix
   ant = input('new mue (0/1)? ');
   if isempty(ant), ant = 0; end;
end;
if ant == 1, muefix = input('new mue = ? '); end
ant = 2;
while ~((ant == 0) | (ant == 1))
   tol
   ant = input('new tol (0/1)? ');
   if isempty(ant), ant = 0; end;
end;
if ant == 1, tol = input('new tol = ? '); end;
antm = 2;
%while ~((antm == 0) | (antm == 1))
%   antm = input('meshrefinement (0/1)? ');
%   if isempty(antm), antm = 0; end;
%end;
ants = 2;
while ~((ants == 0) | (ants == 1))
   ants = input('save values (0/1)? ');
   if isempty(ants), ants = 0; end;
end;
antp = 2;
while ~((antp == 0) | (antp == 1))
   antp = input('plot (0/1)? ');
   if isempty(antp), antp = 0; end;
end;
antc = 2;
while ~((antc == 0) | (antc == 1))
   antc = input('continuation (0/1)? ');
   if isempty(antc), antc = 0; end;
end;
