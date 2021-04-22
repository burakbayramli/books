function y = bumpy(x)
% our first function M-file
% x could be a vector
% created by <yourname> on <date>
y=1/(4*pi)*(1./((x-2).^2+1)+1./((x+.5).^4+32)+1./((x+1).^2+2));
