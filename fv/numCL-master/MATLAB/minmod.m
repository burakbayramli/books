function m = minmod(a,b,c)

m = (sign(a)==sign(b) & sign(b)==sign(c) & sign(c)==sign(a)).*sign(a).*min(abs([a b c]),[],2);

return