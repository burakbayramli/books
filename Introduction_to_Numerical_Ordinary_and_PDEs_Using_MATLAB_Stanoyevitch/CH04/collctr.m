function n = collctr(an)
n=0;
while an ~= 1
   if ceil(an/2)==an/2  %tests if an is even
      an=an/2;
   else
      an=3*an+1;
   end
   n=n+1;
end
   