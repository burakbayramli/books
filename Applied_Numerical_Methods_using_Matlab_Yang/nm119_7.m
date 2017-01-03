%nm119_7: example of while loop
r=1;
while r<10
   r= input('\nType radius (or press Enter to stop):');
   if r<=0, break, end %isempty(r)|r<=0, break, end
   v= 4/3*pi*r*r*r;
   fprintf('The volume of a sphere with radius %3.1f  is %8.2f\n',r,v);
end