%nm119_6: example of for loop
point= [76 85 91 65 87];
for n=1:length(point)
   if point(n)>=80,  pf(n,:)= 'pass';
     elseif point(n)>=0, pf(n,:)= 'fail';
     else  %if point(n)<0
     pf(n,:)= '????';
     fprintf('\n\a Something wrong with the data??\n');
     break;
   end
end
pf