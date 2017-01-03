function cosine
%COSINE Illustrates cosine formula and dot products.
%
%       cosine is an interactive code to illustrate the cosine formula
%       and dot products.  The user is asked to click on a graph to define
%       vectors u and v.  The cosine of the angle between them
%       is computed by the formula cosine = u'*v/(norm(u)*norm(v)).

%       Written by T. Bryan on 27 August 1993 to illustrate G. Strang's
%       book, "Introduction to Linear Algebra." 

clc
disp('This code illustrates the cosine formula and dot products')
disp('found on p. 16 of G. Strang, "Introduction to Linear Algebra."')

clf
axis([-10 10 -10 10]);axis('square')
plot(0,0); hold on
axis([-10 10 -10 10]);axis('square')
button = 1;
fprintf('\n\n')
disp('OPEN A FIGURE WINDOW')
disp('Click at two points on the plot with the left button.')
disp(' ')
disp('Press any other key or button twice to exit.')
[x,y,button]=ginput(2);
clc
while button==1
  u=[x(1);y(1)];
  v=[x(2);y(2)];
  plot([0 u(1)],[0 u(2)],'-',[0 v(1)],[0 v(2)],'-')
  text(u(1),u(2),'u')
  text(v(1),v(2),'v')
  axis([-10 10 -10 10]);axis('square')

  fprintf('\n\nThe cosine of the angle between the two vectors is\n')
  cosine = u'*v/(norm(u)*norm(v));
  fprintf('cosine = u''*v/(norm(u)*norm(v)) = %g\n\n',cosine)
  disp('The angle between the two vectors in radians is')
  fprintf('acos(cosine) = %g\n\n',acos(cosine))
  disp('The angle between the two vectors in degrees is')
  fprintf('180*acos(cosine)/pi = %g\n\n',180*acos(cosine)/pi)
  pause(3)
  
  disp('Click twice on the plot with the left button.')
  disp('Press any other key or button twice to exit.')
  [x,y,button]=ginput(2);
  clc
  clf
  axis([-10 10 -10 10]);axis('square')
  plot(0,0); hold on
  axis([-10 10 -10 10]);axis('square')
end



