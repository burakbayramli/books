% Exercise 3.1.b:  (compare Lecture-Slides 2, Slide 13)
function w = getVectorFromRotationMatrix(R)
  length_w = acos((trace(R)-1)/2);
  if (length_w == 0)
      w = [0 0 0]';
  else
      w = 1/(2*sin(length_w))*[R(3,2)-R(2,3) R(1,3)-R(3,1) R(2,1)-R(1,2)]'*length_w;
  end
end