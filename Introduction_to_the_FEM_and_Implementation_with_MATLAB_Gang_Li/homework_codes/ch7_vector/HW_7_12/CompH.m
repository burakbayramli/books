% Set up H matrix
function H= CompH()
H=zeros(3,4);
H(1,1)=1;  H(2,4)=1;
H(3,2)=1;  H(3,3)=1;