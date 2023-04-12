l=90;
E=1080000;
w=120;
mu=0.25;
t=0.025;
P0=20;
x1_1=0;
y1_1=0;
x2_1=l;
y2_1=0;
x3_1=l;
y3_1=w;
b1_1=-w;
b2_1=w;
b3_1=0;
c1_1=0;
c2_1=-l;
c3_1=l;
K1=Kstif2Dtriangle( E,l,w,mu,x1_1,y1_1,x2_1,y2_1,x3_1,y3_1,b1_1,b2_1,b3_1,c1_1,c2_1,c3_1 )
%element 2
x1_2=0;
y1_2=0;
x2_2=l;
y2_2=w;
x3_2=0;
y3_2=w;
b1_2=0;
b2_2=w;
b3_2=-w;
c1_2=-l;
c2_2=0;
c3_2=l;
K2=Kstif2Dtriangle( E,l,w,mu,x1_2,y1_2,x2_2,y2_2,x3_2,y3_2,b1_2,b2_2,b3_2,c1_2,c2_2,c3_2 )
%connectivity matrix
c=zeros(2,3);
c(1,1)=1;

c(1,2)=2;
c(1,3)=3;
c(2,1)=1;
c(2,2)=3;
c(2,3)=4;
%global stiffness matrix
neln =3; %number of nodes per element
ndof =2; %number of DOFs per node
el_stif =K1; %current element stiffness matrix
nelem =2; %number of elements
gStif=zeros(8,8);%global stiffness matrix
connect =c ;%element connectivity
%connect(i,j)=List of nodes on the jth element
elmn = 1 %( Loop over all the elements)
for a = 1:neln
  for i = 1:ndof
    for b = 1:neln
      for k = 1:ndof
	rw = ndof*(connect(elmn,a )-1)+i;
	cl = ndof*(connect(elmn,b )-1)+k;
	gStif(rw,cl) = gStif(rw,cl) + el_stif(ndof*(a-1)+i,ndof*(b-1)+k);
      end
    end
  end
end
el_stif =K2; %current element stiffness matrix
elmn = 2 %( Loop over all the elements)
for a = 1:neln
  for i = 1:ndof
    for b = 1:neln
      for k = 1:ndof
	rw = ndof*(connect(elmn,a )-1)+i;
	cl = ndof*(connect(elmn,b )-1)+k;
	gStif(rw,cl) = gStif(rw,cl) + el_stif(ndof*(a-1)+i,ndof*(b-1)+k);
      end
    end
  end
end

gStif=gStif*t;
gStifred=zeros(4,4);
for i=1:4
  for j=1:4
    gStifred(i,j)=gStif(i+2,j+2);
  end
end

gStifred

F=[P0*w*t/2; 0; P0*w*t/2; 0]

gStifred\F
