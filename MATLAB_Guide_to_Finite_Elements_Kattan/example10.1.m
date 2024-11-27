E=210e6
G=84e6
A=2e-2
Iy=10e-5
Iz=20e-5
J=5e-5
k1=SpaceFrameElementStiffness(E,G,A,Iy,Iz,J,0,0,0,3,0,0)
k2=SpaceFrameElementStiffness(E,G,A,Iy,Iz,J,0,0,0,0,0,-3)
k3=SpaceFrameElementStiffness(E,G,A,Iy,Iz,J,0,0,0,0,-4,0)
K=zeros(24,24);
K=SpaceFrameAssemble(K,k1,1,2);
K=SpaceFrameAssemble(K,k2,1,3);
K=SpaceFrameAssemble(K,k3,1,4)
k=K(1:6,1:6)
f=[-10 ; 0 ; 20 ; 0 ; 0 ; 0]
u=k\f
U=[u ; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]
F=K*U

u1=[U(1);U(2);U(3);U(4);U(5);U(6);
    U(7);U(8);U(9);U(10);U(11);U(12) ]

u2=[U(1);U(2);U(3);U(4);U(5);U(6);U(13);
    U(14);U(15);U(16);U(17);U(18)]

u3=[U(1);U(2);U(3);U(4);U(5);U(6);U(19);
    U(20);U(21);U(22);U(23);U(24)]

f1=SpaceFrameElementForces(E,G,A,Iy,Iz,J,0,0,0,3,0,0,u1)

f2=SpaceFrameElementForces(E,G,A,Iy,Iz,J,0,0,0,0,0,-3,u2)

f3=SpaceFrameElementForces(E,G,A,Iy,Iz,J,0,0,0,0,-4,0,u3)

