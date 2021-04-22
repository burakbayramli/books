
function [ex,S,M,w]=exact
L=12; a1=4; p1=10; a2=8; p2=-5; a3=8; m=-20; p4=20; p3=1;
E=1e4; I=1;
i=[0:.01:12];
ex=i;
ind=1;
for x=i    
    if x<a1
        w1=-p1*x^2/(6*E*I)*(3*a1-x);
    else
        w1=-p1*a1^2/(6*E*I)*(3*x-a1);
    end
    if x<a1
        M1=p1*a1*(1-x/a1);
    else
            M1=0;
    end
    if x<a2
        w2=-p2*x^2/(6*E*I)*(3*a2-x);
    else
        w2=-p2*a2^2/(6*E*I)*(3*x-a2);
    end
    if x<a2
        M2=p2*a2*(1-x/a2);
    else
            M2=0;
    end
    w3=-m*x^2/(2*E*I);
    M3=m;    
    w4=-p4*x^2*(3*L-x)/(6*E*I);
    M4=p4*(L-x);
    if x<a3
        w5=-p3*x^2*(6*a3^2-4*x*a3+x^2)/(24*E*I);
    else
        w5=   -p3*a3*(a3/2)^2/(6*E*I)*(3*x-a3/2);
        
    end
    if x<a3
        M5=1/2*p3*(a3-x)^2;
    else
        M5=0;
    end
    if x<=12
        S1=20;
    else
        S1=0;
    end
    if x<=8
        S2=-5;
    else
        S2=0;            
    end
    if x<8
        S3=(8-x)*1;
    else
        S3=0;
    end
    if x<4
        S4=10;
    else
        S4=0;
    end
    w(ind)=w1+w2+w3+w4+w5;
    M(ind)=M1+M2+M3+M4+M5;
    S(ind)=S1+S2+S3+S4;
    ind=ind+1;                        
end
M=-M;
