%
%                     Script file  gdome  (geodesic dome)
%  The program use the MATLAB function  bucky  which provides the
%  coordinates on the unit sphere of a polyhedron consisting of 12 
%  pentagons and  20 hexagons.  This polyhedron is displayed. 
%  The user hits return, and each of the hexagons is divided into
%  6 triangles (isoceles).  After hitting return second time, the
%  the pentagons are also subdivided into triangles. This yields
%  a surface consisting of 180 isoceles triangles of two types.

    [B,V] = bucky;
    P = V';
   
    for i = 1:12
       x = P(1,(i-1)*5+1:i*5);
       y = P(2,(i-1)*5+1:i*5);
       z = P(3,(i-1)*5+1:i*5);
       fill3(x,y,z,'b')
       hold on
    end

   % for i = 1:60
   %   vertexnumber = num2str(i);
   %   text(V(i,1), V(i,2), V(i,3),vertexnumber)
   % end

    H1 = zeros(3,6);
    H1(:,1) = P(:,1);
    H1(:,2) = P(:,6);
    H1(:,3) = P(:,10);
    H1(:,4) = P(:,12);
    H1(:,5) = P(:,11);
    H1(:,6) = P(:,2);

    
   R = [ cos(.4*pi), -sin(.4*pi), 0; sin(.4*pi), cos(.4*pi),0; 0 0 1];
   H = zeros(3,120);
   for j =1:5
      H(:,(j-1)*6+1: j*6) = R^(j-1)*H1;
      xedge = H(1, (j-1)*6 +1: j*6);
      yedge = H(2, (j-1)*6 +1: j*6);
      zedge = H(3, (j-1)*6+ 1: j*6);
      fill3(xedge, yedge, zedge, 'w')
    end

    H6 = zeros(3,6);
    H6(:,1) = P(:,10);
    H6(:,2) = P(:,9);
    H6(:,3) = P(:,38);
    H6(:,4) = P(:,37);
    H6(:,5) = P(:,13);
    H6(:,6) = P(:,12);
    c1 = [.9 .9 .9];
    for j = 6:10
       H(:, (j-1)*6+1:6*j) = R^(j-6)*H6;
       xedge = H(1, (j-1)*6+1: j*6);
       yedge = H(2, (j-1)*6+1: j*6);
       zedge = H(3, (j-1)*6+1: j*6);
       fill3(xedge, yedge, zedge, c1)
    end

    H11 = zeros(3,6);
    H11(:,1) = P(:,13);
    H11(:,2) = P(:,37);
    H11(:,3) = P(:,36);
    H11(:,4) = P(:,34);
    H11(:,5) = P(:,33);
    H11(:,6) = P(:,14);
    c2 = [.8 .8 .8];
    for j = 11:15
       H(:, (j-1)*6 +1: j*6) = R^(j-11)*H11;
       xedge = H(1, (j-1)*6+1:j*6);
       yedge = H(2, (j-1)*6+1:j*6);
       zedge = H(3, (j-1)*6+1:j*6); 
       fill3(xedge, yedge, zedge, c2)
    end


    Q = [cos(.2*pi) -sin(.2*pi) 0; sin(.2*pi) cos(.2*pi) 0; 0 0 -1];

    H16 = Q*H1;
    c3 =  [.7 .7 .7];
    for j = 16:20
       H(:,(j-1)*6+1: j*6) = R^(j-16)*H16;
       xedge = H(1, (j-1)*6+1:j*6);
       yedge = H(2, (j-1)*6+1:j*6);
       zedge = H(3, (j-1)*6+1:j*6);
       fill3(xedge, yedge, zedge, c3)
    end



    for i = 1:30
        for j = 1:i
            if B(i,j) > 0
                L = [V(i,:); V(j,:)];
                x = L(:,1);
                y = L(:,2);
                z = L(:,3);
                plot3(x,y,z)
            end
         end
    end     
    view (128,15)
    axis equal
    disp('hit return to continue ')
    pause

    for j = 1:20

        if j <= 5
           color = [1 1 1];
        elseif 6 <= j  & j <= 10
           color = c1;
        elseif 11 <= j  & j <= 15
           color = c2;
        else
             color = c3;
        end

    p1 = H(:, (j-1)*6+1);
    p2 = H(:, (j-1)*6+2);
    p3 = H(:, (j-1)*6+3);
    p4 = H(:, (j-1)*6+4);
    p5 = H(:, (j-1)*6+5); 
    p6 = H(:, (j-1)*6+6);
    A = [p1'; p2'; p3'];
    d = .5*norm(p1+p4);

    p = d*(A\[1 1 1]');

    U = [p1,p,p2,p,p3,p,p4, p,p5, p,p6,p,p1];


    x1 = U(1,1:3);
    y1 = U(2,1:3);
    z1 = U(3,1:3);
    fill3(x1,y1,z1,color)

    x2 = U(1,3:5);
    y2 = U(2,3:5);
    z2 = U(3,3:5);
    fill3(x2,y2,z2,color)

    x3 = U(1,5:7);
    y3 = U(2,5:7);
    z3 = U(3,5:7); 
    fill3(x3,y3,z3, color)

    x4 = U(1,7:9);
    y4 = U(2,7:9);
    z4 = U(3,7:9);
    fill3(x4,y4,z4,color)

    x5 = U(1, 9:11);
    y5 = U(2, 9:11);
    z5 = U(3, 9:11);
    fill3(x5,y5,z5, color)

    x6 = U(1,11:13);
    y6 = U(2,11:13);
    z6 = U(3,11:13); 
    fill3(x6, y6, z6, color)
    
    end
    disp('hit return to continue ')
    pause

    c4 = [0 .8  1];

    for j = 1:12

       p1 = P(:,(j-1)*5+1);
       p2 = P(:,(j-1)*5+2);
       p3 = P(:,(j-1)*5+3);
       p4 = P(:,(j-1)*5+4);
       p5 = P(:,(j-1)*5+5);
     
       d = .2*norm(p1+p2+p3+p4+p5);

       A = [p1'; p2';p3'];
       p = d*(A\[1 1 1]');

       UU = [p1,p,p2,p,p3,p,p4,p,p5,p,p1];
       x1 = UU(1,1:3);
       y1 = UU(2,1:3);
       z1 = UU(3,1:3);
       fill3(x1,y1,z1,c4)

       x2 = UU(1,3:5);
       y2 = UU(2,3:5);
       z2 = UU(3,3:5);
       fill3(x2,y2,z2,c4)

       x3 = UU(1,5:7);
       y3 = UU(2,5:7);
       z3 = UU(3,5:7);
       fill3(x3,y3,z3,c4);

       x4 = UU(1,7:9);
       y4 = UU(2,7:9);
       z4 = UU(3,7:9);
       fill3(x4,y4,z4, c4);
  
       x5 = UU(1,9:11);
       y5 = UU(2,9:11);
       z5 = UU(3,9:11);
       fill3(x5,y5,z5,c4)
       
   end
   

   hold off
