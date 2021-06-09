% This module generates a mesh of the linear strain triangular element
%
global nnd nel geom connec XIG YIG
global Length Width NXE NYE X_origin Y_origin dhx dhy
%
%
nnd = 0;
k = 0;
for i = 1:NXE
    for j=1:NYE
            k = k + 1;
            n1 = (2*j-1) + (2*i-2)*(2*NYE+1) ;
            n2 = (2*j-1) + (2*i-1)*(2*NYE+1);
            n3 = (2*j-1) + (2*i)*(2*NYE+1);
            n4 = n1 + 1;
            n5 = n2 + 1;
            n6 = n3 + 1 ;
            n7 = n1 + 2;
            n8 = n2 + 2;
            n9 = n3 + 2;
            %
            geom(n1,:) = [(i-1)*dhx - X_origin             (j-1)*dhy - Y_origin];
            geom(n2,:) = [((2*i-1)/2)*dhx - X_origin       (j-1)*dhy - Y_origin  ];
            geom(n3,:) = [i*dhx - X_origin                 (j-1)*dhy - Y_origin  ];
            geom(n4,:) = [(i-1)*dhx - X_origin           ((2*j-1)/2)*dhy - Y_origin ];
            geom(n5,:) = [((2*i-1)/2)*dhx - X_origin     ((2*j-1)/2)*dhy - Y_origin ];                         
            geom(n6,:) = [i*dhx - X_origin               ((2*j-1)/2)*dhy - Y_origin ];
            geom(n7,:) = [(i-1)*dhx - X_origin              j*dhy - Y_origin];
            geom(n8,:) = [((2*i-1)/2)*dhx - X_origin        j*dhy - Y_origin];
            geom(n9,:) = [i*dhx - X_origin                  j*dhy - Y_origin];
            %
            nel = 2*k;
            m = nel -1;
            connec(m,:) = [n1  n2  n3    n5    n7   n4];
            connec(nel,:) = [n3   n6    n9    n8    n7   n5];
            max_n = max([n1  n2  n3   n4  n5  n6  n7   n8    n9]);
            if(nnd <= max_n); nnd = max_n; end;
            %
            % XIN and YIN are two vectors that holds the coordinates X and Y
            % of the grid necessary for the function contourf (XIN,YIN, stress)
            %
            XIG(2*i-1) = geom(n1,1); XIG(2*i) = geom(n2,1); XIG(2*i+1) = geom(n3,1);
            YIG(2*j-1) = geom(n1,2); YIG(2*j) = geom(n4,2); YIG(2*j+1) = geom(n7,2);
    end
end
%




