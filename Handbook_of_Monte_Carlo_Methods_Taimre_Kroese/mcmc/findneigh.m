function A = findneigh(i,j,m)
% find neighbors of the (i,j)-th site of an m by m grid
if i==1
    if j==1
        A = [1,1;1,2;2,1];
    elseif j==m
        A = [1,m;1,m-1;2,m];
    else
        A = [1,j;1,j-1;1,j+1;2,j];
    end
elseif i==m
    if j==1
        A = [m,1;m,2;m-1,1];
    elseif j==m
        A = [m,m;m,m-1;m-1,m];
    else
        A = [m,j;m,j-1;m,j+1;m-1,j];
    end
else
   if j==1
        A = [i,1;i,2;i-1,1;i+1,1];
    elseif j==m
        A = [i,m;i,m-1;i+1,m;i-1,m];
    else
        A = [i,j;i,j-1;i,j+1;i+1,j;i-1,j];
    end 
end
end
