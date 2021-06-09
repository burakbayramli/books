% get gauss points in the parent element domain [-1, 1] and the corresponding weights

function [w,gp] = gauss(ngp)

    if ngp == 1
        gp = 0;
        w  = 2;
    elseif ngp == 2
        gp = [-0.57735027, 0.57735027];
        w  = [1,           1];
    elseif ngp == 3
        gp = [-0.7745966692,  0.7745966692,  0.0];
        w  = [0.5555555556,   0.5555555556,  0.8888888889];
    end
