% global_min_cost_func.m
% This cost function is for testing the global minimum.
% K. Beers. MIT ChE. 8/2/03

function F = global_min_cost_func(x,ModelParam);

delta_1 = x-ModelParam.xmin1; distsq_1 = dot(delta_1,delta_1);
delta_2 = x-ModelParam.xmin2; distsq_2 = dot(delta_2,delta_2);

if( distsq_1 <= distsq_2)
    F = ModelParam.c1 + ModelParam.c2*distsq_1;
else
    F = distsq_2;
end

return;
