nodes = 12*[-7.5, 0; 7.5, 0; -7.5, 15;
        7.5, 15; -5, 25; 5, 25; -5, 35; 5, 35; -15, 40; 15, 40;
        -25, 45; -15, 45; -5, 45; 5, 45; 15, 45; 25, 45];
PlaneTrussElement(29000000,2.5,nodes([3 5],:))