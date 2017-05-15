%test of cells

A=ones(4,4);
B=ones(3,3);
V=ones(1,7);

C=cell(3,1);

C(1)={A}; C(2)={B}; C(3)={V};

C{1}
