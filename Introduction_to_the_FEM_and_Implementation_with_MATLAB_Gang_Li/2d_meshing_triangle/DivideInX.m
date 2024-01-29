function [left_s,left_e, right_s,right_e]=DivideInX(sid,eid)
n_input_nodes=eid-sid+1;
d=round(n_input_nodes/2);   
left_s=sid; left_e=sid+d-1;
right_s=sid+d; right_e=eid;