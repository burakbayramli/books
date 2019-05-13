clear all;  
for numb = 1:3
        data = [];
        load torque_data;
        
        data = torque_data(numb).d;
        time = data(:,1);
        torque = data(:,[2 3]);
        thetad = data(:,[4 5]);
        theta_dot_d = data(:,[6 7]);
        theta_ddot_d = data(:,[8 9]);
        
        plot(time,thetad(:,1),time,thetad(:,2),'.');
        hold on;
       
        
end 