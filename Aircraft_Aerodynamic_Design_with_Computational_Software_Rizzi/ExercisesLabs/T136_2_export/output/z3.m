
load x3-Cx_alpha
h=plot(results.alpha_sweep*57,results.rudderangle*57,'o-')
hold on
set(h,'Linewidth',2)

load x8-Cx_alpha
h=plot(results.alpha_sweep*57,results.rudderangle*57,'+-')
hold on
set(h,'Linewidth',2)

load x1-Cx_alpha
h=plot(results.alpha_sweep*57,results.rudderangle*57,'^-')
hold on
set(h,'Linewidth',2)

load x4-Cx_alpha
h=plot(results.alpha_sweep*57,results.rudderangle*57,'s-')
hold on
set(h,'Linewidth',2)

load x5-Cx_alpha
h=plot(results.alpha_sweep*57,results.rudderangle*57,'p-')
hold on
set(h,'Linewidth',2)

load x7-Cx_alpha
h=plot(results.alpha_sweep*57,results.rudderangle*57,'h-')
hold on
set(h,'Linewidth',2)

ylabel('Canard angle to trim, \delta_c, [deg]')
xlabel('Angle of attack, \alpha, [deg]')
legend('X_{CG} = 2.32 ','X_{CG} = 2.42','X_{CG} = 2.52 @ NP','X_{CG} = 2.62','X_{CG} = 2.72','X_{CG} = 2.82')
