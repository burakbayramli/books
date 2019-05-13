function activation = inertia_neuron(theta)

activation = [1;sin(theta');cos(theta');sin(sum(theta));cos(sum(theta))];


