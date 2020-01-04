
DIR_IN = 'Data/';
filename = 'tfq.roy.8k';

display = 1;
[pitch, voicing] = fread_pitch_creaky([DIR_IN, filename], display);
