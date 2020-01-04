
DIR_IN = 'Data/';
filename = 'silly_items.8k';

display = 1;
[pitch, voicing] = fread_pitch_creaky([DIR_IN, filename], display);
