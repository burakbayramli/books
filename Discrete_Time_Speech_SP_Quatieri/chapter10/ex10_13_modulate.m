
DIR_IN = 'Data/';
filename = 'NE1.mm.8k';

display = 1;
[pitch, voicing] = fread_pitch_modulate([DIR_IN, filename], display);
