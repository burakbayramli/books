
DIR_IN = 'Data/';
filename = 'jazz_hour.8k';

display = 1;
[pitch, voicing] = fread_pitch_diplo([DIR_IN, filename], display);
