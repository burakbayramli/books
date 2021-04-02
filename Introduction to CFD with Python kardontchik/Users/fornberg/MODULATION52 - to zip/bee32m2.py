# ----------------------------------------------------------------
#   Beethoven Sonata No 32 Op 111, 2nd movement: Arietta - Adagio
# ----------------------------------------------------------------

from MODULATION import *
import config

config.yaxis = []
plt.close('all')
# ------------------------------------------------------------------------
#   WRITE BELOW ALL YOUR config.beats_per_bar, chords and keys statements
# ------------------------------------------------------------------------
config.beats_per_bar = 3

# --------------------------
#       Theme   bars 1-16
# --------------------------

keys(1,1,8,3,'C','M')
chords(1,2,[1,0,1,1,0],'C')
chords(2,3,[1,0,1,1,0],'C')
chords(4,3,[1,1,1,1,0],'C')
chords(6,1,[1,0,1,0,1],'C') 
chords(6,2,[0,0,1,1,1],'C')

chords(6,3,[1,1,1,1,0],'G')
# out-of-key driving valid chord in C Maj: G - B - D

chords(8,2,[1,1,1,1,0],'C')

keys(9,1,12,3,'A','m')
chords(10,1,[1,1,1,0,0],'A')
chords(10,2,[1,1,1,0,0],'A')
chords(10,3,[1,1,1,0,0],'A')
chords(11,1,[1,1,1,0,0],'A')

chords(12,2,[1,1,1,0,0],'A')

chords(13,1,[1,1,1,0,0],'C')
chords(13,2,[1,1,1,0,0],'C')
chords(13,3,[1,1,1,0,0],'C')
keys(14,1,16,3,'C','M')

chords(15,1,[1,1,1,0,0],'C')
chords(15,2,[1,1,1,0,0],'C')
chords(15,3,[1,1,1,0,0],'C')
chords(16,1,[1,1,1,0,0],'C')

# -----------------------------------------------------------------------
#   Update the values of from_bar and to_bar to fit your problem
# -----------------------------------------------------------------------
from_bar = 1
to_bar = 16

# -----------------------------------------------------------------------

texts(from_bar,to_bar)
plt.title('Beethoven Sonata No 32 Op 111, 2nd mov: Arietta(1822)')
plt.show()






