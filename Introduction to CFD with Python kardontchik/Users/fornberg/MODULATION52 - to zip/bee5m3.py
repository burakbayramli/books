# --------------------------------------------------------------------
#   Beethoven sonata No 5 op 10/1 (1797)
#   3rd movement - prestissimo
# --------------------------------------------------------------------

from MODULATION import *
import config

config.yaxis = []
plt.close('all')
# ------------------------------------------------------------------------
#   WRITE BELOW ALL YOUR config.beats_per_bar, chords and keys statements
# ------------------------------------------------------------------------
config.beats_per_bar = 2

keys(0,1,16,1,'C','m')
chords(0,2,[0,1,0,1,0],'C')
chords(1,2,[0,1,0,0,1],'C')     # C min
chords(2,2,[0,1,0,0,0],'C')

neap(3,1,[0,1,1,1,1],'G')     # G min
neap(3,2,[0,1,1,1,0],'G')

chords(4,2,[0,1,0,1,0],'G')
chords(5,2,[0,1,0,0,1],'G')

chords(6,2,[0,1,1,1,1],'C')     # C min
chords(7,1,[0,1,1,1,1],'C')

chords(8,2,[1,1,1,1,0],'F')

chords(9,2,[0,1,1,1,1],'C')     # C min

chords(10,2,[1,1,1,1,0],'F')

chords(11,2,[0,1,1,1,1],'C')

chords(12,1,[0,1,0,0,0],'G')
chords(12,2,[0,1,0,0,0],'C')

chords(13,1,[0,1,0,0,0],'G')
chords(13,2,[0,1,0,0,0],'C')

chords(14,1,[0,1,0,0,0],'G')


# -----------------------------------------------------------------------
#   Update the values of from_bar and to_bar to fit your problem
# -----------------------------------------------------------------------
from_bar = 0
to_bar = 16

# -----------------------------------------------------------------------

texts(from_bar,to_bar)
plt.title('Beethoven Sonata No 5 Op 10/1, 3rd mov - Prestissimo (1797)')
plt.show()






