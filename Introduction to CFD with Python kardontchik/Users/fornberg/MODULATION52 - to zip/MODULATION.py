"""
    MODULATION

    Author: Jaime E. Kardontchik, PhD
            Sunnyvale, California
    Date:  September 25, 2016 

    Feb 17, 2018
    For printed books: only black&white plots with 'N' added on top of Neapolitan
    
"""
import matplotlib.pyplot as plt
import config

plt.close ('all')

key_list =  ['Fb','Cb','Gb','Db','Ab','Eb','Bb','F','C','G','D','A','E','B','F#','C#','G#','D#','A#']
key_color = ['k','k','k', 'k', 'k', 'k', 'k', 'k','k','k','k','k','k','k','k', 'k', 'k', 'k', 'k']

def chords(bar,beat,chord,chord_key):
    
    # linestyle specified in list ch.
    # initialize the ch list
    ch = ['-']*5
    # indexes of missing notes in chord
    a = [i for i,x in enumerate(chord) if x == 0]
    for i in a:
        ch[i] = ':'

    # find index of the chord_key
    if chord_key in key_list:
        j = key_list.index(chord_key)
        config.yaxis.append(j)
    else:
        print 'TYPO ERROR in chord_key'
        return

    # rectangular region where the chord will be plotted    
    x1 = bar + (beat - 1.0)/float(config.beats_per_bar)
    x2 = x1 + 1/float(config.beats_per_bar)
    y1 = float(j) + 0.1
    y2 = float(j) + 0.9
    dy = 0.16
    # plot color
    scolor = key_color[j]
    if (-1 in chord):
        scolor = 'g'

    rectx = [x1,x2,x2,x1,x1]
    recty1 = [y1,y1,y1+dy,y1+dy,y1]
    recty2 = [y1+dy,y1+dy,y1+2*dy,y1+2*dy,y1+dy]
    recty3 = [y1+2*dy,y1+2*dy,y1+3*dy,y1+3*dy,y1+2*dy]
    recty4 = [y1+3*dy,y1+3*dy,y1+4*dy,y1+4*dy,y1+3*dy]
    recty5 = [y1+4*dy,y1+4*dy,y2,y2,y1+4*dy]
    
    plt.hold('on')
    plt.plot(rectx,recty1,scolor,linestyle=ch[0])
    plt.plot(rectx,recty2,scolor,linestyle=ch[1])             
    plt.plot(rectx,recty3,scolor,linestyle=ch[2])
    plt.plot(rectx,recty4,scolor,linestyle=ch[3])
    plt.plot(rectx,recty5,scolor,linestyle=ch[4])
        
    return None

def neap(bar,beat,chord,chord_key):
    
    # linestyle specified in list ch.
    # initialize the ch list
    ch = ['-']*5
    # indexes of missing notes in chord
    a = [i for i,x in enumerate(chord) if x == 0]
    for i in a:
        ch[i] = ':'

    # find index of the chord_key
    if chord_key in key_list:
        j = key_list.index(chord_key)
        config.yaxis.append(j)
    else:
        print 'TYPO ERROR in chord_key'
        return

    # rectangular region where the chord will be plotted    
    x1 = bar + (beat - 1.0)/float(config.beats_per_bar)
    x2 = x1 + 1/float(config.beats_per_bar)
    y1 = float(j) + 0.1
    y2 = float(j) + 0.9
    dy = 0.16
    # plot color
    scolor = 'k'

    rectx = [x1,x2,x2,x1,x1]
    recty1 = [y1,y1,y1+dy,y1+dy,y1]
    recty2 = [y1+dy,y1+dy,y1+2*dy,y1+2*dy,y1+dy]
    recty3 = [y1+2*dy,y1+2*dy,y1+3*dy,y1+3*dy,y1+2*dy]
    recty4 = [y1+3*dy,y1+3*dy,y1+4*dy,y1+4*dy,y1+3*dy]
    recty5 = [y1+4*dy,y1+4*dy,y2,y2,y1+4*dy]
    
    plt.hold('on')
    plt.plot(rectx,recty1,scolor,linestyle=ch[0])
    plt.plot(rectx,recty2,scolor,linestyle=ch[1])             
    plt.plot(rectx,recty3,scolor,linestyle=ch[2])
    plt.plot(rectx,recty4,scolor,linestyle=ch[3])
    plt.plot(rectx,recty5,scolor,linestyle=ch[4])

    # 'N' for Neapolitan
    x3 = x1 + 0.15/float(config.beats_per_bar)
    x4 = x2 - 0.15/float(config.beats_per_bar)
    y3 = float(j) + 1.1
    nx = [x3,x3,x4,x4]
    ny = [y3, y3+dy, y3, y3+dy]

    plt.plot(nx,ny,scolor,linestyle = '-')
        
    return None

def keys(bar1,beat1,bar2,beat2,key,mode):
    
    # find index from the key_list
    if key in key_list:
        j = key_list.index(key)
    else:
        print 'TYPO ERROR in key'
        return
    
    scolor = key_color[j]
    if mode == 'M':
        ch = '-'
    elif mode == 'm':
        ch = '--'
    else:
        print 'TYPO ERROR in mode'
        return
    
    # rectangular region where the key will be plotted    
    x1 = bar1 + (beat1 - 1.0)/float(config.beats_per_bar)
    x2 = bar2 + beat2/float(config.beats_per_bar)
    y1 = float(j)
    y2 = y1 + 1

    rectx = [x1,x2,x2,x1,x1]
    recty = [y1,y1,y2,y2,y1]

    plt.hold('on')
    plt.plot(rectx,recty,scolor,linewidth=4.0,linestyle=ch)

    config.yaxis.append(j)

    return

def texts(from_bar,to_bar):
    
    plt.xlabel('BAR NUMBER')
    plt.ylabel("KEYS and CHORDS (7th's and 9th's)")
    
    frame = plt.gca()
    
    xmin = from_bar - 1
    xmax = to_bar + 1
    frame.axes.set_xlim([xmin,xmax])
    
    ymin = min(config.yaxis)
    ymax = max(config.yaxis)
    
    ypos = [float(i) + 0.5 for i in range(ymin,ymax+1)]
    ylabels = [key_list[i] for i in range(ymin,ymax+1)]
    frame.set_yticks(ypos)
    frame.set_yticklabels(ylabels)

    return

