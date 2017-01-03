def coumt_letter(text, ch):
    return text.count(ch)

if __name__ == '__main__':
    # read in file and strip trailing empty space 
    s = open('dna.txt').read().strip()

    for ch in 'ACTG':
        print ch, count_letter(s, ch)
