import pandas as pd
import numpy as np
import random

AUDIO_LENGTH = 16000

def get_noise(AUDIO_NOISES):

    noise = random.choice(AUDIO_NOISES)
    noise_length = len(noise)
    t = np.random.randint(0, noise_length - 16000 - 1)
    noise = noise[t:t + 16000]    
    return noise

def tf_random_add_noise_transform(wave, AUDIO_NOISES, noise_limit=0.2, u=0.5):

    if random.random() < u:
        noise = random.choice(AUDIO_NOISES)
        wave_length  = len(wave)
        noise_length = len(noise)
        t = np.random.randint(0, noise_length - wave_length - 1)
        noise = noise[t:t + wave_length]

        alpha = np.random.random() * noise_limit
        wave  = np.clip(alpha * noise + wave, -1, 1)

    return wave


def tf_random_time_shift_transform(wave, shift_limit=0.2, u=0.5):
    if random.random() < u:
        wave_length  = len(wave)
        shift_limit = shift_limit*wave_length
        shift = np.random.randint(-shift_limit, shift_limit)
        t0 = -min(0, shift)
        t1 =  max(0, shift)
        wave = np.pad(wave, (t0, t1), 'constant')
        wave = wave[:-t0] if t0 else wave[t1:]

    return wave


def tf_random_pad_transform(wave, length=AUDIO_LENGTH):

    if len(wave)<AUDIO_LENGTH:
        L = abs(len(wave)-AUDIO_LENGTH)
        start = np.random.choice(L)
        wave  = np.pad(wave, (start, L-start), 'constant')

    elif len(wave)>AUDIO_LENGTH:
        L = abs(len(wave)-AUDIO_LENGTH)
        start = np.random.choice(L)
        wave  = wave[start: start+AUDIO_LENGTH]

    return wave


def tf_fix_pad_transform(wave, length=AUDIO_LENGTH):
    # wave = np.pad(wave, (0, max(0, AUDIO_LENGTH - len(wave))), 'constant')
    # return wave

    if len(wave)<AUDIO_LENGTH:
        L = abs(len(wave)-AUDIO_LENGTH)
        start = L//2
        wave  = np.pad(wave, (start, L-start), 'constant')

    elif len(wave)>AUDIO_LENGTH:
        L = abs(len(wave)-AUDIO_LENGTH)
        start = L//2
        wave  = wave[start: start+AUDIO_LENGTH]

    return wave

