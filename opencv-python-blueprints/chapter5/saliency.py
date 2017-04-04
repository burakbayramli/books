#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""A module to generate a saliency map from an RGB image

    This code is based on the approach described in:
    [1] X. Hou and L. Zhang (2007). Saliency Detection: A Spectral Residual
        Approach. IEEE Transactions on Computer Vision and Pattern Recognition
        (CVPR), p.1-8. doi: 10.1109/CVPR.2007.383267
"""

import cv2
import numpy as np
from matplotlib import pyplot as plt


class Saliency:
    """Generate saliency map from RGB images with the spectral residual method

        This class implements an algorithm that is based on the spectral
        residual approach (Hou & Zhang, 2007).
    """
    def __init__(self, img, use_numpy_fft=True, gauss_kernel=(5, 5)):
        """Constructor

            This method initializes the saliency algorithm.

            :param img: an RGB input image
            :param use_numpy_fft: flag whether to use NumPy's FFT (True) or
                                  OpenCV's FFT (False)
            :param gauss_kernel: Kernel size for Gaussian blur
        """
        self.use_numpy_fft = use_numpy_fft
        self.gauss_kernel = gauss_kernel
        self.frame_orig = img

        # downsample image for processing
        self.small_shape = (64, 64)
        self.frame_small = cv2.resize(img, self.small_shape[1::-1])

        # whether we need to do the math (True) or it has already
        # been done (False)
        self.need_saliency_map = True

    def get_saliency_map(self):
        """Returns a saliency map

            This method generates a saliency map for the image that was
            passed to the class constructor.

            :returns: grayscale saliency map
        """
        if self.need_saliency_map:
            # haven't calculated saliency map for this image yet
            num_channels = 1
            if len(self.frame_orig.shape) == 2:
                # single channel
                sal = self._get_channel_sal_magn(self.frame_small)
            else:
                # multiple channels: consider each channel independently
                sal = np.zeros_like(self.frame_small).astype(np.float32)
                for c in xrange(self.frame_small.shape[2]):
                    small = self.frame_small[:, :, c]
                    sal[:, :, c] = self._get_channel_sal_magn(small)

                # overall saliency: channel mean
                sal = np.mean(sal, 2)

            # postprocess: blur, square, and normalize
            if self.gauss_kernel is not None:
                sal = cv2.GaussianBlur(sal, self.gauss_kernel, sigmaX=8,
                                       sigmaY=0)
            sal = sal**2
            sal = np.float32(sal)/np.max(sal)

            # scale up
            sal = cv2.resize(sal, self.frame_orig.shape[1::-1])

            # store a copy so we do the work only once per frame
            self.saliencyMap = sal
            self.need_saliency_map = False

        return self.saliencyMap

    def _get_channel_sal_magn(self, channel):
        """Returns the log-magnitude of the Fourier spectrum

            This method calculates the log-magnitude of the Fourier spectrum
            of a single-channel image. This image could be a regular grayscale
            image, or a single color channel of an RGB image.

            :param channel: single-channel input image
            :returns: log-magnitude of Fourier spectrum
        """
        # do FFT and get log-spectrum
        if self.use_numpy_fft:
            img_dft = np.fft.fft2(channel)
            magnitude, angle = cv2.cartToPolar(np.real(img_dft),
                                               np.imag(img_dft))
        else:
            img_dft = cv2.dft(np.float32(channel),
                              flags=cv2.DFT_COMPLEX_OUTPUT)
            magnitude, angle = cv2.cartToPolar(img_dft[:, :, 0],
                                               img_dft[:, :, 1])

        # get log amplitude
        log_ampl = np.log10(magnitude.clip(min=1e-9))

        # blur log amplitude with avg filter
        log_ampl_blur = cv2.blur(log_ampl, (3, 3))

        # residual
        residual = np.exp(log_ampl - log_ampl_blur)

        # back to cartesian frequency domain
        if self.use_numpy_fft:
            real_part, imag_part = cv2.polarToCart(residual, angle)
            img_combined = np.fft.ifft2(real_part + 1j*imag_part)
            magnitude, _ = cv2.cartToPolar(np.real(img_combined),
                                           np.imag(img_combined))
        else:
            img_dft[:, :, 0], img_dft[:, :, 1] = cv2.polarToCart(residual,
                                                                 angle)
            img_combined = cv2.idft(img_dft)
            magnitude, _ = cv2.cartToPolar(img_combined[:, :, 0],
                                           img_combined[:, :, 1])

        return magnitude

    def calc_magnitude_spectrum(self):
        """Plots the magnitude spectrum

            This method calculates the magnitude spectrum of the image passed
            to the class constructor.

            :returns: magnitude spectrum
        """
        # convert the frame to grayscale if necessary
        if len(self.frame_orig.shape) > 2:
            frame = cv2.cvtColor(self.frame_orig, cv2.COLOR_BGR2GRAY)
        else:
            frame = self.frame_orig

        # expand the image to an optimal size for FFT
        rows, cols = self.frame_orig.shape[:2]
        nrows = cv2.getOptimalDFTSize(rows)
        ncols = cv2.getOptimalDFTSize(cols)
        frame = cv2.copyMakeBorder(frame, 0, ncols-cols, 0, nrows-rows,
                                   cv2.BORDER_CONSTANT, value=0)

        # do FFT and get log-spectrum
        img_dft = np.fft.fft2(frame)
        spectrum = np.log10(np.abs(np.fft.fftshift(img_dft)))

        # return for plotting
        return 255*spectrum/np.max(spectrum)

    def plot_power_spectrum(self):
        """Plots the power spectrum

            This method plots the power spectrum of the image passed to
            the class constructor.

            :returns: power spectrum
        """
        # convert the frame to grayscale if necessary
        if len(self.frame_orig.shape) > 2:
            frame = cv2.cvtColor(self.frame_orig, cv2.COLOR_BGR2GRAY)
        else:
            frame = self.frame_orig

        # expand the image to an optimal size for FFT
        rows, cols = self.frame_orig.shape[:2]
        nrows = cv2.getOptimalDFTSize(rows)
        ncols = cv2.getOptimalDFTSize(cols)
        frame = cv2.copyMakeBorder(frame, 0, ncols - cols, 0, nrows - rows,
                                   cv2.BORDER_CONSTANT, value=0)

        # do FFT and get log-spectrum
        if self.use_numpy_fft:
            img_dft = np.fft.fft2(frame)
            spectrum = np.log10(np.real(np.abs(img_dft))**2)
        else:
            img_dft = cv2.dft(np.float32(frame), flags=cv2.DFT_COMPLEX_OUTPUT)
            spectrum = np.log10(img_dft[:, :, 0]**2+img_dft[:, :, 1]**2)

        # radial average
        L = max(frame.shape)
        freqs = np.fft.fftfreq(L)[:L/2]
        dists = np.sqrt(np.fft.fftfreq(frame.shape[0])[:, np.newaxis]**2 +
                        np.fft.fftfreq(frame.shape[1])**2)
        dcount = np.histogram(dists.ravel(), bins=freqs)[0]
        histo, bins = np.histogram(dists.ravel(), bins=freqs,
                                   weights=spectrum.ravel())

        centers = (bins[:-1] + bins[1:]) / 2
        plt.plot(centers, histo/dcount)
        plt.xlabel('frequency')
        plt.ylabel('log-spectrum')
        plt.show()

    def get_proto_objects_map(self, use_otsu=True):
        """Returns the proto-objects map of an RGB image

            This method generates a proto-objects map of an RGB image.
            Proto-objects are saliency hot spots, generated by thresholding
            the saliency map.

            :param use_otsu: flag whether to use Otsu thresholding (True) or
                             a hardcoded threshold value (False)
            :returns: proto-objects map
        """
        saliency = self.get_saliency_map()

        if use_otsu:
            _, img_objects = cv2.threshold(np.uint8(saliency*255), 0, 255,
                                           cv2.THRESH_BINARY + cv2.THRESH_OTSU)
        else:
            thresh = np.mean(saliency)*255*3
            _, img_objects = cv2.threshold(np.uint8(saliency*255), thresh, 255,
                                           cv2.THRESH_BINARY)
        return img_objects
