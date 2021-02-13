Sample: NV12toBGRandResize
Minimum spec: SM 3.0

This code shows two ways to convert and resize NV12 frames to BGR 3 planars frames using CUDA in batch. Way-1, Convert NV12 Input to BGR @ Input Resolution-1, then Resize to Resolution#2. Way-2, resize NV12 Input to Resolution#2 then convert it to BGR Output. NVIDIA HW Decoder, both dGPU and Tegra, normally outputs NV12 pitch format frames. For the inference using TensorRT, the input frame needs to be BGR planar format with possibly different size. So, conversion and resizing from NV12 to BGR planar is usually required for the inference following decoding. This CUDA code provides a reference implementation for conversion and resizing.

Key concepts:
Graphics Interop
Image Processing
Video Processing
