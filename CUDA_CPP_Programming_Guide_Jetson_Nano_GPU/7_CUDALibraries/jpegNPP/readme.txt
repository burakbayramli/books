Sample: jpegNPP
Minimum spec: SM 3.0

This sample demonstrates a simple image processing pipeline. First, a JPEG file is huffman decoded and inverse DCT transformed and dequantized. Then the different planes are resized. Finally, the resized image is quantized, forward DCT transformed and huffman encoded.

