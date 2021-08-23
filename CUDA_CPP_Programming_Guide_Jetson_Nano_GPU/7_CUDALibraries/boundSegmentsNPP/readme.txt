Sample: boundSegmentsNPP
Minimum spec: SM 3.0

An NPP CUDA Sample that demonstrates using nppiLabelMarkers to generate connected region segment labels in an 8-bit grayscale image then compressing the sparse list of generated labels into the minimum number of uniquely labeled regions in the image using nppiCompressMarkerLabels.  Finally a boundary is added surrounding each segmented region in the image using nppiBoundSegments.

Key concepts:
Performance Strategies
Image Processing
NPP Library
