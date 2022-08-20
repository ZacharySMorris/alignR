# alignR

## Overview

alignR is a package for digitizing 3D landmarks for use in geometric morphometric analyses of either discrete anatomical points (fixed_lm), 
high density surfaces with pseudolandmarks (pseudo_lm), or a mixture of both (mixed_lm). The interface for landmarking is based on rglwidget (rgl package 
link) and Shiny (shiny package link).

### Discrete Landmarks
The discrete or fixed landmark version of alignR allows for the rapid collection of 3D datapoints which can be immediately input into functions for 
performing geometric morphometric analysis found in both geomorph (package) and morpho (package). alignR improves on previous landmarking functions in R 
by allowing for loading of existing landmark data and the ability to swap back and forth between specimens for visual re-assessment of landmark placement.

### Pseudolandmark Alignment
The pure pseudolandmark version alignR simplifies the alignment process for geometric morphometic analyses of complex shapes without fixed, discrete 
landmarks. By simply digitizing points along the major anatomical axes, alignR is able to quickly perform a rough orientation followed by a finer scale fit 
of all specimens BASED ON WHAT MATH?. This greatly improves the speed of aligning specimens compared to purely automatic methods as currently implemented.

### Mixed Landmarks
However, the mixed landmark version of alignR may be the most powerful. After digitizing a set of discrete landmarks (with key anatomical data), this 
analysis fills the surface around the fixed landmarks with pseudolandmarks. The discrete landmarks increase the speed of fitting specimens (as with 
pseudo_lm), but then are retained in the resulting dataset. This allows for comparisons of the full 3D shape and key landmarks, while also resolving the 
issue of density of landmarks which make joint analysis theoretically or pragmatically problematic in other implementations.
