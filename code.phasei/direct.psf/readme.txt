1. direct.psf.combined.master.r
Master frame obtained by combining all the image. The combined image looks not good. ii6308.fits looks much better. Use PSF of the master frame.
Light curves failed.

2. direct.psf.sgl.master.r
Use ii6308.fits as master. Determined PSF for individule images. Used aperture photometry to match frames.
Light curves improved a lot but still some bad ones.

3. direct.allframe.phot.r
Use ii6308.fits as master. Determined PSF for individule images. Use 3-sigma ALLSTAR results to match frames. Set FI = 2 for DAOPHOT & ALLSTAR. This setting would take longer time than (1) and (2).
Success! Very good light curves.
