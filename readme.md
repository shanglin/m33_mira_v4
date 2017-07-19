## This page is for personal record

<!---

# The M33 SURVEY: II. Mira Variables

### This repository contains the codes for the Mira search project using *I* - band observations.

_________________________
_________________________

- ```GPcodes``` contains two packages initially designed by ```xxx(20xx)```. 
   - ```cpp``` is the C++ version of a Gaussian Process model periodogram.
   - ```r_v2``` is the R package of the same periodogram.
   - Both packages need the support of ```Armadillo```. R version can be used as interactive plotting or trouble shooting purposes. While C++ version can be used for massive calculation on cluster.

- ```MasseyVI``` Some simple script to read the M33 catalog from [the Local Group Survey](http://www2.lowell.edu/users/massey/lgsurvey.html).
- ```code.phasei``` is for the data reduction purpose. It includes the step-to-step photometry procedures using DAOPHOT/ALLSTAR/ALLFRAME toolkit.
- ```rf_model``` performs Random Forest classification of Miras and non-Miras.
- ```sim_ofiles``` generates simulated light curves of known classes.
- ```sc_get_spec``` applies Gaussian Process periodogram on light curves using cluster to obtain frequency spectra.
- ```sc_ext_ftrs``` extracts some features from the spectra and light curves to aid the classification.
- ```mira_cata_sampsize``` studies the classification results. 
   - ```vis_inspect_source``` is a piece of PHP-based tool designed to perform interactive inspection on figures using web browser. It can be used for other similar visual inspections without significant modification.

-->
   
