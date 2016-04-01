I copied the fake .als files, m33massey.cat, etc. from Dr. Pellerin's reduction to derive the astrometry.

The code reduce.massey.r was not used in this reduction.

(1) update.wcs_direct.r
   (a) generate a table <field>_rd.dat with ID, X, Y, RA, Dec, Massey_ID.
   (b) use the above table to update the WCS for all the images with .alf photometry.
   (c) useful output: <root>w.fits, field.fits

(2) update.wcs_wiyn.r
  There are 3 fields failed: w9i, wfi, and wbi. We found that the
  master frame for wbi does not overlap much with other frames and
  repeat the ALLFRAME photometry with bad frames removed.
  There are two additional fake .als files in 
  	/dc/m33/d3/wiyn/astr/master/i/ on baires.
  Use update.wcs_wiyn.single.r  to finished the last three fields.

  Besides, some of the wiyn images (not master) failed to obtain an
  WCS solution. They were labeled as <root>w.fits_bad. They should be
  fixed by xy2sky of master frame and then update the WCS. We did not
  perform this for now since they are not useful.
