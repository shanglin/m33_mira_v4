1. mksigmag.r:  sigma-relation of each alf
2. calmagoff.r: magoff between .fnl and massey: 
             fit true.mag = a*instr.mag + b
3. match objects based on R.A. Dec. , ds9 masters to find overlap
regions 
   (1) bfmklc.r generate a catlog of uid, ra, dec, m, e
   (2) mk.field.map.r derive the map.

4. generate combine light curves with universal IDs. Get rid of bad observations & add zero points of each .fnl files.
