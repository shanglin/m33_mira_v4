#!/bin/tcsh -f
foreach x (`seq 1 22`)
# set f_tex = "$argv:gas/.tex//"
set f_tex = "file_"$x
# echo "converting ""$f_tex"".tex >>>>>>"
latex "$f_tex"".tex"
# bibtex "$f_tex"
# latex "$f_tex"".tex"
# latex "$f_tex"".tex"
dvips -Ppdf "$f_tex"".dvi"
ps2pdf "$f_tex"".ps"
mkdir -p TeXlogs
mv "$f_tex"".log" "$f_tex"".aux" "$f_tex"".dvi" "$f_tex"".ps" "$f_tex"".bbl" "$f_tex"".blg" "$f_tex"".out" TeXlogs
end
