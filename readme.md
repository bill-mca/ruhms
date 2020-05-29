Creek Model is an R script for basic hydraulic modelling of waterways developed
for the purpose of preparing applications for controlled activity approval in
NSW. The use was made clear to me working for Mulloon Consulting, Contracting
and Certifying.

Creek model was developed using R version 3.6.1 on GNU/linux and tested with
R version 3.6.2 on Windows 10.

Creek Model requires the sp library to be installed. You may need to run
install.packages('sp') from an R prompt before first using this script.

Creek Model is licenced under the GNU General Public Licence (Version 3)

To easily use Creek Model under windows you will need to edit the registry to
make R executable. Instructions can be found in the windows batch files named
run_model.bat and optionally, all_svg_png.bat

For linux users you need a recent version of R. For Debian based systems run
sudo apt install R

Then to setup the neccessary R packages run
sudo R

And from the root-privileged R prompt run
install.packages('sp')

coming soon