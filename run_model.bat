:: This is a windows batch script to run creek model. You must have R installed
:: and the 'sp' package for R.
:: For this script to run you must also have R added to the windows path
:: variable. To do so in Windows 10:
:: Access 'view advanced system settings' from the start menu.
:: Press the environment variables button.
:: Under 'user variables for <USER>', select path.
:: Press edit and append the directory containing Rscript.exe.
:: for me it was C:\Program Files\R\R-3.6.2\bin
:: Note that in earlier versions of Windows 10 Path
:: is one long string with different directories seperated by a semi-colon (;)

::Rterm --file=custom_creek.R --no-restore

Rterm -e "source('custom_creek.R')" --no-restore

cmd /k