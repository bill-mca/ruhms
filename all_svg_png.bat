:: This Windows batch script converts all SVGs in the working directory to pngs
:: Creek model outputs svgs which can be easily edited using inkscape (see
:: inkscape.org). to batch convert your svgs to pngs on windows 10 you can
:: install inkscape.
:: Then access 'view advanced system settings' from the start menu. Press the
:: environment variables button. Under 'user variables for <USER>' select path.
:: Press edit and append the directory containing inkscape.exe for me it was
:: C:\Program Files\Inkscape Note that in earlier versions of Windows 10 Path
:: is one long string with different directories seperated by a semi-colon (;)

for %%f in (.\*.svg) do (
    echo %%f
    inkscape --export-png="%%~nf.png" "%%f"
    )
