#!/bin/sh

# Base file (and bash wizardry) by Guillaume,
# Modestly modified by Val to work on mac os
# reviewed in 2020 - no change

#input=../Resistances/resMap_X2030_iter1.tif
#outputprefix=../Circuitscape/Inputs/thisisatest
#buffer=120
#en=y

while getopts i:o:b:e: option
	do
		case "${option}"
			in
			i) input=${OPTARG};;
			o) outputprefix=${OPTARG};;
			b) buffer=${OPTARG};;
			e) en=${OPTARG};;
		esac
done

echo $en
echo $buffer

bounds=$(gdalinfo $input | awk '/(Upper Left)|(Lower Right)/' | awk '{gsub(/,|\)|\(/," ");print $3 " " $4}' | sed ':a;N;$!ba;s/\n/ /g')

minx=$(echo $bounds | awk '{print $1;}')
maxy=$(echo $bounds | awk '{print $2;}')
maxx=$(echo $bounds | awk '{print $3;}')
miny=$(echo $bounds | awk '{print $4;}')

echo $bounds
echo $input

newminx=$(echo $minx - $buffer| bc)
newminy=$(echo $miny - $buffer| bc)
newmaxx=$(echo $maxx + $buffer| bc)
newmaxy=$(echo $maxy + $buffer| bc)


gdalwarp -te $newminx $newminy $newmaxx $newmaxy -dstnodata 2 -co COMPRESS=LZW $input _tmp.tif
gdal_translate _tmp.tif ${outputprefix}.tif -of GTiff -a_nodata none
rm -rf _tmp.tif # added -rf feb 2020 as per guillaume's advice


# EW image

if [ "$en" = "y" ]
then
	gdal_merge.py -createonly -init 0 -o _tmp.tif $input
	gdalwarp -te $minx $newminy $maxx $newmaxy -dstnodata 0 -co COMPRESS=LZW _tmp.tif _tmp2.tif
	gdal_translate _tmp2.tif _tmp3.tif -co COMPRESS=LZW -a_nodata none
	rm _tmp.tif _tmp2.tif
	gdalwarp -te $newminx $newminy $maxx $newmaxy -dstnodata 2 -co COMPRESS=LZW _tmp3.tif _tmp.tif
	gdal_translate _tmp.tif _tmp2.tif -co COMPRESS=LZW -a_nodata none
	rm _tmp.tif _tmp3.tif
	gdalwarp -te $newminx $newminy $newmaxx $newmaxy -dstnodata 1 -co COMPRESS=LZW _tmp2.tif _tmp5.tif
	gdal_translate _tmp5.tif ${outputprefix}_EW.tif -of GTiff -a_nodata none
	rm _tmp5.tif _tmp2.tif

	# NS image
	gdal_merge.py -createonly -init 0 -o _tmp.tif $input
	gdalwarp -te $newminx $miny $newmaxx $maxy -dstnodata 0 -co COMPRESS=LZW _tmp.tif _tmp2.tif
	gdal_translate _tmp2.tif _tmp3.tif -co COMPRESS=LZW -a_nodata none
	rm _tmp.tif _tmp2.tif
	gdalwarp -te $newminx $newminy $newmaxx $maxy -dstnodata 2 -co COMPRESS=LZW _tmp3.tif _tmp.tif
	gdal_translate _tmp.tif _tmp2.tif -co COMPRESS=LZW -a_nodata none
	rm _tmp.tif _tmp3.tif
	gdalwarp -te $newminx $newminy $newmaxx $newmaxy -dstnodata 1 -co COMPRESS=LZW _tmp2.tif _tmp5.tif
	gdal_translate _tmp5.tif ${outputprefix}_NS.tif -of GTiff -a_nodata none
	rm _tmp5.tif _tmp2.tif
fi
