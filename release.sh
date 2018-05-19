#!/bin/bash
####################################################################################################

# Software name:

package=packmol

# HTML file containing download list:

downloads=/home/leandro/public_html/packmol/versionhistory/index.html

# GIT URL:

giturl=https://github.com/leandromartinez98/packmol

# Name of file containing version number

versionfile=./title.f90

####################################################################################################

#git log --pretty=oneline 16.323...16.330 | awk '{$1=""; print "-"$0}'

year=`date +%y`
day=`date +%j`
version="${year:0:1}${year:1:1}.$day"

file="$package-$version.tar.gz" 
version_file=$version

i=2
while grep -q $file $downloads ; do
  file=$package-$version.$i.tar.gz
  version_file=$version.$i
  i=`expr $i + 1`
done
version=$version_file
file=$package-$version.tar.gz
echo "Will create file: $file"

cat $versionfile | sed -e "s/Version.*/Version\ $version \')\")/" > version_title_temp.f90
\mv -f version_title_temp.f90 $versionfile

git add -A .
git commit -m "Changed version file to $version"
git tag -a $version -m "Release $version"
git push origin master tag $version

today=`date +"%b %d, %Y"`
changelog="https://github.com/leandromartinez98/$package/releases/tag/$version"
newline="<tr><td width=190px valign=top><a href=$giturl/archive/$version.tar.gz> $file </a></td><td> Released on $today - <a target=newpage href=$changelog> [change log at github] </a></td></tr>"
htmlfile=$downloads

echo "------------------------------"
echo "CREATING RELEASE IN HOME-PAGE:"
echo "------------------------------"
mkdir TEMP
cd TEMP
wget https://github.com/leandromartinez98/packmol/archive/$version.tar.gz 
tar -xf $version.tar.gz
mv packmol-$version packmol
tar -cf packmol.tar ./packmol
gzip packmol.tar
#scp packmol.tar.gz martinez@ssh.ime.unicamp.br:./public_html/packmol/
\cp -f packmol.tar.gz ~/public_html/m3g/packmol/packmol.tar.gz
~/public_html/m3g/update.sh
cd ..
\rm -rf ./TEMP

writeline=yes
while IFS= read -r line ; do
 
  if [ "$writeline" = "yes" ] ; then
    echo $line >> ./htmlfile_new_temp
  fi
  if [ $writeline = "no" ] ; then
    writeline="yes"
  fi

  if [[ $line == *NEW_VERSION_HERE* ]] ; then
    echo $newline >> ./htmlfile_new_temp
  fi

done < "$htmlfile"
rm $htmlfile
mv htmlfile_new_temp $htmlfile   

echo "----------------------"
echo "CHANGE LOG:"
echo "----------------------"
range=`git tag | tail -n 2 | xargs | sed 's! !...!'`
git log --pretty=oneline $range | awk '{$1=""; print "-"$0}'
echo "----------------------"

echo " Done. " 

