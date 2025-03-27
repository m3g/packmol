####################################################################################################

#
# How to release the software:
#
# run ./release. 21.0.1 (for example)
#
# go to the github site and release the version corresponding to the new tag.
#

# Software name:

package=packmol

# Release version, read from command line:
version="$1"

# GIT URL:

giturl=https://github.com/m3g/packmol

# Name of files containing version number

versionfile=./src/title.f90
fpmfile=./fpm.toml

####################################################################################################

#git log --pretty=oneline 16.323...16.330 | awk '{$1=""; print "-"$0}'

year=`date +%y`
day=`date +%j`
#version="${year:0:1}${year:1:1}.$day"
if [[ $version < " " ]]; then
  version=`grep version fpm.toml`
  echo "ERROR: Please provide version number, with: ./release.sh 20.1.1"
  echo "       current $version"
  exit
fi

cat $versionfile | sed -e "s/Version.*/Version\ $version \')\")/" > tmpfile.txt
\mv -f tmpfile.txt $versionfile

cat $fpmfile | sed -e "s/version.*/version = \"$version\"/" > tmpfile.txt
\mv -f tmpfile.txt $fpmfile

git add -A .
git commit -m "Changed version file to $version"
git tag -a "v$version" -m "Release $version"
git push origin master tag "v$version"

echo "----------------------"
echo "CHANGE LOG:"
echo "----------------------"
range=`git tag | sort -V | tail -n 2 | xargs | sed 's! !...!'`
git log --pretty=oneline $range | awk '{$1=""; print "-"$0}'
echo "----------------------"

echo " Done. "   