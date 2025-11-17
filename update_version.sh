####################################################################################################
#
# How to update the version number:
#
# run: ./update_version 21.0.1 (for example)
#

# Software name:

package=packmol

# Release version, read from command line:
version="$1"

# Name of files containing version number

versionfile=./src/title.f90
fpmfile=./fpm.toml
pyprojfile=./pyproject.toml

####################################################################################################
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

cat $pyprojfile | sed -e "s/version.*/version = \"$version\"/" > tmpfile.txt
\mv -f tmpfile.txt $pyprojfile

echo "----------------------"
echo "Please update the CHANGELOG.md file. Commits:"
echo "----------------------"
range=`git tag | sort -V | tail -n 2 | xargs | sed 's! !...!'`
git log --pretty=oneline $range | awk '{$1=""; print "-"$0}'
echo "----------------------"

echo " Done. "   
