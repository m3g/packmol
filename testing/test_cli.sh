#!/bin/bash
#
# Synopsis:
# Test the command-line interface by checking if executing packmol with command-line
# arguments changes results. The script checks for differences in the output files with
# the GNU diffutil. All the supported ways to invoke packmol are considered.
#
# Written by Misael Díaz-Maldonado, 2025
#
# Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
# Ernesto G. Birgin.
#
# Example invokation of this shell script: ./test_cli.sh /usr/bin/packmol
#
# NOTES:
# We skip tests designed to fail and we patch input files with a seed equal to -1 because
# that will also fail our test because that signals packmol to generate a seed based on
# the current time and so results are expected to be different.
#

verbose=0

# checks number of command-line arguments given
if [ "$#" -ne 1 ] ; then
	echo "Error: expects the path to packmol as the first command-line argument"
	echo "Example: ./test_cli.sh /usr/bin/packmol"
	exit 1
fi

packmol="$1"
# checks that we can execute packmol
if ! [ -f "$packmol" ] ; then
	echo "Error: packmol does not exist"
	exit 1
fi

if ! [ -x "$packmol" ] ; then
	echo "Error: cannot execute packmol"
	exit 1
fi

# checks that the input_files directo exists
if ! [ -d input_files ] ; then
	echo "Error: the 'input_files' directory does not exist"
	exit 1
fi

# checks that all the commands this script depends exists for the sake of completeness
ls=$(which ls)
if ! [ -x "$ls" ] ; then
	echo "Error: ls command not found"
	exit 1
fi

cp=$(which cp)
if ! [ -x "$cp" ] ; then
	echo "Error: cp command not found"
	exit 1
fi

rm=$(which rm)
if ! [ -x "$rm" ] ; then
	echo "Error: rm command not found"
	exit 1
fi

wc=$(which wc)
if ! [ -x "$wc" ] ; then
	echo "Error: wc command not found"
	exit 1
fi

cat=$(which cat)
if ! [ -x "$cat" ] ; then
	echo "Error: cat command not found"
	exit 1
fi

sed=$(which sed)
if ! [ -x "$sed" ] ; then
	echo "Error: sed command not found"
	exit 1
fi

diff=$(which diff)
if ! [ -x "$diff" ] ; then
	echo "Error: diff command not found"
	exit 1
fi

grep=$(which grep)
if ! [ -x "$grep" ] ; then
	echo "Error: grep command not found"
	exit 1
fi

uname=$(which uname)
if ! [ -x "$uname" ] ; then
	echo "Error: uname command not found"
	exit 1
fi

os=$("$uname")
# OSX does not ship with GNU gawk by default so we need to use what they provide
if [ "Darwin" == "$os" ] ; then
	gawk=$(which awk)
else
	gawk=$(which gawk)
fi

if ! [ -x "$gawk" ] ; then
	echo "Error: gawk command not found"
	exit 1
fi

# for each input file in the input_files directory run packmol and check that we get the
# same results (same contents in the output file) regardless of the invokation method
# used to execute packmol
"$ls" input_files/*.inp | while read input_file
do
	# here we use pattern matching to exclude input_files that we know would fail
	# because they are meant to fail; since that would mess our test we filter
	# those out with grep
	sw=$(echo "$input_file" | "$grep" -v "error" | "$grep" -v "fail" | "$wc" -c)
	if [ "$sw" -eq 0 ] ; then
		if [ "$verbose" -eq 1 ] ; then
			echo "Skipping test $input_file because it is designed to fail"
		fi
		continue
	fi

	# we need to patch some of the data in the original input files this is why we
	# create .txt and .tmp versions; we use regex to replace the .inp with either
	# .txt or .tmp extensions
	input_txt=$(\
		echo "$input_file" |\
		"$sed" 's/\.inp/.txt/g'
	)

	input_tmp=$(\
		echo "$input_file" |\
		"$sed" 's/\.inp/.tmp/g'
	)

	# We fix the `seed` value on those input_files that set it to -1; otherwise
	# packmol could yield different results and we don't want that of course, we
	# want reproducible results for the same input. That is why we provide a fix
	# value for the seed.
	"$cp" "$input_file" "$input_txt"
	"$sed" -e 's/seed -1/seed 1024/g' -i'.tmp' "$input_txt"
	"$cp" "$input_txt" "$input_tmp"
	"$sed" -e 's/output\.pdb/output.tmp/g' -i'.tmp' "$input_tmp"
	"$rm" "$input_txt.tmp"
	"$rm" "$input_tmp.tmp"

	# here we extract the name of the output file from the input-file
	output_pdb=$(\
		"$cat" "$input_txt" | \
		"$grep" "output" | \
		"$gawk" '{print $2}'
	)

	# uses substitution to produce an output file with a .txt file extension
	output_txt=$(\
		echo "$output_pdb" | \
		"$sed" 's/\.pdb/.txt/g'
	)

	# uses substitution to produce an output file with a .tmp file extension
	output_tmp=$(\
		echo "$output_pdb" | \
		"$sed" 's/\.pdb/.tmp/g'
	)

	echo -n "Running test $input_txt ... "

	# runs packmol in the usual way with input redirection
	if ! "$packmol" < "$input_txt" 1>/dev/null 2>/dev/null ; then
		echo "FAIL"
		"$rm" -f "$input_tmp"
		"$rm" -f "$input_txt"
		"$rm" -f "$output_txt"
		"$rm" -f "$output_pdb"
		"$rm" -f "$output_tmp"
		exit 1
	fi

	# Provides the input file via command-line to packmol and executes it;
	# we redirect the standard output and error to the null-device to keep
	# our test output clean. In this case the output file is determined from
	# the input-file `input_tmp`; note that the output file will have a .tmp
	# file extension because of the preprocessing that we have done.
	if ! "$packmol" -i "$input_tmp" 1>/dev/null 2>/dev/null ; then
		echo "FAIL"
		"$rm" -f "$input_tmp"
		"$rm" -f "$input_txt"
		"$rm" -f "$output_txt"
		"$rm" -f "$output_pdb"
		"$rm" -f "$output_tmp"
		exit 1
	fi

	# provides both the input and output files to packmol and executes it
	if ! "$packmol" -i "$input_txt" -o "$output_txt" 1>/dev/null 2>/dev/null ; then
		echo "FAIL"
		"$rm" -f "$input_tmp"
		"$rm" -f "$input_txt"
		"$rm" -f "$output_txt"
		"$rm" -f "$output_pdb"
		"$rm" -f "$output_tmp"
		exit 1
	fi

	# here we check that the output files are identical via GNU diffutil;
	# to do that we concatenate the diff contents and determine the number of
	# bytes (we should get zero bytes when they are the same)
	res1=$("$diff" --normal "$output_pdb" "$output_txt" | "$wc" -c)
	res2=$("$diff" --normal "$output_pdb" "$output_tmp" | "$wc" -c)
	if [ "$res1" -eq 0 ] && [ "$res2" -eq 0 ] ; then
		echo "OK"
	else
		echo "FAIL"
		"$rm" -f "$input_tmp"
		"$rm" -f "$input_txt"
		"$rm" -f "$output_txt"
		"$rm" -f "$output_pdb"
		"$rm" -f "$output_tmp"
		exit 1
	fi

	# we clean up the testing directory from our temporary files
	"$rm" -f "$input_tmp"
	"$rm" -f "$input_txt"
	"$rm" -f "$output_txt"
	"$rm" -f "$output_pdb"
	"$rm" -f "$output_tmp"
done

# if we get here packmol passed all the tests successfully
exit 0
