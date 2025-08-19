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

if [ "$#" -ne 1 ] ; then
	echo "Error: expects the path to packmol as the first command-line argument"
	echo "Example: ./test_cli.sh /usr/bin/packmol"
	exit 1
fi

packmol="$1"
if ! [ -f "$packmol" ] ; then
	echo "Error: packmol does not exist"
	exit 1
fi

if ! [ -x "$packmol" ] ; then
	echo "Error: cannot execute packmol"
	exit 1
fi

if ! [ -d input_files ] ; then
	echo "Error: the 'input_files' directory does not exist"
	exit 1
fi

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
if [ "Darwin" == "$os" ] ; then
	gawk=$(which awk)
else
	gawk=$(which gawk)
fi

if ! [ -x "$gawk" ] ; then
	echo "Error: gawk command not found"
	exit 1
fi

pass=0
"$ls" input_files/*.inp | while read input_file
do
	sw=$(echo "$input_file" | "$grep" -v "error" | "$grep" -v "fail" | "$wc" -c)
	if [ "$sw" -eq 0 ] ; then
		if [ "$verbose" -eq 1 ] ; then
			echo "Skipping test $input_file because it is designed to fail"
		fi
		continue
	fi

	input_txt=$(\
		echo "$input_file" |\
		"$sed" 's/\.inp/.txt/g'
	)

	input_tmp=$(\
		echo "$input_file" |\
		"$sed" 's/\.inp/.tmp/g'
	)

	"$cp" "$input_file" "$input_txt"
	"$sed" -e 's/seed\s\+-1/seed 1024/g' -i'.tmp' "$input_txt"
	"$cp" "$input_txt" "$input_tmp"
	"$sed" -e 's/output\.pdb/output.tmp/g' -i'.tmp' "$input_tmp"

	output_pdb=$(\
		"$cat" "$input_txt" | \
		"$grep" "output" | \
		"$gawk" '{print $2}'
	)

	output_txt=$(\
		echo "$output_pdb" | \
		"$sed" 's/\.pdb/.txt/g'
	)

	output_tmp=$(\
		echo "$output_pdb" | \
		"$sed" 's/\.pdb/.tmp/g'
	)

	echo -n "Running test $input_txt ... "

	if ! "$packmol" < "$input_txt" 1>/dev/null 2>/dev/null ; then
		echo "FAIL"
		"$rm" -f "$input_tmp"
		"$rm" -f "$input_txt"
		"$rm" -f "$output_txt"
		"$rm" -f "$output_pdb"
		"$rm" -f "$output_tmp"
		continue
	fi

	if ! "$packmol" -i "$input_tmp" 1>/dev/null 2>/dev/null ; then
		echo "FAIL"
		"$rm" -f "$input_tmp"
		"$rm" -f "$input_txt"
		"$rm" -f "$output_txt"
		"$rm" -f "$output_pdb"
		"$rm" -f "$output_tmp"
		continue
	fi

	if ! "$packmol" -i "$input_txt" -o "$output_txt" 1>/dev/null 2>/dev/null ; then
		echo "FAIL"
		"$rm" -f "$input_tmp"
		"$rm" -f "$input_txt"
		"$rm" -f "$output_txt"
		"$rm" -f "$output_pdb"
		"$rm" -f "$output_tmp"
		continue
	fi

	res1=$("$diff" --normal "$output_pdb" "$output_txt" | "$wc" -c)
	res2=$("$diff" --normal "$output_pdb" "$output_tmp" | "$wc" -c)
	if [ "$res1" -eq 0 ] && [ "$res2" -eq 0 ] ; then
		echo "OK"
	else
		echo "FAIL"
		pass=1
	fi

	"$rm" -f "$input_tmp"
	"$rm" -f "$input_txt"
	"$rm" -f "$output_txt"
	"$rm" -f "$output_pdb"
	"$rm" -f "$output_tmp"
done

if [ "$pass" -eq 0 ] ; then
	exit 0
else
	exit 1
fi
