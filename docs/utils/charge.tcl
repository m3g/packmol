#! /usr/bin/tclsh
#
# Counts the number of charged amino acid in a PDB file
# and computes the charge considering neutral histidines.
#
# usage: ./charge 1NAX.pdb
#
# L. Martinez (Jul 04)
#
if { $argv < " " } {
  puts " Run with: charge.tcl structure.pdb "
  exit
}
set file [open $argv r]
puts "-----------"
puts $argv
puts "-----------"
set data [read $file]
close $file
set charge 0
set nhis 0; set narg 0; set nlys 0; set nasp 0; set nglu 0 
set data [split $data "\n"]
foreach line $data {
  if { [ string trim [ string range $line 12 15 ] ] == "CA" } { 
    set amino [string range $line 17 19]
    if { $amino == "HIS" } { incr nhis }
    if { $amino == "HSD" } { incr nhis }
    if { $amino == "HSE" } { incr nhis }
    if { $amino == "ARG" } { incr narg }
    if { $amino == "LYS" } { incr nlys }
    if { $amino == "ASP" } { incr nasp }
    if { $amino == "GLU" } { incr nglu }
  }
}
set charge [ expr $narg + $nlys - $nasp - $nglu ]
puts "Charge = $charge"
puts "-----------"
puts "HIS = $nhis"
puts "ARG = $narg"
puts "LYS = $nlys"
puts "GLU = $nglu"
puts "ASP = $nasp"
puts "-----------"
