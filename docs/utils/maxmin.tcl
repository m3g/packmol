#!/usr/bin/tclsh
#
# Make it executable with: chmod +x maxmin.tcl
# Run with: ./maxmin.tcl
#
# L. Martinez, Feb 26, 2008. (leandromartinez98@gmail.com)
#

puts "-------------------------------------------------------"
puts " Gets maximum and minimum coordinates from a PDB file. "
puts " The selection will be defined below: "
puts " Use 'all' to consider all atoms in each field. "
puts "-------------------------------------------------------"

puts -nonewline " PDB file: "; flush stdout; gets stdin pdbfile
puts -nonewline " Segment: "; flush stdout; gets stdin segment
puts -nonewline " Residue type: "; flush stdout; gets stdin residue
puts -nonewline " Atom type: "; flush stdout; gets stdin atomtype
puts -nonewline " Atom index greater than: "; flush stdout; gets stdin firstatom
puts -nonewline " Atom index less than: "; flush stdout; gets stdin lastatom
set pdbfile [ string trim $pdbfile ]
set segment [ string trim $segment ]
set residue [ string trim $residue ]
set atomtype [ string trim $atomtype ]
set firstatom [ string trim $firstatom ] 
set lastatom [ string trim $lastatom ]

set file [ open $pdbfile r ]
set file [ read $file ]
set file [ split $file "\n" ]

set natoms 0
foreach line $file {
  if { [ string range $line 0 3 ] == "ATOM" |
       [ string range $line 0 5 ] == "HETATM" } {
    incr natoms
    set ss [ string trim [ string range $line 72 74 ] ]
    set rt [ string trim [ string range $line 17 20 ] ]
    set at [ string trim [ string range $line 12 15 ] ]
    set consider true
    if { $firstatom != "all" } { if { $natoms < $firstatom } { set consider false } }
    if { $lastatom != "all" } { if { $natoms > $lastatom } { set consier false } }
    if { $segment != "all" } { if { $ss != $segment } { set consider false } }
    if { $residue != "all" } { if { $rt != $residue } { set consider false } }
    if { $atomtype != "all" } { if { $at != $atomtype } { set consider false } }
    if { $consider == "true" } {
      set x [ string trim [ string range $line 30 37 ] ]
      set y [ string trim [ string range $line 38 45 ] ]
      set z [ string trim [ string range $line 46 54 ] ]
      if { [ info exists xmin ] == 1 } {
        if { $x < $xmin } { set xmin $x }
        if { $y < $ymin } { set ymin $y }
        if { $z < $zmin } { set zmin $z }
        if { $x > $xmax } { set xmax $x }
        if { $y > $ymax } { set ymax $y }
        if { $z > $zmax } { set zmax $z }
      } else {
        set xmin $x
        set ymin $y
        set zmin $z
        set xmax $x
        set ymax $y
        set zmax $z
      } 
    }
  }
}

puts "-------------------------------------------------------"
puts " Minimum and maximum coordinates of selected atoms: "
puts " X_min = $xmin     X_max = $xmax " 
puts " Y_min = $ymin     Y_max = $ymax " 
puts " Z_min = $zmin     Z_max = $zmax " 
puts "-------------------------------------------------------"
puts " Length in each direction: "
puts " x: [ expr $xmax - $xmin ] "
puts " y: [ expr $ymax - $ymin ] "
puts " z: [ expr $zmax - $zmin ] "
puts "-------------------------------------------------------"

