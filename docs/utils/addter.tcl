#!/usr/bin/tclsh

set argv [ split $argv " " ]
set i 0
foreach arg $argv {
  if { $arg > " " } {
    incr i
    set file($i) $arg
  } 
}
set input $file(1)

set input [ open $input r ]
set input [ read $input ]
set input [ split $input "\n" ]

set residlast "xxxx"
set chainlast "xxxx"
set indexlast "xxxx"
foreach line $input {
  if { [ string range $line 0 3 ] == "ATOM" | \
       [ string range $line 0 5 ] == "HETATM" } {
    set resid [ string range $line 17 20 ]
    set chain [ string range $line 21 21 ]
    set index [ string range $line 22 25 ]
    if { $resid == $residlast & \
         $chain == $chainlast & \
         $index == $indexlast } {
      puts $line
    } elseif { $residlast == "xxxx" } {
      puts $line
    } else {
      puts "TER"
      puts $line
    }
    set residlast $resid
    set chainlast $chain
    set indexlast $index
  } else {
    set residlast "xxxx"
    set chainlast "xxxx"
    set indexlast "xxxx"
    puts $line
  }
}

