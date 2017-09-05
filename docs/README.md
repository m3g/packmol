{::nomarkdown}

<!-- https://github.com/leandromartinez98/packmol/raw/master/docs/imgs/pbc.jpg -->

<h3>User guide</h3>
<i>Important:</i> always download the latest version of Packmol
in order that all features are available.
<br><br>

<!-- START SECTION -->
<b> Contents </b>

<br>
<a href="#need">1. What do you need?</a><br>
<a href="#comp">2. How to compile Packmol.</a><br>
<a href="#run">3. Running Packmol.</a><br>
<a href="#basic">4. Basic input structure.</a><br>
<a href="#more">5. More types of molecules.</a><br>
<a href="#atom">6. Atom selections.</a><br>
<a href="#types">7. Types of constraints.</a><br>
<a href="#pbc">8. Periodic boundary conditions.</a><br>
<a href="#radii">9. Different radii for different atoms.</a><br>
<a href="#numb">10. Controlling residue numbering in PDB files.</a><br>
<a href="#restart">11. Building very large systems: using restart files.</a><br>
<a href="#conv">12. Convergence problems: what to try.</a><br>
<a href="#add">13. Additional input options and keywords.</a>
<br><br>

<!-- END SECTION -->


<!-- START SECTION -->
<a name="need"></a><b>What do you need? </b>
<br><br>

You need coordinate files for each type of molecule you want your
simulation to have. For example, if you are going to simulate a solution
of water and ions, you will need a coordinate file for <i>one</i>  water
molecule, and independent coordinates files for each of the ions. This
coordinate files may be in the PDB, TINKER, MOLDEN or MOLDY format.
<br><br>
Of course, you also need the <tt>Packmol</tt> package, which you can get from
<pre><tt>  http://www.ime.unicamp.br/~martinez/packmol
</tt></pre>
or by clicking on the <a target="contents" href="https://github.com/leandromartinez98/packmol/releases/latest">
[Latest Release]</a> link. By following this link you will download the file
<tt>packmol.tar.gz</tt> which contains the whole source code of
Packmol. 
<br><br>

<!-- END SECTION -->

<!-- START SECTION -->
<a name="comp"></a><b>How to compile Packmol </b>
<br><br>

Once you have downloaded the <tt>packmol.tar.gz</tt> file from the home-page,
you need to expand the files and compile the package. This is done by:
<br><br>
Expanding the files:
<pre>tar -xvzf packmol.tar.gz</pre>
This will create a directory called <tt>packmol</tt> inside which you can find
the source code. You can build the executable by:
<pre>
  cd packmol
  make
</pre>
<br>
That's it, if no error was reported the packmol executable was 
built.
<br><br>
-----
<br><br>
If you have problems, let the configure script find a suitable
compiler for you:<br><br>
<tt>chmod +x ./configure </tt>  (this makes the script executable)
<br><br>
<tt>./configure </tt> (this executes the script)
<br><br>
If the script was not able to find a suitable compiler, then
you can manually set the compiler by:
<br><br>
<tt>./configure /path/to/your/compiler/yourcompiler</tt>
<br><br>
Then, run the "make" command again:
<br><br>
<tt>make</tt>
<br><br>
If no error was
detected, an executable called packmol is now ready.
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="run"></a><b>Running Packmol </b>
<br><br>

Once you have compiled and built your input file, run Packmol  with
<br>
<pre>
packmol < packmol.inp
</pre>
<br>
Were packmol.inp is the input file (you can obtain example files by
clicking at the 'Input examples' link on the left). <br><br> 
A successful packing will end with something like
<br>
<pre>
-------------------------------------------------
                Success!
Final objective function value: .22503E-01
Maximum violation of target distance:   0.000000
Maximum violation of the constraints: .78985E-02
-------------------------------------------------
</pre>
<br>
Where the maximum violation of the target distance indicates the
difference between the minimum distance between atoms required by
the user and that of the solution. It will not be greater than 10<sup>-2</sup> 
The maximum violation of the constrains must not be greater than 10<sup>-2</sup>.
<br><br>
A good idea is to check if your constraints are correct by using
the "check" keyword in the input file. With this option a rough
initial approximation will be built but no actual packing will
be performed. You can look at the output to see if the molecules
are within the desired regions (but do not expect a good structure
at this point!). Just add the word "check" to any line of your
input file (available since 28 Feb 2008).
<br><br>
<hr
><br>
<u>Common issues:</u> <br><br>
 - If you get "Command not found" when running Packmol, use <br>
<pre>
./packmol < packmol.inp
</pre>
(with a "./" before "packmol") 
or add the directory where the packmol executable is located to your path.
<br><br>
 - If you run packmol and get the message "Killed", this is because the
package is trying to allocate more memory than available for static                          
storage. Open the "sizes.i" file and decrease the "maxatom" parameter to
the number of atoms of your system, compile the package again, and try
again.
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="basic"></a><b>Basic input structure </b>
<br><br>

The minimal input file must contain the distance tolerance required (for
systems at room temperature and pressure and coordinates in Angstroms,
2.0 Å is a good value. This is specified with
<br><br>
<tt>tolerance 2.0</tt>
<br><br>
The file must contain also the name of the output file to be created,
specified with
<br><br>
<tt>output test.pdb</tt>
<br><br>
and the file type (pdb, tinker, xyz or moldy, pdb is the default value),
<br><br>
<tt>filetype pdb</tt>
<br><br>
At least one type of molecule must be present. This is set by the
<tt>structure ... end structure</tt> section, for example, if
<tt>water.pdb</tt> is the
file containing the coordinates of a single water molecule, you could
add to your input file something like
<br>
<pre>
structure water.pdb
  number 2000
  inside cube 0. 0. 0. 40. 
end structure
</pre>
<br>
This section specifies that 2000 molecules of the <tt>water.pdb</tt> type, will
be placed inside a cube with minimum coordinates
(<i>x</i>,<i>y</i>,<i>z</i>) = (0,0,0) and
maximum coordinates (40,40,40). Therefore, this minimum input file must
be:
<br>
<pre>
tolerance 2.0
output test.pdb
filetype pdb
structure water.pdb
  number 2000 
  inside cube 0. 0. 0. 40. 
end structure
</pre>
<br>
Running Packmol with this input file will fill a cube of side 40.0 Å
with 2000 water molecules. Every pair of atoms of different molecules
will be separated by, at least, 2.0 Å and the molecules will be
randomly distributed inside de cube.
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="more"></a><b>More types of molecules </b>
<br><br>

You can add more types of molecules to the same region, or to different
regions of the space, simply adding other <tt>structure ... end
structure</tt>
section to the input file.
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="atom"></a><b>Atom selections </b>
<br><br>

The coordinate file of a single molecule contains, for example, 10
atoms. You can restrain a part of the molecule to be in a specified
region of the space. This is useful for building vesicles where the
hydrophilic  part of the surfactants must be pointing to the aqueous
environment, for example. For the 10 atoms molecule, this is done by
using the keyword atoms, as in
<br>
<pre>
structure molecule.pdb     
  inside cube 0. 0. 0. 20.
  atoms 9 10              
    inside box 0. 0. 15. 20. 20. 20.
  end atoms 
end structure                     
</pre>
<br>
In this case, all the atoms of the molecule will be put inside the
defined cube, but atoms 9 and 10 will be restrained to be inside the
box.
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="types"></a><b>Types  of constraints </b>
<br><br>

There are several types of constraints that can be applied both to whole
molecules or to parts of the molecule. These constraints define the
region of the space in which the molecules must be at the solution. Very
ordered systems can be built in such a way. The constraints  are:
<br><br>

<table align="center" width="80%">
<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 1. </td>
<td valign="bottom"><tt>
fixed
</tt></td>
</tr>

<tr><td></td>
<td>
Usage: <tt>fixed </tt><i>x   y   z  a   b   g</i><br><br>
This options holds the molecule
fixed in the position specified by the parameters. <i>x, y, z, a, b,
g,</i>
which are six real numbers. The first three determine the translation of
the molecule relative to its position in the coordinate file. The former
three parameters are rotation angles (in radians). For this option it is
required that only one molecule is set. It may be accompanied by the
keyword <tt>center</tt>. If this keyword is present the first three numbers
are the position of the baricenter (not really the center of mass,
because we suppose that all atoms have the same mass). Therefore this
keyword must be used in the following context:
<br><br>
<pre>
structure molecule.pdb
  number 1  
  center    
  fixed 0. 0. 0. 0. 0. 0.
end structure
</pre>
<br><br>
In this example, the molecule will be fixed with its center 
the origin and no rotation.

<br><br></td></tr>
<!-- END CONSTRAINT -->


<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
2. 
</td>
<td valign="bottom"><tt>
inside cube
</tt></td>
</tr>

<tr><td></td>
<td>
Usage: <tt>inside cube </tt><i>x<sub>min</sub>&nbsp; y<sub>min</sub>&nbsp; z<sub>min</sub>&nbsp;    d</i><br><br>

<i>x<sub>min</sub> ,  y<sub>min</sub> ,  z<sub>min</sub></i> and <i>d</i>
are four real numbers. The coordinates (<i>x</i>,<i>y</i>,<i>z</i>) of the atoms restrained
by this option will satisfy, at the solution:
<center>
<br><br>
<i>x<sub>min</sub></i> < <i>x</i> < <i>x<sub>min</sub></i> + <i>d</i><br>
<i>y<sub>min</sub></i> < <i>y</i> < <i>y<sub>min</sub></i> + <i>d</i><br>
<i>z<sub>min</sub></i> < <i>z</i> < <i>z<sub>min</sub></i> + <i>d</i>
</center>

<br><br></td></tr>
<!-- END CONSTRAINT -->


<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
3. 
</td>
<td valign="bottom"><tt>
outside cube
</tt></td>
</tr>

<tr><td></td>
<td>
Usage: <tt>outside cube </tt><i>x<sub>min</sub>&nbsp; y<sub>min</sub>&nbsp; z<sub>min</sub>&nbsp;    d</i><br><br>

<i>x<sub>min</sub> ,  y<sub>min</sub> ,  z<sub>min</sub></i> and <i>d</i>
are four real numbers. The coordinates (<i>x</i>,<i>y</i>,<i>z</i>) of the atoms restrained
by this option will satisfy, at the solution:
<center>
<br><br>
<i>x</i> < <i>x<sub>min</sub></i> or <i>x</i> > <i>x<sub>min</sub></i> + <i>d</i><br>
<i>y</i> < <i>y<sub>min</sub></i> or <i>y</i> > <i>y<sub>min</sub></i> + <i>d</i><br>
<i>z</i> < <i>z<sub>min</sub></i> or <i>z</i> > <i>z<sub>min</sub></i> + <i>d</i>
</center>

<br><br></td></tr>
<!-- END CONSTRAINT -->


<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
4. 
</td>
<td valign="bottom"><tt>
inside box
</tt></td>
</tr>

<tr><td></td>
<td>
Usage: <tt>inside box</tt>&nbsp; 
<i>x<sub>min</sub>&nbsp;
y<sub>min</sub>&nbsp;
z<sub>min</sub>&nbsp;
x<sub>max</sub>&nbsp;   
y<sub>max</sub>&nbsp; 
z<sub>max</sub></i><br><br>

<i>x<sub>min</sub>&nbsp;,
y<sub>min</sub>&nbsp;,
z<sub>min</sub>&nbsp;,
x<sub>max</sub>&nbsp;,   
y<sub>max</sub>&nbsp;</i> and <i> 
z<sub>max</sub></i>
are six real numbers. The coordinates
(<i>x</i>,<i>y</i>,<i>z</i>) of the atoms restrained by this option will satisfy, at the
solution:
<br><br>
<center>
<i>x<sub>min</sub></i> < <i>x</i> < <i>x<sub>max</sub></i><br>
<i>y<sub>min</sub></i> < <i>y</i> < <i>y<sub>max</sub></i><br>
<i>z<sub>min</sub></i> < <i>z</i> < <i>z<sub>max</sub></i>
</center>


<br><br></td></tr>
<!-- END CONSTRAINT -->


<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
5. 
</td>
<td valign="bottom"><tt>
outside box
</tt></td>
</tr>

<tr><td></td>
<td>
Usage: <tt>outside box</tt>&nbsp; 
<i>x<sub>min</sub>&nbsp;
y<sub>min</sub>&nbsp;
z<sub>min</sub>&nbsp;
x<sub>max</sub>&nbsp;   
y<sub>max</sub>&nbsp; 
z<sub>max</sub></i><br><br>

<i>x<sub>min</sub>&nbsp;,
y<sub>min</sub>&nbsp;,
z<sub>min</sub>&nbsp;,
x<sub>max</sub>&nbsp;,   
y<sub>max</sub>&nbsp;</i> and <i> 
z<sub>max</sub></i>
are six real numbers. The coordinates
(<i>x</i>,<i>y</i>,<i>z</i>) of the atoms restrained by this option will satisfy, at the
solution:
<br><br>
<center>
<i>x</i> < <i>x<sub>min</sub></i> or <i>x</i> > <i>x<sub>max</sub></i><br>
<i>y</i> < <i>y<sub>min</sub></i> or <i>y</i> > <i>y<sub>max</sub></i><br>
<i>z</i> < <i>z<sub>min</sub></i> or <i>z</i> > <i>z<sub>max</sub></i>
</center>
<br><br></td></tr>
<!-- END CONSTRAINT -->


<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
6. 
</td>
<td valign="bottom"><tt>
inside (or outside) sphere
</tt></td>
</tr>
<tr><td></td>
<td>
Spheres are defined by equations of the general form <br><br>
<center>
<img src=https://github.com/leandromartinez98/packmol/raw/master/docs//imgs/img31.png>
</center>
<br>
and, therefore, you must provide four real parameters <i>a</i>,
<i>b</i>, <i>c</i> and <i>d</i>  in
order to define it. The input syntax is, for example,
<br><br><tt>
inside sphere 2.30 3.40 4.50 8.0
</tt><br><br>
and therefore the coordinates of the atoms will satisfy the equation
<br><br>
<center>
<img src=https://github.com/leandromartinez98/packmol/raw/master/docs//imgs/img36.png>
</center><br>  
Other input alternative would be:
<br><br><tt>
outside sphere 2.30 3.40 4.50 8.0
</tt><br><br>
The <tt>outside</tt> parameter is similar to the <tt>inside</tt> parameter, but the
equation above uses <img src=https://github.com/leandromartinez98/packmol/raw/master/docs//imgs/img37.png>  instead of  <img src=https://github.com/leandromartinez98/packmol/raw/master/docs//imgs/img38.png> 
and, therefore, the
atoms will be placed outside the defined sphere.
<br><br></td></tr>
<!-- END CONSTRAINT -->


<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
7. 
</td>
<td valign="bottom"><tt>
inside (or outside) ellipsoid 
</tt></td>
</tr>

<tr><td></td>
<td>
Ellipsoids are defined by the general equation
<br><br>
<center>
<img src=https://github.com/leandromartinez98/packmol/raw/master/docs//imgs/img39.png>
</center>
<br>
The parameters must be given as in the sphere example, but now they are
7, and must be entered in the following order:
<br><br><tt>
inside ellipsoid</tt> <i>&nbsp;
a<sub>1</sub>&nbsp;  
b<sub>1</sub>&nbsp;  
c<sub>1</sub>&nbsp;  
a<sub>2</sub>&nbsp;  
b<sub>2</sub>&nbsp;
c<sub>2</sub>&nbsp;  
d</i>
<br><br>
The coordinates
(<i>a<sub>1</sub></i>,<i>b<sub>1</sub></i>,<i>c<sub>1</sub></i>) 
will define the center of the ellipsoid, the
coordinates 
(<i>a<sub>2</sub></i>,<i>b<sub>2</sub></i>,<i>c<sub>2</sub></i>) 
will define the relative size of the axes and <i>d</i>
will define the volume of the ellipsoid. Of course, the commands
<br><br><tt>
outside ellipsoid</tt> <i>&nbsp;
a<sub>1</sub>&nbsp;  
b<sub>1</sub>&nbsp;  
c<sub>1</sub>&nbsp;  
a<sub>2</sub>&nbsp;  
b<sub>2</sub>&nbsp;
c<sub>2</sub>&nbsp;  
d</i>
<br><br>
can also be used in the same manner as the parameters for spheres. Note
that the case 
<i>a<sub>2</sub></i> = 
<i>b<sub>2</sub></i> = 
<i>c<sub>2</sub></i> = 1.0 provides the exactly the same as the
sphere parameter. The parameters for the ellipsoid are not normalized.
Therefore, if 
<i>a<sub>2</sub></i>, 
<i>b<sub>2</sub></i> and 
<i>c<sub>2</sub></i> are large, the ellipsoid will be large, even
for a small <i>d</i>.
<br><br></td></tr>
<!-- END CONSTRAINT -->


<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
8. 
</td>
<td valign="bottom"><tt>
over (or below) plane 
</tt></td>
</tr>

<tr><td></td>
<td>
The planes are defined by the general equation
<br><br>
<center>
<i>ax</i> + <i>by</i> + <i>cz</i> - <i>d</i> = 0
</center><br>
And it is possible to restrict atoms to be over or below the plane. The syntax is
<br><pre>
over plane 2.5 3.2 1.2 6.2

below plane 2.5 3.2 1.2 6.2
</pre><br>
where the <tt>over</tt> keyword will make the atoms satisfy the condition
<br><center>
2.5<i>x</i> + 3.2<i>y</i> + 1.2<i>z</i> - 6.2 <img src=https://github.com/leandromartinez98/packmol/raw/master/docs//imgs/img37.png> 0
</center><br><br>
the <tt>below</tt> keyword will make the atoms satisfy
<br><br><center>
2.5<i>x</i> + 3.2<i>y</i> + 1.2<i>z</i> - 6.2 <img src=https://github.com/leandromartinez98/packmol/raw/master/docs//imgs/img38.png> 0
</center><br>
<br><br></td></tr>
<!-- END CONSTRAINT -->


<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
9. 
</td>
<td valign="bottom"><tt>
inside (or outside) cylinder
</tt></td>
</tr>
<tr><td></td>
<td>

In order to define a cylinder, it is necessary first to define a
line oriented in space. This line is defined in <tt>Packmol</tt> by the
parametric equation
<br><br><center>
<i>p</i> = (
<i>a<sub>1</sub>&nbsp;</i>,
<i>b<sub>1</sub>&nbsp;</i>,
<i>c<sub>1</sub>&nbsp;</i>) + <i>t</i>&nbsp;(
<i>a<sub>2</sub>&nbsp;</i>,
<i>b<sub>2</sub>&nbsp;</i>,
<i>c<sub>2</sub></i>)
</center><br>
where <i>t</i> is the independent parameter. The vector 
(<i>a<sub>2</sub>&nbsp;</i>,
<i>b<sub>2</sub>&nbsp;</i>,
<i>c<sub>2</sub></i>)                        
defines the
direction of the line. The cylinder is therefore defined by the distance
to this line, <i>d</i>, and a length <i>l</i>. Therefore, the usage must be:
<br><br>
inside cylinder 
&nbsp;<i>a<sub>1</sub></i>  
&nbsp;<i>b<sub>1</sub></i>  
&nbsp;<i>c<sub>1</sub></i>  
&nbsp;<i>a<sub>2</sub></i>  
&nbsp;<i>b<sub>2</sub></i>  
&nbsp;<i>c<sub>2</sub></i>  
&nbsp;<i>d </i> 
&nbsp;<i>l </i>
<br><br>
outside cylinder 
&nbsp;<i>a<sub>1</sub></i>  
&nbsp;<i>b<sub>1</sub></i>  
&nbsp;<i>c<sub>1</sub></i>  
&nbsp;<i>a<sub>2</sub></i>  
&nbsp;<i>b<sub>2</sub></i>  
&nbsp;<i>c<sub>2</sub></i>  
&nbsp;<i>d </i> 
&nbsp;<i>l </i>
<br><br>

Here, the first three parameters define the point where the cylinder
starts, and <i>l</i> defines the length of the cylinder. <i>d</i> defines de
radius of the cylinder. The simpler example is a cylinder oriented in
the <i>x</i> axis and starting at the origin, such as
<br><br><pre>
inside cylinder 0. 0. 0. 1. 0. 0. 10. 20.
</pre><br><br>
This cylinder is specified by the points that have a distance of 10. to
the <i>x</i> axis (the cylinder has a radius of 10.). Furthermore, it starts at
the origin, therefore no atom restricted by this cylinder will have an
<i>x</i>
coordinate less than 0. Furthermore, it has a length of 20. and, as
such, no atom will have an <i>x</i> coordinate greater than 20. The orientation
of the cylinder, parallel to the <i>x</i> axis is defined by the director
vector (1,0,0), the fourth, fifth and sixth parameters. Cylinders can be
oriented in space in anyway.

<br><br></td></tr>
<!-- END CONSTRAINT -->

<!-- START CONSTRAINT -->
<tr>
<td valign="bottom" align="right" width="20" height="30"> 
10. 
</td>
<td valign="bottom"><tt>
Constrain rotations
</tt></td>
</tr>

<tr><td></td>
<td>
It is possible to constrain rotations of all molecules of each type, so
that they have some average orientation in space.
<br><br>
The keywords to be used are, within a structure...end structure section:
<br><br><pre>
constrain_rotation x 180. 20. 
constrain_rotation y 180. 20. 
constrain_rotation z 180. 20. 
</pre>
<br><br>
Each of these keywords restricts the possible rotation angles around
each axis to be within 180&plusmn;20 degrees (or any other value). For a
technical reason the rotation around the <tt>z</tt> axis will, alone, be
identical to the rotation around the <tt>y</tt> axis (we hope to fix
this some day). Constraining the three rotations will constrain
completely the rotations. Note that to have your molecules oriented
parallel to an axis, you need to constrain the rotations relative to the
other two.
<br><br>
For example, to constrain the rotation of your molecule along the
<tt>z</tt> axis, use something like:
<br><br><pre>
constrain_rotation x 0. 20. 
constrain_rotation y 0. 20. 
</pre><br>
Note that these rotations are defined relative to the molecule in the
orientation which was provided by the user, in the input PDB file.
Therefore, it is a good idea to orient the molecule in a reasonable way
in order to understand the rotations. For example, if the molecule is
elongated in one direction, a good idea is to provide the molecule with
the larger dimension oriented along the <tt>z</tt> axis.<br><br>
<br><br></td></tr>
<!-- END CONSTRAINT -->
</table>
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="pbc"></a><b>Periodic Boundary Conditions </b>
<br><br>

Periodic Boundary Conditions for cubic and rectangular boxes are often
requested by users. We aim to implement that in the future. At the same
time, there is a simple workaround we suggest: If your system will be
simulated in a, for example, 100. Angs box, define your cubic constraints
such that the packing is done in a 98. Angs box. That way, when the actual
simulation box with PBC is built, images will be 2. Angs apart from each
other, as illustrated in the figure below. There will be an empty space
at the boundary, but that will readily disappear with energy
minimization and equilibration.

<center>
<br>
<img src="https://github.com/leandromartinez98/packmol/raw/master/docs//imgs/pbc.jpg">
</center>
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="radii"></a><b>Different radii for different atoms </b>
<br><br>

It is possible (from version 15.133 on) to attribute different radii to
different atoms during the packing procedure. The default behavior is
that all atoms will be distant to each other at the final structure at least
the distance defined by the <code>tolerance</code> parameter. In this
case it is possible to think that the radius of every atom is half the
distance tolerance. A tolerance of 2.0 Angs is usually fine for
simulation of molecular systems using all-atom models.  
<br><br>
Most times, therefore, you won't need this option. This was requested by
users that want to pack multiscale models, in which all-atom and
coarse-grained representations are combined in the same system.
<br><br>
It is easy to define different radii to different molecules, or to atoms
within a molecule. Just add the "<code>radius</code>" keyword within the
<code>structure ... end structure</code> section of a molecule type, or
within <code>atoms ... end atoms</code> section of an atom selection.
<br><br>
For example, in this case:<br><br>
<pre>
tolerance 2.0
structure water.pdb
  number 500
  inside box 0. 0. 0. 30. 30. 30.
  radius 1.5
end structure
</pre>
the radius of the atoms of the water molecules will be 1.5. Note that
this implies a distance tolerance of 3.0 within water molecules.
<br><br>
In this case, on the other side:<br><br>
<pre>
tolerance 2.0
structure water.pdb
  number 500
  inside box 0. 0. 0. 30. 30. 30.
  atoms 1 2
    radius 1.5
  end atoms
end structure
</pre>
only atoms 1 and 2 of the water molecule (as listed in the water.pdb
file) will have a radius of 1.5, while atom 3 will have a radius of 1.0,
as defined by the tolerance of 2.0 
<br><br>
Always remember that the distance tolerance is the sum of the radii of
each pair of atoms, and that the greatest the radii the harder the
packing. Also, keep in mind that your minimization and equilibration
will take care of the actual atom radii, and Packmol is designed only to
give a first coordinate file.  
<br><br>
Finally, currently the restrictions are set to be fulfilled by the
<b>center</b> of the atoms. Therefore, if you are using large radii, you
might want to adjust the sizes of the boxes, spheres, etc., so that the
whole atoms are within the desired regions. For standard all-atom
simulations this is not usually an issue because the radii are small.
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="numb"></a><b>Controlling residue numbering in PDB files.</b>
<br><br>

Since Packmol will create one or more copies of your molecules in a
new PDB file, there are some options on how residue numbers are set to
these new molecules. There are four options, which are set with the
<tt>resnumbers</tt> keyword. This keyword may assume three values, 0, 1,
2 or 3, and may be inserted within the <tt>structure ... end structure</tt>
section of each type of molecule. The options are: <br><br>
<tt>
resnumbers 0 
</tt><br><br>
In this case the residue numbers of all residues will correspond to the
molecule of each type, independently of the residue
numbering of the original pdb file. This means that if you pack 10
water molecules and 10 ethanol molecules, the water molecules will be numbered 
1 to 10, and the ethanol molecules will be numbered 1 to 10. 
<br><br>
<tt>
resnumbers 1
</tt><br><br>
In this case, the residue numbers of the original pdb files are kept
unmodified. This means that if you pack 10 proteins of 5 residues, the
residue numbers will be preserved and, therefore, they will be
repeated for equivalent residues in each molecule of the same protein.
<br><br>
<tt>
resnumbers 2
</tt><br><br>
In this case, the residue numbers of all residues for this structure
will be numbered sequentially according to the number of residues that
are printed previously in the same file. This means that if you pack
10 proteins of 5 residues, there will be residue numbers ranging
from 1 to 50.
<br><br>
<tt>
resnumbers 3
</tt><br><br>
In this case, the numbering of the residues will correspond to the
sequential numbering of all residues in the file. That is, if you
pack a protein with 150 residues followed by 10 water molecules, the
water molecules will be numbered from 151 to 161.
<br><br>
For example, this keyword may be used as in:
<pre><tt>structure peptide.pdb  
  number 10               
  resnumbers 1            
  inside box 0. 0. 0. 20. 20. 20.
end structure        
</tt></pre>
<em>Default:</em> The default behavior is to use 0 for structures with
only one residue and 1 for structures with more than one residue.
<br><br>
<em> Chain identifier: </em><br>
It is also possible to modify the "chain" identifiers of PDB files.
By default, each type of molecule is set to a "chain". On the otherside,
using the <br><br><code>changechains</code><br><br> 
whithin the <code>structure...end structure</code>
section of a type of molecule, the chains will alternate between
two values ("A" and "B" for example). This might be useful if the
molecules are peptides, and topology builders sometimes think
that the peptides of the same chain must be join by covalent bonds.
This is avoided by alternating the chain from molecule to molecule.
<br><br>
Additionally each structure can have a specific chain identifier, set
by the user with the option: <br><br><code>chain D</code><br><br>
where "D" is the desired identifier (Do not use "#"). 
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="restart"></a><b>Building very large systems: using restart files </b>
<br><br>
From version 16.143 on, it is possible to build the system from multiple
and independent executions of Packmol by the use of restart files. In
order to write a restart file, the following keyword must be used:
<tt>
restart_to restart.pack
</tt>
where <tt>restart.pack</tt> is the name of the restart file to be
created. It is possible to write restart files for the whole system, if
the keyword is put outside <tt>structure...end structure</tt> sections,
or to write a restart file for a specific part of the system, using, for
instance:
<pre>
structure water.pdb
  number 1000
  inside cube 0. 0. 0. 40.
  restart_to water1.pack
end structure
</pre>
This will generate a restart file for the water molecules only.
<br><br>

These restart files can be used to start a new execution of Packmol with
more molecules. The <tt>restart_from</tt> keyword must then be used. For
example: 
<pre>
structure water.pdb
  number 1000
  inside cube 0. 0. 0. 40.
  restart_from water1.pack
end structure
</pre>
The new input file might contain other molecules, as a regular Packmol
input file, and these water molecules will be packed together with the
new molecules, but starting from the previous runs. This can be used,
for example, to build solvated bilayers by parts. For instance, the
bilayers could be built and, later, different solvents can be added to
the bilayer, without having to restart the whole system construction
from scratch every time. This could also be used to add some molecule to
the bilayer.
<br><br>
<em>Tip:</em> the restart file can be used to restart the position of a
<em>smaller</em> number of molecules of the same type. 
For instance, if a new molecule is introduced
inside a previous set of molecules (a lipid bilayer, for instance), you
can tell Packmol to pack less molecules of the original set, in order to
provide space for the new structure, while using the original restart
file of more molecules. That is, a <tt> restart_from water1.pack </tt>
similar to the ones of the example above could be used to restart the
positions of 800 molecules.  
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="conv"></a><b>Convergence problems: what to try </b>
<br><br>
Sometimes Packmol is not able to find an adequate packing solution. Here
are some tips to try to overcome these difficulties:<br><br>
<table>
<tr><td>&bull; Look at the best solution obtained, many times it is good enough to
be used. </td></tr>
<tr><td>&bull; Simulate the same problem with only a few molecules of each type. For
example, instead of using 20 thousand water molecules, put 300, and see
if they are in the correct regions. </td></tr>
<tr><td>&bull; If you have large molecules, try running the program twice, one to
pack these molecules, and then use the solution as fixed molecule for
the next packing, in which solvation is included. This may be
particularly useful for building solvated membranes. Build the membrane
first and then use it as a fixed molecule for a solvation run.</td></tr>
<tr><td>&bull; You can change some options of the packing procedure to try
improve the optimization: 
<table width=90% align=center>
<tr><td valign=top>1.</td>
<td><tt>discale [real]</tt><br>
This option controls the distance tolerance actually used in the
local optimization method. It was found that using larger distances
helps sometimes. Try setting <tt>discale</tt> to 1.5, for example.
</td></tr>

<tr><td valign=top>2.</td>
<td><tt>maxit [integer]</tt><br>
This is the maximum number of iterations of the local optimizer (GENCAN)
per loop. The default value is currently 20, changing it may improve (or
worse) the convergence.
</td></tr>

<tr><td valign=top>2.</td>
<td><tt>movebadrandom</tt><br>
 One of the convergence heuristics of Packmol consists in moving
 molecules that are badly placed. If this option is set, the molecules
will be placed in new random position in the box. If not (default), the
molecules are moved to positions nearby molecules that are well packed.
Using this option can help when the restraints are complex, but will
probably be bad if there are large structures, because the new random
position might overlap with those.
</td></tr>
</table>
</td></tr>
</table>
<br><br>
<!-- END SECTION -->

<!-- START SECTION -->
<a name="add"></a><b>Additional input options and keywords</b>
<br><br>

There are some input options which are probably not of interest of the
general user, but may be useful in specific contexts. These keywords may
be added in the input file in any position.<br><br>

Add the TER flag betwen every molecule (AMBER uses this): <br>
Usage: <tt> add_amber_ter </tt> <br><br>

Add box side information to output PDB File (GROMACS uses this): <br>
Usage: <tt> add_box_sides 1.0 </tt> <br>
Where the "1.0" is an optional real number that will be added to
the length of each side, if the actual sides of your simulation box
will not be exactly the same as the maximum and minimum coordinates
of the molecules of the system (usually, if you are using periodic
boundary conditions, you may want to add "1.0" to avoid clashes
at the boundary).
<br><br>

Increase maximum system dimensions: <br>
Usage: <tt> sidemax [real] </tt> (ex: sidemax 2000.d0) <br>
"<tt>sidemax</tt>" is used to build an initial approximation of the molecular
distribution. Increase "<tt>sidemax</tt>" if your system is very large, or your
system may look cut out by this maximum dimension. All system
coordinates must fit within <tt>-sidemax</tt> and <tt>+sidemax</tt>, and
using a <tt>sidemax</tt>
that approximately fits your system may accelerate a little bit the
packing calculation. The default value is 1000.d0. <br><br>

Change random number generator seed: <br>
Usage: <tt> seed [integer] </tt> (ex: seed 191917) <br>
Use <tt> seed -1 </tt> to generate a seed automatically from the computer
time.
<br><br>

Use a truly random initial point for the minimization (the default
option is to generate an homogeneous-density initial): <br>
Usage: <tt> randominitialpoint </tt> <br><br>
Avoid, or not, overlap with fixed molecules at initial point
(avoiding this overlaps is generally recommended, but sometimes
generates gaps that are too large):<br>
Usage: <tt> avoid_overlap yes/no </tt> <br><br>
Change the maximum number of Gencan iterations per loop:<br> Usage: <tt>
maxit [integer] </tt> <br><br>

Change the maximum number of loops:<br>
Usage: <tt> nloop [integer] </tt><br><br>

Change the frequency of output file writing: <br>
Usage: <tt> writeout [integer] </tt><br><br>

Write the current point to output file even if it is worst than the the
best point so far (used for checking purposes only): <br>
Usage: <tt> writebad </tt><br><br>

Check the initial point: This is only for testing purposes,
just build the initial approximation, writes it to the output
file and exits. <br>
Usage: <tt>check</tt><br><br>

Change the optimization subroutine printing output: <br>
Usage: <tt> iprint1 [integer] </tt> and/or <tt> iprint2 [integer]
</tt><br>
where the integer must be 0, 1, 2 or 3.
<br><br>

Change the number of bins of the linked-cell method (technical): <br>
Usage: <tt> fbins [real] </tt> 
</tt><br>
The default value is the square root of three.
<br><br>

Compare analytical and finite-difference gradients: This is only for
testing purposes. Writes chkgrad.log file containing the comparison.<br>
Usage: <tt>chkgrad</tt>
<br><br>
<!-- END SECTION -->

{:/nomarkdown}
