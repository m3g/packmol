# Packmol

Packmol - Creates Initial Configurations for Molecular Dynamics Simulations

**https://m3g.github.io/packmol**

## What is Packmol

Packmol creates an initial point for molecular dynamics simulations by packing molecules in defined regions of space. The packing guarantees that short range repulsive interactions do not disrupt the simulations.

The great variety of types of spatial constraints that can be attributed to the molecules, or atoms within the molecules, makes it easy to create ordered systems, such as lamellar, spherical or tubular lipid layers.

The user must provide only the coordinates of one molecule of each type, the number of molecules of each type and the spatial constraints that each type of molecule must satisfy.

The package is compatible with input files of PDB, TINKER, and XYZ.

## Usage

User guide, examples, and tutorials, are available at: https://m3g.github.io/packmol

## Installation instructions

### Multi-platform package provider with Julia

If you are not familiar with compiling packages, you may find it easier to get the Julia interface for
`packmol`, which provides executables for all common platforms: https://github.com/m3g/Packmol.jl

Installation of the Julia programming language and of the Julia `Packmol` package are necessary, but
these follow simple instructions which are described in the link above.

Compilation of the package, particularly on Linux platforms is, nevertheless, easy, following the instructions
below.

### Downloading

1. Download the `.tar.gz` or `.zip` files of the latest version from: https://github.com/m3g/packmol/releases

2. Unpack the files, for example with: 
   ```bash
   tar -xzvf packmol-21.0.0.tar.gz
   ```
   or
   ```bash
   unzip -xzvf packmol-21.0.0.zip
   ```
   substituting the `21.0.0` with the correct version number.

### Using `make`

3. Go into the `packmol` directory, and compile the package (we assume `gfortran` or other compiler is available):
    ```bash
    cd packmol
    ./configure [optional: path to fortran compiler]
    make
    ```

4. An executable called `packmol` will be created in the main directory. Add that directory to your path.

### Using the Fortran Package Manager (`fpm`)

3. Install the Fortran Package Manager from: https://fpm.fortran-lang.org/en/install/index.html#install

4. Go into the `packmol` directory, and run:
   ```bash
   fpm install --profile release
   ```
   this will compile and send the executable somewhere in your `PATH`.
   By default (on Linux systems) it will be `~/.local/bin`. Making it available
   as a `packmol` command anywhere in your computer.

   `fpm` will look for Fortran compilers automatically and will use `gfortran`
   as default. To use another compiler modify the environment variable
   `FPM_FC=compiler`, for example for `ifort`, use in bash, `export FPM_FC=ifort`.

## References

Please always cite one of the following references in publications for which Packmol was useful:

L Martinez, R Andrade, EG Birgin, JM Martinez, Packmol: A package for building initial configurations for molecular dynamics simulations. Journal of Computational Chemistry, 30, 2157-2164, 2009. (http://www3.interscience.wiley.com/journal/122210103/abstract)

JM Martinez, L Martinez, Packing optimization for the automated generation of complex system's initial configurations for molecular dynamics and docking. Journal of Computational Chemistry, 24, 819-825, 2003.
(http://www3.interscience.wiley.com/journal/104086246/abstract)



