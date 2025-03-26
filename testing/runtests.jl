println("Initalizing enviroment...")
import Pkg; Pkg.activate(@__DIR__); Pkg.instantiate()
using PDBTools
using CellListMap

# 
# Note: this tests depend on each molecule being identified as a single residue
# in the structure. Thus, the PDB file should have a single residue for each molecule,
# for instance, a protein must have a single residue name for all its atoms.
# The residue numbers are overwritten by the `resnumbers` option of the input
# packmol files .
#

struct MinimumDistance 
    i::Int
    j::Int
    d::Float64 
end
function update_mind(i, j, d2, pdb, md::MinimumDistance)
    residue(pdb[i]) == residue(pdb[j]) && return md
    d = sqrt(d2)
    return d < md.d ?  MinimumDistance(i, j, d) : md
end
CellListMap.reducer(md1::MinimumDistance, md2::MinimumDistance) = md1.d < md2.d ? md1 : md2
CellListMap.copy_output(md::MinimumDistance) = md
CellListMap.reset_output(::MinimumDistance) = MinimumDistance(0, 0, +Inf)

function check_mind(input_file::String)
    tolerance = nothing
    output_name = nothing
    unitcell = nothing
    precision = 0.01
    for line in eachline(input_file)
        line = strip(line)
        isempty(line) && continue
        keyword, values... = split(line)
        keyword == "tolerance" && (tolerance = parse(Float64, values[1]))
        keyword == "output" && (output_name = values[1])
        keyword == "precision" && (precision = parse(Float64,values[1]))
        if keyword == "pbc" 
            if length(values) == 3
                unitcell = parse.(Float64,values[1:3])
            elseif length(values) == 6
                unitcell = parse.(Float64,values[4:6]) - parse.(Float64,values[1:3])
            else
                error("pbc not properly set")
            end
        end
    end
    if isnothing(tolerance) || isnothing(output_name)
        error("tolerance or output not found")
    end
    pdb = readPDB(string(output_name))
    sys = ParticleSystem(
        positions = coor.(pdb),
        unitcell = unitcell,
        cutoff = tolerance * 1.5,
        output = MinimumDistance(0, 0, +Inf),
    )
    mind = map_pairwise((x,y,i,j,d2,md) -> update_mind(i, j, d2, pdb, md), sys)
    if (mind.d < (1 - precision) * tolerance)
        error("""\n

            Packing reported success, but minimum distance is not correct for $input_file
            Obtained minimum-distance = $(mind.d) for tolerance $tolerance and precision $precision.
            Atoms: $(mind.i) and $(mind.j) - VMD: index $(mind.i-1) $(mind.j-1)
            
        """)
    end
    printstyled(" OK. \n"; color=:green, bold=true)
    return nothing
end 

println("Running tests...")
if !isinteractive()
    packmol_arg = findfirst(==("-packmol"), ARGS)
    if !isnothing(packmol_arg)
        packmol = ARGS[packmol_arg + 1]
        popat!(ARGS, packmol_arg + 1)
        popat!(ARGS, packmol_arg)
    else
        packmol = joinpath(@__DIR__,"..","packmol")
    end
    println(" Packmol path: $packmol")
    for input_test in ARGS
        print(" Running test $input_test ...")
        log = IOBuffer()
        if packmol != "nothing"
            run(pipeline(`$packmol`; stdin=input_test, stdout=log))
            if occursin("Success!", String(take!(log)))
                check_mind(input_test)
            else
                error("Failed packing for $input_test")
            end
        else
            println("\n Skipping packmol run. Using available output file.")
            check_mind(input_test)
        end
    end
end


