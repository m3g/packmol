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

struct MinimumDistance d::Float64 end
function update_mind(i, j, d2, pdb, md::MinimumDistance)
    residue(pdb[i]) == residue(pdb[j]) && return md
    MinimumDistance(min(sqrt(d2), md.d))
end
CellListMap.reducer(md1::MinimumDistance, md2::MinimumDistance) = MinimumDistance(min(md1.d,md2.d))
CellListMap.copy_output(md::MinimumDistance) = md
CellListMap.reset_output(::MinimumDistance) = MinimumDistance(+Inf)

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
        keyword == "pbc" && (unitcell = (values[1], values[2], values[3]))
        keyword == "precision" && (precision = values[1])
    end
    if isnothing(tolerance) || isnothing(output_name)
        error("tolerance or output not found")
    end
    pdb = readPDB(string(output_name))
    sys = ParticleSystem(
        positions = coor.(pdb),
        unitcell = unitcell,
        cutoff = tolerance * 1.5,
        output = MinimumDistance(+Inf),
    )
    mind = map_pairwise((x,y,i,j,d2,md) -> update_mind(i, j, d2, pdb, md), sys)
    if (mind.d < (1 - precision) * tolerance)
        error("""\n

            Packing reported success, but minimum distance is not correct for $input_file
            Obtained minimum-distance = $(mind.d) for tolerance $tolerance and precision $precision.
            
        """)
    end
    println("Test $input_file OK.")
    return nothing
end 

if !isinteractive()
    packmol = joinpath(@__DIR__,"..","packmol")
    for input_test in ARGS
        log = IOBuffer()
        run(pipeline(`$packmol`; stdin=input_test, stdout=log))
        if occursin("Success!", String(take!(log)))
            check_mind(input_test)
        else
            error("Failed packing for $input_test")
        end
    end
end


