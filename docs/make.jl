using Scanf
using Documenter

DocMeta.setdocmeta!(Scanf, :DocTestSetup, :(using Scanf); recursive=true)

makedocs(;
    modules=[Scanf],
    authors="KlausC <klausC@users.noreply.github.com> and contributors",
    repo="https://github.com/KlausC/Scanf.jl/blob/{commit}{path}#{line}",
    sitename="Scanf.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://KlausC.github.io/Scanf.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/KlausC/Scanf.jl",
)
