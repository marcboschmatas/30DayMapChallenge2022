# LISA PLOT FOR INCOME IN BARCELONA BY CENSUS TRACTS
#= # install libraries
using Pkg
Pkg.add("GeoInterface")
Pkg.add("SpatialDependence")
Pkg.add("Shapefile") =#

# load libraries
using DataFrames
using CSV
using SpatialDependence
using StableRNGs
using Plots
using Shapefile


# read income data for census tracts
# downloaded from here - https://www.ine.es/experimental/atlas/exp_atlas_tab.htm (Indicadores de renta media y mediana)


income = CSV.read("/home/marc/Documents/morans/30824.csv", DataFrame, delim = "\t", decimal = ',')

 # filter for household netto income in 2019 by census tracts in Barcelona
 

 income = rename(income,Dict("Indicadores de renta media" => "Indicadores"))

income = dropmissing(income, :Secciones)



income = filter([:Indicadores, :Periodo, :Municipios] => (x,y,z) -> x == "Renta neta media por hogar" && y == "2019" && contains(z, "Barcelona"), income)


# create column with census tract code only


income.CUSEC = SubString.(string.(income.Secciones), 1, 10)



# transform Total to numeric (remove thousand separator)

income.Total = parse.(Float64,income.Total)*1000


# read census tracts - downloaded from here https://opendata-ajuntament.barcelona.cat/data/ca/dataset/20170706-districtes-barris/resource/cd800462-f326-429f-a67a-c69b7fc4c50a
# messes up the geometry, shouldn't be happening at all
seccdf = Shapefile.Table("/home/marc/Documents/30dayMapChallenge/data/seccens/0301040100_SecCens_UNITATS_ADM.shp")

seccdf = DataFrame(seccdf)
# keep only census tracts
seccdf = filter(:SEC_CENS => !=("-"),seccdf)

# compute census tract code
seccdf.CUSEC = "08019".*seccdf.DISTRICTE.*seccdf.SEC_CENS

# join income value to census tracts layer

income = rename(income, Dict(:Total => :household_income))
seccdf = innerjoin(select(income,:CUSEC,:household_income),
    seccdf,
    on = :CUSEC)



# Build polygon contiguity matrix
W = SpatialDependence.polyneigh(seccdf.geometry)


# sanity check - Moran's Global I
moran(seccdf.household_income, W, permutations = 9999, rng = StableRNG(1234567))

# local moran
lisas = localmoran(seccdf.household_income, W, permutations = 9999, rng = StableRNG(1234567))

plot(seccdf, lisas, sig = 0.05, adjust = :none,
    title = "Day 29: Out of your confort Zone\nLISA map with julia")

