# smg
Simple R wrapper for the webtool [Swiss Maps Generator](https://swiss-maps.vercel.app/). 

## Installation
For regularly updated version (latest: 0.1.0), install from GitHub:
```r
install.packages("devtools")
devtools::install_github("zumbov2/smg")
```

## Example
Obtain geodata on national, cantonal and municipal boundaries (and lakes) from 2019.

``` r
smg::get_shapes(year = 2019)
#> $country
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.956801 ymin: 45.81915 xmax: 10.49345 ymax: 47.80987
#> CRS:           NA
#>   id                       geometry
#> 1 CH POLYGON ((8.618794 47.78162...
#> 
#> $cantons
#> Simple feature collection with 26 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.956801 ymin: 45.81915 xmax: 10.49345 ymax: 47.80987
#> CRS:           NA
#> First 10 features:
#>    id                       geometry
#> 1   1 MULTIPOLYGON (((8.427539 47...
#> 2   2 MULTIPOLYGON (((7.092139 46...
#> 3   3 MULTIPOLYGON (((7.838452 47...
#> 4   4 MULTIPOLYGON (((8.45493 46....
#> 5   5 MULTIPOLYGON (((8.715688 47...
#> 6   6 MULTIPOLYGON (((8.450104 46...
#> 7   7 MULTIPOLYGON (((8.396287 46...
#> 8   8 MULTIPOLYGON (((8.872311 46...
#> 9   9 MULTIPOLYGON (((8.666939 47...
#> 10 10 MULTIPOLYGON (((6.773945 46...
#> 
#> $municipalities
#> Simple feature collection with 2215 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.956801 ymin: 45.81915 xmax: 10.49345 ymax: 47.80987
#> CRS:           NA
#> First 10 features:
#>      id                       geometry
#> 1     1 MULTIPOLYGON (((8.473272 47...
#> 2    10 MULTIPOLYGON (((8.415352 47...
#> 3   100 MULTIPOLYGON (((8.462532 47...
#> 4  1001 MULTIPOLYGON (((8.060476 46...
#> 5  1002 MULTIPOLYGON (((8.023794 46...
#> 6  1004 MULTIPOLYGON (((7.95405 46....
#> 7  1005 MULTIPOLYGON (((8.086902 46...
#> 8  1007 MULTIPOLYGON (((7.953446 46...
#> 9  1008 MULTIPOLYGON (((8.010641 46...
#> 10 1009 MULTIPOLYGON (((8.129738 47...
#> 
#> $lakes
#> Simple feature collection with 22 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 6.143711 ymin: 45.72351 xmax: 9.752322 ymax: 47.81829
#> CRS:           NA
#> First 10 features:
#>      id                       geometry
#> 1  9040 MULTIPOLYGON (((8.658734 47...
#> 2  9050 MULTIPOLYGON (((8.805825 47...
#> 3  9073 MULTIPOLYGON (((7.746506 46...
#> 4  9089 MULTIPOLYGON (((7.939932 46...
#> 5  9148 MULTIPOLYGON (((7.217148 47...
#> 6  9151 MULTIPOLYGON (((6.935877 46...
#> 7  9157 MULTIPOLYGON (((8.258488 47...
#> 8  9163 MULTIPOLYGON (((8.1447 47.1...
#> 9  9172 MULTIPOLYGON (((8.229287 47...
#> 10 9175 MULTIPOLYGON (((8.478943 47...
```