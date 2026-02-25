# iaw -- Ivo's Analytical Workbench

R package providing ~145 utility functions for data manipulation,
regression analysis, plotting, and parallel computing.

All functions live in the `iaw` environment and are called as
`iaw$func()`.

## Installation

```r
# install.packages("remotes")  # if needed
remotes::install_github("iwelch/Riaw")
```

Then load it:

```r
library(iaw)
```

### Dependencies

Required packages (`parallel`, `data.table`, `stats`, `utils`,
`grDevices`, `graphics`, `tools`) are installed automatically.

Optional packages that enable additional features:

```r
install.packages(c("Cairo", "sandwich", "lmtest", "knitr",
                   "MASS", "dplyr", "fst", "glue", "bit64"))
```

### Installing from a local clone

```bash
git clone git@github.com:iwelch/Riaw.git
cd Riaw
make install
```

This copies source files to `inst/lib/`, generates man pages, and
runs `R CMD INSTALL`.

## Usage

```r
library(iaw)

# regression with enhanced output
fit <- iaw$olm(mpg ~ wt + hp, data = mtcars)
print(fit)

# population covariance
iaw$covp(mtcars$wt, mtcars$mpg)

# parallel grouped computation
iaw$rbind.mc.by(mtcars, mtcars$cyl, function(d) {
  data.frame(cyl = d$cyl[1], mean_mpg = mean(d$mpg))
})
```

Help is available for every function:

```r
?olm
?covp
?rbind.mc.by
```

## License

GPL (>= 3)
