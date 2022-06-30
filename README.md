# The `certeplot2` package for R

This is a Certe R Package for fast and convenient plotting, by providing wrappers around 'tidyverse' packages such as 'ggplot2', while also providing plotting in the Certe organisational style. This package is part of the 'certedata' universe.


You can read the [manual with explanation about its functions here](https://certe-medical-epidemiology.github.io/certeplot2/reference).


## About the 'certedata' universe

<div style="position: relative; height: 410px;">
  <a href="https://certe-medical-epidemiology.github.io/certedata/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certedata/logo.svg" alt="The certedata R package" style="position: absolute; left: 0px; top: 0px; width: 136px; height: 156px;">
  </a>
  <a href="https://certe-medical-epidemiology.github.io/certestyle/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certestyle/logo.svg" alt="The certestyle R package" style="position: absolute; left: 135px; top: 0px; width: 136px; height: 156px;">
  </a>
  <a href="https://certe-medical-epidemiology.github.io/certeapi/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certeapi/logo.svg" alt="The certeapi R package" style="position: absolute; left: 270px; top: 0px; width: 136px; height: 156px;">
  </a>

  <a href="https://certe-medical-epidemiology.github.io/certeplot2/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certeplot2/logo.svg" alt="The certeplot2 R package" style="position: absolute; left: 67.5px; top: 122px; width: 136px; height: 156px;">
  </a>
  <a href="https://certe-medical-epidemiology.github.io/certedb/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certedb/logo.svg" alt="The certedb R package" style="position: absolute; left: 202.5px; top: 122px; width: 136px; height: 156px;">
  </a>
  <a href="https://certe-medical-epidemiology.github.io/certeprojects/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certeprojects/logo.svg" alt="The certeprojects R package" style="position: absolute; left: 337.5px; top: 122px; width: 136px; height: 156px;">
  </a>

  <a href="https://certe-medical-epidemiology.github.io/certemail/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certemail/logo.svg" alt="The certemail R package" style="position: absolute; left: 0px; top: 244px; width: 136px; height: 156px;">
  </a>
  <a href="https://certe-medical-epidemiology.github.io/certestats/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certestats/logo.svg" alt="The certestats R package" style="position: absolute; left: 135px; top: 244px; width: 136px; height: 156px;">
  </a>
  <a href="https://certe-medical-epidemiology.github.io/certegis/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certegis/logo.svg" alt="The certegis R package" style="position: absolute; left: 270px; top: 244px; width: 136px; height: 156px;">
  </a>
  <a href="https://certe-medical-epidemiology.github.io/certetoolbox/" target="_blank">
    <img src="https://certe-medical-epidemiology.github.io/certetoolbox/logo.svg" alt="The certetoolbox R package" style="position: absolute; left: 405px; top: 244px; width: 136px; height: 156px;">
  </a>
</div>

These are R packages developed by [**Certe**](https://www.certe.nl), a non-profit organisation for integrated medical diagnostics and advice for primary and secondary healthcare. Certe provides high-quality medical diagnostics and related products and services for healthcare providers and their patients. Hospitals, (general) practitioners, midwives and thus hundreds of thousands of people have relied on Certe's care for many years.

Their department of Medical Epidemiology, which developed these R packages, conducts (and develops new methods for) medical data analyses, for both routine workflows and scientific research. For all our packages, please visit [our GitHub organisation overview](https://github.com/certe-medical-epidemiology). Our R packages are not on CRAN since their use is primarily intended for own staff, but they are publicly available to support open science.

### Install

All our R packages are published [here at R-universe](https://certe-medical-epidemiology.r-universe.dev), allowing anyone to install and update the packages using common methods, such as the RStudio menu bar or `install.packages()`. To add the Certe Medical Epidemiology R-universe to your existing repositories, run:

```r
options(repos = c(
  CerteMedEpi = "https://certe-medical-epidemiology.r-universe.dev",
  options()$repos)
```

You can now install any Certe R package, e.g.:

```r
# our 'loader package' certedata installs all Certe R packages this way:
install.packages("certedata", dependencies = TRUE)

# or install a specific package:
install.packages("certegis")
install.packages("certeplot2")
install.packages("certestats")
```

### Developer status

| Certe R Package | Latest release | Daily tests | Code coverage | Code compliance | Changelog |
|:----------------|:--------------:|:-----------:|:-------------:|:---------------:|:---------:|
| <img src="https://certe-medical-epidemiology.github.io/certeapi/logo.svg" style="width: 20x; height: 23px;"> [`certeapi`](https://certe-medical-epidemiology.github.io/certeapi) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certeapi?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certeapi/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certeapi/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certeapi/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certeapi) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certeapi/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certeapi/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certeapi/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certeapi/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certedata/logo.svg" style="width: 20x; height: 23px;"> [`certedata`](https://certe-medical-epidemiology.github.io/certedata) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certedata?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certedata/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certedata/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certedata/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certedata) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certedata/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certedata/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certedata/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certedata/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certedb/logo.svg" style="width: 20x; height: 23px;"> [`certedb`](https://certe-medical-epidemiology.github.io/certedb) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certedb?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certedb/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certedb/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certedb/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certedb) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certedb/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certedb/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certedb/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certedb/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certegis/logo.svg" style="width: 20x; height: 23px;"> [`certegis`](https://certe-medical-epidemiology.github.io/certegis) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certegis?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certegis/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certegis/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certegis/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certegis) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certegis/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certegis/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certegis/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certegis/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certemail/logo.svg" style="width: 20x; height: 23px;"> [`certemail`](https://certe-medical-epidemiology.github.io/certemail) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certemail?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certemail/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certemail/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certemail/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certemail) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certemail/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certemail/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certemail/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certemail/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certeplot2/logo.svg" style="width: 20x; height: 23px;"> [`certeplot2`](https://certe-medical-epidemiology.github.io/certeplot2) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certeplot2?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certeplot2/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certeplot2/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certeplot2/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certeplot2) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certeplot2/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certeplot2/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certeplot2/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certeplot2/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certeprojects/logo.svg" style="width: 20x; height: 23px;"> [`certeprojects`](https://certe-medical-epidemiology.github.io/certeprojects) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certeprojects?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certeprojects/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certeprojects/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certeprojects/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certeprojects) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certeprojects/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certeprojects/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certeprojects/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certeprojects/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certestats/logo.svg" style="width: 20x; height: 23px;"> [`certestats`](https://certe-medical-epidemiology.github.io/certestats) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certestats?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certestats/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certestats/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certestats/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certestats) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certestats/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certestats/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certestats/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certestats/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certestyle/logo.svg" style="width: 20x; height: 23px;"> [`certestyle`](https://certe-medical-epidemiology.github.io/certestyle) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certestyle?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certestyle/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certestyle/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certestyle/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certestyle) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certestyle/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certestyle/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certestyle/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certestyle/commits/main) |
| <img src="https://certe-medical-epidemiology.github.io/certetoolbox/logo.svg" style="width: 20x; height: 23px;"> [`certetoolbox`](https://certe-medical-epidemiology.github.io/certetoolbox) | [![R-universe](https://certe-medical-epidemiology.r-universe.dev/badges/certetoolbox?color=22bb55)](https://certe-medical-epidemiology.r-universe.dev/ui#builds) | [![R-CMD-check](https://github.com/certe-medical-epidemiology/certetoolbox/actions/workflows/workflow-schedule.yml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certetoolbox/actions/workflows/workflow-schedule.yml) | [![codecov](https://codecov.io/gh/certe-medical-epidemiology/certetoolbox/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certetoolbox) |  [![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certetoolbox/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certetoolbox/) | [![commits](https://badgen.net/github/commits/certe-medical-epidemiology/certetoolbox/main?icon=github&color=green)](https://github.com/certe-medical-epidemiology/certetoolbox/commits/main) |

### Copyright

We believe open science matters, which is why these R packages are publicly available on GitHub.

They are free, open-source software and are all licensed under the [GNU General Public License v2.0 (GPL-2)](./LICENSE.md). In a nutshell, this means that our R packages:

- May be used for commercial purposes
- May be used for private purposes
- May **not** be used for patent purposes
- May be modified, although:
  - Modifications **must** be released under the same license when distributing the package
  - Changes made to the code **must** be documented
- May be distributed, although:
  - Source code **must** be made available when the package is distributed
  - A copy of the license and copyright notice **must** be included with the package.
- Come with a LIMITATION of liability
- Come with NO warranty

----

© Certe Medical Diagnostics & Advice Foundation - [www.certe.nl](https://www.certe.nl). Certe is the sole copyright holder and funder of this software.
