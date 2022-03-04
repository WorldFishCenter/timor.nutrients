---
title: "Modelling scenarios for nutrient-sensitive fisheries management"
date: "Last compiled on 2022-03-04 19:07:02"
mainfont: Montserrat
geometry: "left=3cm,right=3cm,top=2cm,bottom=2cm"
output:
  bookdown::pdf_book:
    toc: yes
    toc_depth: 2
    number_sections: true
  bookdown::epub_book:
    number_sections: true
    toc: true
  bookdown::gitbook:
    lib_dir: assets
    split_by: section
    config:
      toolbar:
        position: static
      download: ["plots_report.pdf", "plots_report.epub"]
header-includes: 
  - \usepackage{float} 
  - \floatplacement{figure}{H}
  - \usepackage{leading}
  - \leading{16pt}
  - \definecolor{myblue}{RGB}{68,117,151}
  - \let\counterwithout\relax
  - \let\counterwithin\relax
  - \usepackage{chngcntr}
---





# Data pipeline

<img src="/Users/lore/My Drive/WorldFish/Timor.nutrients/inst/pipeline.png" width="2232" height="80px" style="display: block; margin: auto auto auto 0;" />

# Data summary
## Regions' temporal coverage

<img src="/Users/lore/My Drive/WorldFish/Timor.nutrients/inst/plots_report_files/figure-html/unnamed-chunk-3-1.png" width="576" />

Atauro, Bobonaro, Bacau and Covalima are the most complete. I'll filter the following analyses on these 4 municipalities and consider the period Jun 2019 - Feb 2022.

# Nutrients seasonality

<img src="/Users/lore/My Drive/WorldFish/Timor.nutrients/inst/plots_report_files/figure-html/unnamed-chunk-4-1.png" width="576" />

There is something strange with Baucau values. That something to focus on. At the moment leave it apart and continue with other municipalities

<img src="/Users/lore/My Drive/WorldFish/Timor.nutrients/inst/plots_report_files/figure-html/unnamed-chunk-5-1.png" width="576" />

\pagebreak

# Catch composition

<img src="/Users/lore/My Drive/WorldFish/Timor.nutrients/inst/plots_report_files/figure-html/unnamed-chunk-6-1.png" width="1056" />
