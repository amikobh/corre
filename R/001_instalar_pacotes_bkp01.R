# 1 PROCEDIMENTOS INICIAIS : LIMPAR OBJETOS
rm(list=ls(all=TRUE))
#########################################################################################################
setwd(file.path("~/corre"))#configurar diretorio
#########################################################################################################
if(!require(conflicted)){install.packages("conflicted", dependencies = TRUE); require(conflicted)}
if(!require(Rcpp)){install.packages("Rcpp", dependencies = TRUE); require(Rcpp)}
if(!require(tidyverse)){install.packages("tidyverse", dependencies = TRUE); require(tidyverse)}
if(!require(readODS)){install.packages("readODS", dependencies = TRUE); require(readODS)}
if(!require(data.table)){install.packages("data.table", dependencies = TRUE); require(data.table)}
if(!require(plyr)){install.packages("plyr", dependencies = TRUE); require(plyr)}
if(!require(abjutils)){install.packages("abjutils", dependencies = TRUE); require(abjutils)}
if(!require(ggplot2)){install.packages("ggplot2", dependencies = TRUE); require(ggplot2)}
if(!require(gt)){install.packages("gt", dependencies = TRUE); require(gt)}
if(!require(ggpubr)){install.packages("ggpubr", dependencies = TRUE); require(ggpubr)}
if(!require(ggrepel)){install.packages("ggrepel", dependencies = TRUE); require(ggrepel)}
if(!require(cowplot)){install.packages("cowplot", dependencies = TRUE); require(cowplot)}
if(!require(bookdown)){install.packages("bookdown", dependencies = TRUE); require(bookdown)}
if(!require(XML)){install.packages("XML", dependencies = TRUE); require(XML)}
if(!require(ggmap)){install.packages("ggmap", dependencies = TRUE); require(ggmap)}
if(!require(googleway)){install.packages("googleway", dependencies = TRUE); require(googleway)}
if(!require(tidygeocoder)){install.packages("tidygeocoder", dependencies = TRUE); require(tidygeocoder)}
if(!require(mondate)){install.packages("mondate", dependencies = TRUE); require(mondate)}
if(!require(janitor)){install.packages("janitor", dependencies = TRUE); require(janitor)}
if(!require(combinat)){install.packages("combinat", dependencies = TRUE); require(combinat)}
if(!require(hrbrthemes)){install.packages("hrbrthemes", dependencies = TRUE); require(hrbrthemes)}
if(!require(gtExtras)){install.packages("gtExtras", dependencies = TRUE); require(gtExtras)}
if(!require(pdftools)){install.packages("pdftools", dependencies = TRUE); require(pdftools)}
if(!require(tm)){install.packages("tm", dependencies = TRUE); require(tm)}
if(!require(kableExtra)){install.packages("kableExtra", dependencies = TRUE); require(kableExtra)}
if(!require(openxlsx)){install.packages("openxlsx", dependencies = TRUE); require(openxlsx)}
if(!require(arsenal)){install.packages("arsenal", dependencies = TRUE); require(arsenal)}
if(!require(stringr)){install.packages("stringr", dependencies = TRUE); require(stringr)}



conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(tidygeocoder::geocode)
conflicts_prefer(dplyr::desc)
conflicts_prefer(combinat::combn)
conflicts_prefer(dplyr::count)
conflicts_prefer(dplyr::rename)

##if(!require(ggplot)){install.packages("ggplot", dependencies = TRUE); require(ggplot)}
#if(!require(lubridate)){install.packages("lubridate", dependencies = TRUE); require(lubridate)}
#if(!require(ggrepel)){install.packages("ggrepel", dependencies = TRUE); require(ggrepel)}
#if(!require(abjutils)){install.packages("abjutils", dependencies = TRUE); require(abjutils)}
#if(!require(flextable)){install.packages("flextable", dependencies = TRUE); require(flextable)}
#if(!require(data.table)){install.packages("data.table", dependencies = TRUE); require(data.table)}

#if(!require(RCurl)){install.packages("RCurl", dependencies = TRUE); require(RCurl)}

#if(!require(tinytex)){install.packages("tinytex", dependencies = TRUE); require(tinytex)}
#if(!require(bookdown)){install.packages("bookdown", dependencies = TRUE); require(bookdown)}
#if(!require(curl)){install.packages("curl", dependencies = TRUE); require(curl)}
#if(!require(mondate)){install.packages("mondate", dependencies = TRUE); require(mondate)}
#if(!require(abjutils)){install.packages("abjutils", dependencies = TRUE); require(abjutils)}
#if(!require(webshot)){install.packages("webshot", dependencies = TRUE); require(webshot)}
#if(!require(webshot2)){install.packages("webshot2", dependencies = TRUE); require(webshot2)}
#if(!require(pacman)){install.packages("pacman", dependencies = TRUE); require(pacman)}

#if(!require(markdown)){install.packages("markdown", dependencies = TRUE); require(markdown)}
#if(!require(stargazer)){install.packages("stargazer", dependencies = TRUE); require(stargazer)}
#if(!require(cowplot)){install.packages("cowplot", dependencies = TRUE); require(cowplot)}
#if(!require(gridExtra)){install.packages("gridExtra", dependencies = TRUE); require(gridExtra)}
#if(!require(gt)){install.packages("gt", dependencies = TRUE); require(gt)}
#if(!require(kableExtra)){install.packages("kableExtra", dependencies = TRUE); require(kableExtra)}
#if(!require(rlang)){install.packages("rlang", dependencies = TRUE); require(rlang)}
#if(!require(janitor)){install.packages("janitor", dependencies = TRUE); require(janitor)}

setwd(file.path("~/corre"))#configurar diretorio

#########################################################################################################
#########################################################################################################
#FIM
#########################################################################################################
#TESTE
