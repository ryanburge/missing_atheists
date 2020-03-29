library(fst)
library(socsci)
library(car)
source("D://theme.R")

## CCES Data
cces06 <- read.fst("C://cces06.fst")
cces08 <- read.fst("C://cces08.fst") 
cces10 <- read.fst("C://cces10.fst")
cces12 <- read.fst("C://cces12.fst")
cces14 <- read.fst("C://cces14.fst")
cces16 <- read.fst("C://cces16.fst")
cces17 <- read.fst("C://cces17.fst")
cces18 <- read.fst("C://cces18.fst")

cces <- read.fst("C://new_cces.fst")

gss <- read.fst("C://gss18b.fst")


source("D://reltrad/CCES/reltrad08.R")
source("D://reltrad/CCES/reltrad10.R")
source("D://reltrad/CCES/reltrad12.R")
source("D://reltrad/CCES/reltrad14.R")
source("D://reltrad/CCES/reltrad16.R")
source("D://reltrad/CCES/reltrad17.R")
source("D://reltrad/CCES/reltrad18.R")
source("D://reltrad/CCES/reltrad_series.R")


source("D://measuring_evangelicals/making_baprot.R")

g1 <- cces18 %>% select(caseid, evangelical, mainline, bprot, catholic, jewish, other, none) %>% 
  gather(reltrad, x1, evangelical:none) %>% 
  filter(x1 ==1) %>% select(caseid, reltrad)

cces18 <- cces18 %>% left_join(g1)
 
library(haven)
cces18 <- read_dta("D://cces18.dta")

cces08 <- read_dta("D://cces/data/cces08.dta")

cces <- read.fst("C://cces_full.fst")

