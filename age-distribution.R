library(data.table)
library(xml2)
library(rvest)

# get data for Canterbury North / Ashbury
census_2016_CNA <- read_html("http://www.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/120031392?opendocument")
# extract all tables
CNA_Tables_2016 <- xml_find_all(census_2016_CNA, "//table")
# convert to data.frames
cna_tables_2016 <- lapply(lapply(CNA_Tables_2016, html_table),setDT)

## get Data for Croydon Park / Enfield

census_2016_cpe <- read_html("http://www.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/120031393?opendocument")
# extract all tables
CPE_Tables_2016 <- xml_find_all(census_2016_cpe, "//table")
# convert to data.frames
cpe_tables_2016 <- lapply(lapply(CPE_Tables_2016, html_table),setDT)

# get data for Canterbury North / Ashbury
census_2011_CNA <- read_html("http://www.censusdata.abs.gov.au/census_services/getproduct/census/2011/quickstat/120031392?opendocument")
# extract all tables
CNA_Tables_2011 <- xml_find_all(census_2011_CNA, "//table")
# convert to data.frames
cna_tables_2011 <- lapply(lapply(CNA_Tables_2011, html_table,fill=TRUE),setDT)

## get Data for Croydon Park / Enfield

census_2011_cpe <- read_html("http://www.censusdata.abs.gov.au/census_services/getproduct/census/2011/quickstat/120031393?opendocument")
# extract all tables
CPE_Tables_2011 <- xml_find_all(census_2011_cpe, "//table")
# convert to data.frames
cpe_tables_2011 <- lapply(lapply(CPE_Tables_2011, html_table,fill=TRUE),setDT)

`%nin%` <- Negate("%in%")

Age <- list(
  "2016" = list("Croydon Park - Enfield" = cpe_tables_2016[[5]][,1:2], "Canterbury (North) - Ashbury" = cna_tables_2016[[5]][,1:2]),
  "2011" = list("Croydon Park - Enfield" = cpe_tables_2011[[6]][,1:2], "Canterbury (North) - Ashbury" = cna_tables_2011[[6]][,1:2])
  )

AgeDT <- rbindlist(lapply(Age, rbindlist,id="SA2"),id="year")
setnames(AgeDT, c("year","SA2","Age","N"))

AgeDT <- AgeDT[Age %nin% c("Median age","People","")][, N := as.numeric(gsub(",","",N))]
AgeDT[, Age  := gsub("( years)|( years and over)","",Age)][, Age := gsub("85","85+",Age)]
AgeDT[, Age := factor(Age, levels = unique(Age))][,P := N / sum(N),by = list(year, SA2)]
deltaAge <- AgeDT[,list(delta = N[year=='2016']-N[year=='2011']),by=list(SA2,Age)]


ggplot(AgeDT, aes(x = Age, y = P)) +
  geom_bar(stat = 'identity') + coord_flip() +
  facet_grid(year~SA2) +
  scale_y_continuous(label=scales::percent, name = "Population")

ggplot(deltaAge,aes(x = Age, y = delta,fill = delta>0)) + 
  geom_bar(stat = 'identity') + facet_wrap(~SA2) + 
  coord_flip() + scale_fill_brewer(guide="none",palette="Set1")

