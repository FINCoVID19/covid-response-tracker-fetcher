# covid-response-tracker-fetcher
R package to fetch data from https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker


### Installation

To install package, just run:

```
remotes::install_github("FINCoVID19/covid-response-tracker-fetcher")
```


### Usage

To access data and store it as a csv, for countries

```
librarary(covidresponser)

countries <- c("DNK", "ITA", "DEU", "ESP", "GBR", "FRA", "NOR", "BEL", "AUT", "SWE", "CHE", "GRC", "PRT", "NLD", "FIN")

covidtracker_create_actions_csv(file = "interventions_oxford.csv", 
                                countries, 
                                from = "2020-03-15", 
                                to = "2020-03-16")
```

We can then easily update the csv as


```
librarary(covidresponser)

countries <- c("DNK", "ITA", "DEU", "ESP", "GBR", "FRA", "NOR", "BEL", "AUT", "SWE", "CHE", "GRC", "PRT", "NLD", "FIN")

covidtracker_update_actions_csv(file = "interventions_oxford.csv", 
                                to = "2020-03-18")
```
