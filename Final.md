Final project
================
Data Sci Doggos
11/17/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
dogz = read.csv("./data/NYC_Dog_Licensing_Dataset.csv") %>% 
  janitor::clean_names() %>% 
  mutate(animal_name = str_to_sentence(animal_name),
         animal_birth_month = as.numeric(ifelse(extract_year-animal_birth_month > 20, "", animal_birth_month)),
         borough = ifelse(zip_code %in% c(10001:10282),"Manhattan", borough),
         borough = ifelse(zip_code %in% c(10301:10314), "Staten Island", borough),
         borough = ifelse(zip_code %in% c(10451:10475), "Bronx", borough),
         borough = ifelse(zip_code %in% c(11004,11101:11106,11109,11351,11354:11375,11377:11379,11385,11411:11423,11426:11430,11432:11436,11691:11694,11697), "Queens", borough),
         borough = ifelse(zip_code %in% c(11201,11203:11226,11228:11239,11241:11243,11249,11252,11256), "Brooklyn", borough)) %>% 
  rename(animal_birth_year = animal_birth_month, license_id = row_number)
```
