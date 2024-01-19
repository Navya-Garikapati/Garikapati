## Week 3: Exercise - 1
install.packages("tidyverse")
install.packages("dplyr")
install.packages("stats")

library(tidyverse) 
library(dplyr) 
library(stats)

adsl <- data.frame(
  stringsAsFactors = FALSE,
  SUBJIDN = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
  SEX = c("M", "F", NA , "F", "M", "M", "F", "M", "F", "M"),
  SEXN = c(1, 0, NA , 0, 1, 1, 0, 1, 0, 1),
  AGE = c(57, NA, 68, 50, 70, 36, 30, 52, 57, 19),
  CNTY = c("CHINA","INDIA","USA","USA",
           "CHINA","CHINA","CHINA","AUS","USA",NA),
  CNTYN = c("1","2","3","3",
            "1","1","1",NA,"3",NA),
  TRTP = c("DRUG","DRUG","PLACEBO","DRUG",
           "PLACEBO",NA,"PLACEBO","PLACEBO","PLACEBO",
           "PLACEBO"),
  TRTPN = c(1, 1, 1, NA, 0, 0, 0, 0, 0, 0),
  TRTSDT = c("2019-08-18","2022-07-04",
             "2022-07-30","2022-03-31","2022-01-25","2022-08-13",
             "2022-06-10","2022-05-20","2022-02-22","2022-01-19"),
  SAFFN = c(0L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L)
)

#Q: In the dataset `adsl`, can you identify the country that has missing
#decode in `CNTYN` ?
  
  cntym <- adsl %>%
    filter(is.na(CNTYN)) %>%
    select( CNTY)
  
  ## Week 3: Exercise - 2 
#step 1> Create a dataset `big_n` from `adsl` and derive a new variable `"N"` 
#with count of subjects in each treatment, include only `safety population`.
  
 big_n <- adsl %>%
   filter(SAFFN== 1) %>%
   group_by(TRTP) %>% 
   ## mutate(N= count(SUBJIDN))
 summarize(N=n())  %>% 
   mutate(PCT=round(N/sum(N)*100,2),N_PCT=paste0(N,"(",PCT,"%)"))
 
 #big_n <- adsl %>% filter(SAFFN==1) %>% group_by(TRTP) %>% summarise(N=n()) %>% 
  # mutate(PCT=round(N/sum(N)*100,2),N_PCT=paste0(N,"(",PCT,"%)"))
 
 ## Week 3: Exercise - 3
 
#step 1> Create a dataset `desc_age_trt` from `adsl` with descriptive statistics - 
 #`n,Min,Max, Mean, Median, and SD` for `AGE` by `Treatment`, include `safety population` only. 
#Note: Each statistic will be a new column. Try to round the decimals of each statistic as per our Global standard tables
 
 desc_age_trt <- adsl  %>%
   #select(TRTP, AGE, SAFFN) %>%
   filter(SAFFN== 1) %>%
   filter(!is.na(AGE)) %>%
   group_by(TRTP)  %>%
   summarize(Min = min(AGE),
             Max = max(AGE),
             n   = n(),
             Mean=mean(AGE),
             Median = median(AGE),
             SD  = sd(AGE)) %>%
#step 2> Create new variable `Var` with value "Age"
   mutate(Var = "Age") %>%
#step 3> Move the column Var to the first position in dataset (use `select()`)
   select(Var,everything())
 


 
 
 


 
 
 
 