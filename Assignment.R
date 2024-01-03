install.packages("tidyverse")
install.packages("dplyr")
install.packages("stats")

library(tidyverse) 
library(dplyr) 
library(stats)

#What are some of the benefits of using pipe operator(%>%)?
#chain multiple operations together 
#allows us to link a sequence of analysis steps.

#create adsl data frame

adsl <- data.frame(SUBJIDN  = 1:200,
                   SEX      = sample(c('M','F', NA), 200, replace = T, prob = c(.8,.15,.05)),
                   AGE      = runif(200, min=18, max=65) %>% round(),
                   TRTP     = sample(c('DRUG','PLACEBO'), 200, replace = T),
                   SAFFL    = sample(c('Y','N', NA), 200, replace = T, prob = c(.9,.07,.03)),
                   COUNTRY  = sample(c('USA','INDIA','EU','CHINA'), 200, replace = T),
                   stringsAsFactors = F)

data2 <- adsl
?sample
?runif


#select variables which start with S or end with Y
adsl %>% 
  select(starts_with("S") | ends_with("Y"))
  
#create new dataframe and Select only numeric variables
adsl_n <- adsl %>%
  select(where(is.numeric))

#Compute individual frequencies for the COUNTRY and SEX variables.

adsl %>%
  count(COUNTRY)

adsl %>%
  count(SEX)

#cross-tabulation for COUNTRY and SEX while remove where COUNTRY or SEX are NA

adsl_counts <- adsl %>%
  count(COUNTRY , SEX) %>% na.omit(adsl_counts)
  #drop_na()
 
#Compute the frequency of SAFFL and frequencies are sorted from highest to lowest.
adsl_saf <- adsl %>%
  count(SAFFL) %>%
arrange(desc(SAFFL))

#add the percentages with frequencies for SAFFL? Ensure your
#final answer is multiplied by 100

adsl_safp <- adsl %>%
  select(SAFFL) %>%
  group_by(SAFFL) %>%
  summarise(count=n()) %>%
  mutate(percentage = count/sum(count) * 100)


#create visits data frame

visits <- data.frame(SUBJIDN   = sample(1:10, 30, replace = T),
                     VISITDT   = Sys.Date() + runif(30, min=-365, max=0))

#What is the earliest visit date across all subjects?
visits  %>% arrange(VISITDT) %>% slice_head(n=1)

#What is the latest visit date across all subjects?
visits  %>% arrange(desc(VISITDT))  %>% slice_head(n=1)
 

#Create a data frame that keeps the earliest visit per subject
earvis <- visits  %>% group_by(SUBJIDN) %>% slice_min(VISITDT , n=1)

#Create a data frame that keeps the latest visit per subject
latevis <- visits  %>% group_by(SUBJIDN) %>% slice_max(VISITDT , n=1)

# one more method
latvis <- visits  %>%
  arrange(SUBJIDN,desc(VISITDT))  %>% 
  group_by(SUBJIDN)  %>% 
  top_n(1,VISITDT)

#Create a new column that represents the number of days between 2021-01-01 and 
#the earliest visit date per subject.

daydiff <- visits  %>% group_by(SUBJIDN) %>% slice_min(VISITDT , n=1) %>%
  mutate(newcol = time_length(difftime(as.Date(VISITDT),
                                       as.Date("2021-01-01")),"days")) %>%
  select(-VISITDT)  

daydif <- round(daydiff)


#create ADAE data frame
adae <- data.frame(SUBJIDN = 1:5,
                   AEACN_SUMMARY = c('DOSE MODIFIED;;DOSE NOT CHANGED;',
                                     "DOSE REDUCED;DOSE REDUCED;DRUG WITHDRAWN;",
                                     "NOT APPLICABLE;DOSE REDUCED;;",
                                     ";;;",
                                     "DRUG WITHDRAWN;;;"),
                   stringsAsFactors = FALSE)


#Recreate variables AEACN1, AEACN2, AEACN3 from AEACN_SUMMARY
sep_data <- adae %>%
  separate(AEACN_SUMMARY, into = c("AEACN1","AEACN2","AEACN3"),
           sep = ";")


#create a new concatenated column named AEACN_SUMMARY2.
#Use - as a delimiter instead of ;

conc_col <- sep_data %>%
  mutate(AEACN_SUMMARY2=paste(AEACN1,AEACN2,AEACN3,sep = "-",collapse = NULL))

#keep subjects who eventually had the DRUG WITHDRAWN

drug_withdrwn <- sep_data %>%
  filter(AEACN1 =="DRUG WITHDRAWN" | AEACN2 =="DRUG WITHDRAWN" 
         |AEACN3 =="DRUG WITHDRAWN") %>%
  select(SUBJIDN)
  
#Reshape the data frame created in Q1 into long format.

sep_data %>%
  pivot_longer(cols = c("AEACN1","AEACN2","AEACN3"),
               names_to = "ACTION")
  