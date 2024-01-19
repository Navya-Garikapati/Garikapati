
library(haven)

adsl1 <- haven::read_sas("adsl.sas7bdat")
adae1 <- haven::read_sas("adae.sas7bdat")


n1 <- merge(adsl1, adae1, by.x = "USUBJID", by.y = "USUBJID", all=T) #** Without package**

library(dplyr)

adsl3 <- adsl1[adsl1$FASFL == 'Y',]

adsl_adae123 <-merge(x = adsl3, y = adae1, all = TRUE)

adsl_adae123 <-left_join(adsl3, adae1, by = "USUBJID")

adsl_adae123 <-right_join(adsl3, adae1, by = "USUBJID", sort="USUBJID")

adsl_adae123 <-inner_join(adsl3, adae1, by = "USUBJID", sort="USUBJID")

adsl4 <- arrange(adsl1, USUBJID)
adsl5 <- arrange(adsl1, desc(USUBJID))


