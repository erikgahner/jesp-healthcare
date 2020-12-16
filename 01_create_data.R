###
## Article:   Personal politics? 
##            Healthcare policies, personal experiences and government attitudes 
## 
##            Journal of European Social Policy
##
##            Erik Gahner Larsen
##            erikgahner@gmail.com
##
##        
## Data:      Health and Health Care - ISSP 2011: https://doi.org/10.4232/1.12252
##            WDI: https://datacatalog.worldbank.org/dataset/world-development-indicators
##
###

library("tidyverse")
library("haven")

# Download Health and Health Care - ISSP 2011 from: https://doi.org/10.4232/1.12252
issp_raw <- read_dta("ZA5800_v3-0-0.dta")

# Download WDI data from: https://datacatalog.worldbank.org/dataset/world-development-indicators
wdi_raw <- read_csv("WDIData.csv") %>% rename(code = `Country Code`)
wdi_country <- read_csv("WDICountry.csv") %>%
  select(`Country Code`, `2-alpha code`) %>%
  mutate(cntry = `2-alpha code`,
         code = `Country Code`) %>%
  select(code, cntry)

wdi <- left_join(wdi_raw, wdi_country, by = "code")

wdi_phe <- wdi %>%
  filter(`Indicator Code` == "SH.XPD.GHED.CH.ZS") %>%
  pivot_longer(`1960`:`2020`, names_to = "year", values_to = "phe") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(`Country Name`, cntry, year, phe)

wdi_the <- wdi %>%
  filter(`Indicator Code` == "SH.XPD.CHEX.GD.ZS") %>%
  select(-`Country Name`) %>%
  pivot_longer(`1960`:`2020`, names_to = "year", values_to = "the") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(cntry, year, the)

wdi_gini <- wdi %>%
  filter(`Indicator Code` == "SI.POV.GINI") %>%
  select(-`Country Name`) %>%
  pivot_longer(`1960`:`2020`, names_to = "year", values_to = "gini") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(cntry, year, gini)

wdi_gdp <- wdi %>%
  filter(`Indicator Code` == "NY.GDP.PCAP.CD") %>%
  select(-`Country Name`) %>%
  pivot_longer(`1960`:`2020`, names_to = "year", values_to = "gdp") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(cntry, year, gdp)

wdi_beds <- wdi %>%
  filter(`Indicator Code` == "SH.MED.BEDS.ZS") %>%
  select(-`Country Name`) %>%
  pivot_longer(`1960`:`2020`, names_to = "year", values_to = "beds") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(cntry, year, beds)

wdi_oop <- wdi %>%
  filter(`Indicator Code` == "SH.XPD.OOPC.CH.ZS") %>%
  select(-`Country Name`) %>%
  pivot_longer(`1960`:`2020`, names_to = "year", values_to = "oop") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(cntry, year, oop)

issp_data <- issp_raw %>%
  mutate(cntry = case_when(
    C_ALPHAN == "BE-FLA" | C_ALPHAN == "BE-WAL" ~ "BE",
    C_ALPHAN == "DE-W" | C_ALPHAN == "DE-E" ~ "DE",
    C_ALPHAN == "GB-GBN" ~ "GB",
    TRUE ~ C_ALPHAN
  ),
  year = DATEYR) %>%
  filter(cntry != "TW") 

issp <- reduce(list(issp_data, wdi_phe, wdi_the, wdi_gini, 
                    wdi_gdp, wdi_beds, wdi_oop), left_join, 
               by = c("cntry", "year")) %>% 
  drop_na(phe) 


issp <- issp %>% 
  mutate(
    # outcome
    moregov = ifelse(V13 < 8, V13, NA),
    
    sat_doctor = ifelse(V52 < 97, V52, NA),
    sat_hospital = ifelse(V54 < 97, V54, NA),
    sat_general = ifelse(V51 < 97, V51, NA),
    
    badexp = ifelse(
      V45 == 1 | V46 == 1 | V47 == 1 | V48 == 1, 1, 0),
    
    male = ifelse(SEX == 1, 1, 0),
    age = ifelse(AGE < 100, AGE, NA),
    age2100 = (age*age)/100,
    edu = DEGREE,
    union = ifelse(UNION == 1, 1, 0),
    married = ifelse(MARITAL == 1, 1, 0),
    healthstatus = ifelse(V59 < 6 & V59 > 0, 6-V59, NA),
    employed = ifelse(MAINSTAT == 1, 1, 0),
    insurance = V63,
    
    name = `Country Name`
  ) 

# The WDI data is updated and do no longer reflect the data used in the manuscript (cf. Table D.2). 
# Here, we make sure to use the data used in the manuscript. 
# Uncomment the lines below to rely on the most recent WDI data.
## (This will not affect any of the results.)
countryleveldata <- read_csv("data_countrylevel.csv")

issp <- issp %>% 
  select(-c("phe", "the", "gini", "beds", "oop", "gdp")) %>% 
  left_join(countryleveldata, by = "name") %>% 
  mutate(phe100 = phe/100)

issp_occ <- issp %>% 
  group_by(ISCO88) %>% 
  summarise(keep_occ = n(),
            .groups = "drop") 

issp <- left_join(issp, issp_occ, by = c("ISCO88"))
issp <- issp %>% 
  mutate(occ = ifelse(keep_occ > 50, ISCO88, 0))

for (i in unique(issp$cntry)) {
  issp <- within(issp, {
    assign(paste0("inc_", i), ifelse(get(paste0(i, "_INC")) > 900000, NA, get(paste0(i, "_INC"))) )
  }
  )
}
issp$inc_JP <- ifelse(issp$JP_INC >= 99999990, NA, issp$JP_INC)
issp$inc_KR <- ifelse(issp$KR_INC >= 99999990, NA, issp$KR_INC)

issp$income <- NA
issp$income[issp$cntry == "AU"] <- cut(issp$inc_AU[issp$cntry == "AU"], breaks = quantile(issp$inc_AU[issp$cntry == "AU"], na.rm = TRUE))
issp$income[issp$cntry == "BE"] <- cut(issp$inc_BE[issp$cntry == "BE"], breaks = quantile(issp$inc_BE[issp$cntry == "BE"], na.rm = TRUE))
issp$income[issp$cntry == "BG"] <- cut(issp$inc_BG[issp$cntry == "BG"], breaks = quantile(issp$inc_BG[issp$cntry == "BG"], na.rm = TRUE))
issp$income[issp$cntry == "CH"] <- cut(issp$inc_CH[issp$cntry == "CH"], breaks = quantile(issp$inc_CH[issp$cntry == "CH"], na.rm = TRUE))
issp$income[issp$cntry == "CL"] <- cut(issp$inc_CL[issp$cntry == "CL"], breaks = quantile(issp$inc_CL[issp$cntry == "CL"], na.rm = TRUE))
issp$income[issp$cntry == "CN"] <- cut(issp$inc_CN[issp$cntry == "CN"], breaks = quantile(issp$inc_CN[issp$cntry == "CN"], na.rm = TRUE))
issp$income[issp$cntry == "CZ"] <- cut(issp$inc_CZ[issp$cntry == "CZ"], breaks = quantile(issp$inc_CZ[issp$cntry == "CZ"], na.rm = TRUE))
issp$income[issp$cntry == "DE"] <- cut(issp$inc_DE[issp$cntry == "DE"], breaks = quantile(issp$inc_DE[issp$cntry == "DE"], na.rm = TRUE))
issp$income[issp$cntry == "DK"] <- cut(issp$inc_DK[issp$cntry == "DK"], breaks = quantile(issp$inc_DK[issp$cntry == "DK"], na.rm = TRUE))
issp$income[issp$cntry == "ES"] <- cut(issp$inc_ES[issp$cntry == "ES"], breaks = quantile(issp$inc_ES[issp$cntry == "ES"], na.rm = TRUE))
issp$income[issp$cntry == "FI"] <- cut(issp$inc_FI[issp$cntry == "FI"], breaks = quantile(issp$inc_FI[issp$cntry == "FI"], na.rm = TRUE))
issp$income[issp$cntry == "FR"] <- cut(issp$inc_FR[issp$cntry == "FR"], breaks = quantile(issp$inc_FR[issp$cntry == "FR"], na.rm = TRUE))
issp$income[issp$cntry == "GB"] <- cut(issp$inc_GB[issp$cntry == "GB"], breaks = quantile(issp$inc_GB[issp$cntry == "GB"], na.rm = TRUE))
issp$income[issp$cntry == "HR"] <- cut(issp$inc_HR[issp$cntry == "HR"], breaks = quantile(issp$inc_HR[issp$cntry == "HR"], na.rm = TRUE))
issp$income[issp$cntry == "IL"] <- cut(issp$inc_IL[issp$cntry == "IL"], breaks = quantile(issp$inc_IL[issp$cntry == "IL"], na.rm = TRUE))
issp$income[issp$cntry == "IT"] <- cut(issp$inc_IT[issp$cntry == "IT"], breaks = quantile(issp$inc_IT[issp$cntry == "IT"], na.rm = TRUE))
issp$income[issp$cntry == "JP"] <- cut(issp$inc_JP[issp$cntry == "JP"], breaks = quantile(issp$inc_JP[issp$cntry == "JP"], na.rm = TRUE))
issp$income[issp$cntry == "KR"] <- cut(issp$inc_KR[issp$cntry == "KR"], breaks = quantile(issp$inc_KR[issp$cntry == "KR"], na.rm = TRUE))
issp$income[issp$cntry == "LT"] <- cut(issp$inc_LT[issp$cntry == "LT"], breaks = quantile(issp$inc_LT[issp$cntry == "LT"], na.rm = TRUE))
issp$income[issp$cntry == "NL"] <- cut(issp$inc_NL[issp$cntry == "NL"], breaks = quantile(issp$inc_NL[issp$cntry == "NL"], na.rm = TRUE))
issp$income[issp$cntry == "NO"] <- cut(issp$inc_NO[issp$cntry == "NO"], breaks = quantile(issp$inc_NO[issp$cntry == "NO"], na.rm = TRUE))
issp$income[issp$cntry == "PH"] <- cut(issp$inc_PH[issp$cntry == "PH"], breaks = quantile(issp$inc_PH[issp$cntry == "PH"], na.rm = TRUE))
issp$income[issp$cntry == "PL"] <- cut(issp$inc_PL[issp$cntry == "PL"], breaks = quantile(issp$inc_PL[issp$cntry == "PL"], na.rm = TRUE))
issp$income[issp$cntry == "PT"] <- cut(issp$inc_PT[issp$cntry == "PT"], breaks = quantile(issp$inc_PT[issp$cntry == "PT"], na.rm = TRUE))
issp$income[issp$cntry == "RU"] <- cut(issp$inc_RU[issp$cntry == "RU"], breaks = quantile(issp$inc_RU[issp$cntry == "RU"], na.rm = TRUE))
issp$income[issp$cntry == "SE"] <- cut(issp$inc_SE[issp$cntry == "SE"], breaks = quantile(issp$inc_SE[issp$cntry == "SE"], na.rm = TRUE))
issp$income[issp$cntry == "SI"] <- cut(issp$inc_SI[issp$cntry == "SI"], breaks = quantile(issp$inc_SI[issp$cntry == "SI"], na.rm = TRUE))
issp$income[issp$cntry == "SK"] <- cut(issp$inc_SK[issp$cntry == "SK"], breaks = quantile(issp$inc_SK[issp$cntry == "SK"], na.rm = TRUE))
issp$income[issp$cntry == "TR"] <- cut(issp$inc_TR[issp$cntry == "TR"], breaks = quantile(issp$inc_TR[issp$cntry == "TR"], na.rm = TRUE))
issp$income[issp$cntry == "US"] <- cut(issp$inc_US[issp$cntry == "US"], breaks = quantile(issp$inc_US[issp$cntry == "US"], na.rm = TRUE))
issp$income[issp$cntry == "ZA"] <- cut(issp$inc_ZA[issp$cntry == "ZA"], breaks = quantile(issp$inc_ZA[issp$cntry == "ZA"], na.rm = TRUE))

issp %>% 
  select(name, cntry, year, moregov, male, age, age2100, edu, married, employed, income, healthstatus, insurance, occ, the, phe, phe100, gini, gdp, beds, oop, sat_doctor, sat_hospital, badexp) %>%
  write_csv("data_issp_healthcare.csv")
