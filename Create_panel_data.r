
# 1. Data preparation - Create Panel data

library(dplyr)
library(tidyverse)
library(openxlsx)


## 1.  Data Transformation

df <- data.frame()

### a.　GINI coeeficientー before taxes and transfers

df_gini_before <- read.csv("../Data/GINI_before_transfers.csv")

df_gini_before_new <- df_gini_before %>%
    filter(
        Measure == "Gini (market income, before taxes and transfers)",
        AGE == "TOT",
        Definition == "Current definition",
        PowerCode == "Units"
    ) %>%
    select(LOCATION,
        TIME,
        METHODO,
        GINI_before_taxes_and_transfers = Value
    )

df_gini_before_11 <- filter(
    df_gini_before_new,
    METHODO == "METH2011"
) # drop

df_gini_before_12 <- filter(
    df_gini_before_new,
    METHODO == "METH2012"
) %>%
    select(
        LOCATION,
        TIME,
        GINI_before_taxes_and_transfers
    )
# We use this !

df <- df_gini_before_12 # Merge


### b. after taxes and transfers

df_gini_after <- read.csv("../Data/GINI_before_transfers.csv")

df_gini_after_12 <- df_gini_after %>%
    filter(
        Measure == "Gini (disposable income, post taxes and transfers)",
        METHODO == "METH2012",
        AGE == "TOT",
        Definition == "Current definition",
        PowerCode == "Units"
    ) %>%
    select(LOCATION,
        TIME,
        GINI_after_taxes_and_transfers = Value
    )



df <- full_join(
    df,
    df_gini_after_12,
    by = c("LOCATION", "TIME")
)

df$Relative_redistributive_effects <- ((df$GINI_before_taxes_and_transfers - df$GINI_after_taxes_and_transfers) / df$GINI_before_taxes_and_transfers) * 100


## c.Poverty line before transfer

df_pov_before <- read.csv("../Data/Poverty_line_before_transfers.csv")

df_pov_before_12 <- df_pov_before %>%
    filter(
        Measure == "Poverty rate before taxes and transfers, Poverty line 50%",
        AGE == "TOT",
        Definition == "Current definition",
        PowerCode == "Units",
        METHODO == "METH2012"
    ) %>%
    select(LOCATION,
        TIME,
        Poverty_rate_before_taxes_and_transfers = Value
    )


df <- full_join(
    df,
    df_pov_before_12,
    by = c("LOCATION", "TIME")
)

## d.Poverty line after transfer

df_pov_after <- read.csv("../Data/Poverty_line_after_transfers.csv")

df_pov_after_12 <- df_pov_after %>%
    filter(
        Measure == "Poverty rate after taxes and transfers, Poverty line 50%",
        AGE == "TOT",
        Definition == "Current definition",
        PowerCode == "Units",
        METHODO == "METH2012"
    ) %>%
    select(LOCATION,
        TIME,
        Poverty_rate_after_taxes_and_transfers = Value
    )

df <- full_join(
    df,
    df_pov_after_12,
    by = c("LOCATION", "TIME")
)


## e. Grants by pravate agencies and NGOs

df_grants <- read.csv("../Data/Grants_by_private_agencies_and_NGOs.csv")
# INDICATOR == "GRANT"
# SUBJECT ==  "TOT"
# MEASURE == "MLN_USD"
# FREQUENCY == "A"

df_grants <- select(
    df_grants,
    LOCATION,
    TIME,
    Grants_by_private_agencies_and_NGOs = Value
)

df <- full_join(
    df,
    df_grants,
    by = c("LOCATION", "TIME")
)

df_gdp <- read.csv("../Data/GDP.csv")
# INDICATOR == "GDP"
# SUBJECT ==  "TOT"
# MEASURE == "MLN_USD"
# FREQUENCY == "A"

df_gdp <- read.csv("../Data/GDP.csv")

# TRANSACT == "B1_GE"
# Transaction == "Gross domestic product (expenditure approach)"
# Unit == "US Dollar"
# PowerCode == "Millions"
# reference period == 2015

df_gdp <- df_gdp %>%
    select(
        LOCATION,
        TIME,
        GDP_based_2015 = Value
    )

df <- full_join(
    df,
    df_gdp,
    by = c("LOCATION", "TIME")
)

df <- df %>%
    mutate(Grants_by_private_agencies_and_NGOs = (Grants_by_private_agencies_and_NGOs / GDP_based_2015) * 100)

## f. Public transfer

df_pub <- read.csv("../Data/Public_transfer.csv")
# INDICATOR == "SOCEXP"
# SUBJECT == "PUB"
# MEASURE == "PC_GDP"
# FREQUEENCY == "A"

df_pub <- df_pub %>%
    select(LOCATION,
        TIME,
        Public_transfer = Value
    )

df <- full_join(
    df,
    df_pub,
    by = c("LOCATION", "TIME")
)

## g. Private transfer

df_priv <- read.csv("../Data/Private_transfer.csv")
# INDICATOR == "SOCEXP"
# SUBJECT == "PRIV"
# MEASURE == "PC_GDP"
# FREQUEENCY == "A"

df_priv <- df_priv %>%
    select(LOCATION,
           TIME,
           Private_transfer = Value
           )

df <- full_join(
    df,
    df_priv,
    by = c("LOCATION", "TIME")
)


## h. Net Public transfer


df_net_pub <- read.csv("../Data/Net_public_transfer.csv")
# INDICATOR == "SOCEXP"
# SUBJECT == "PUBNET"
# MEASURE == "PC_GDP"
# FREQUEENCY == "A"

df_net_pub <- df_net_pub %>%
    select(LOCATION,
        TIME,
        Net_public_transfer = Value
    )

df <- full_join(
    df,
    df_net_pub,
    by = c("LOCATION", "TIME")
)


## i.Tax Revenue

df_tax_revenue <- read.csv("../Data/Tax_revenue.csv")
# INDICATOR == "TAXREV"
# SUBJECT == "TOT"
# MEASURE == "PC_GDP"
# FREQUEENCY == "A"

df_tax <- df_tax_revenue %>%
    select(LOCATION,
        TIME,
        Tax_revenue = Value
    )

df <- full_join(
    df,
    df_tax,
    by = c("LOCATION", "TIME")
)





# j. unemployment rate from World Bank

df_un <- read.csv("../Data/Unemployment_rate_WB.csv")

df_un$Time <- as.integer(df_un$Time)


df_un <- df_un %>%
    select(
        TIME = Time,
        LOCATION = Country.Code,
        Unemployment_rate_pct = Unemployment..total....of.total.labor.force...modeled.ILO.estimate...SL.UEM.TOTL.ZS.
    )

df_un$Unemployment_rate_pct <- as.numeric(df_un$Unemployment_rate_pct)


df <- left_join(
    df,
    df_un,
    by = c("LOCATION", "TIME")
)


# k. poverty_rate

df_pov_rate <- read.csv("../Data/Poverty_rate.csv")


df_pov_rate <- df_pov_rate %>%
    select(TIME,
        LOCATION,
        Poverty_rate = Value
    )


df <- left_join(
    df,
    df_pov_rate,
    by = c("LOCATION", "TIME")
)

# l.

df_eld <- read.csv("../Data/Elderly_population.csv")

df_eld <- df_eld %>%
    select(TIME,
           LOCATION,
           Elderly_pct = Value)
df <- left_join(
    df,
    df_eld,
    by = c("LOCATION", "TIME")
)




# Change the country name
Acronym <- c(
    "AUS",
    "AUT",
    "BEL",
    "CAN",
    "CZE",
    "DNK",
    "FIN",
    "FRA",
    "DEU",
    "GRC",
    "HUN",
    "ISL",
    "IRL",
    "ITA",
    "JPN",
    "KOR",
    "LUX",
    "NLD",
    "NZL",
    "NOR",
    "POL",
    "PRT",
    "SVK",
    "ESP",
    "SWE",
    "CHE",
    "TUR",
    "GBR",
    "USA",
    "EST",
    "ISR",
    "SVN",
    "MEX",
    "CHL",
    "LVA",
    "LTU",
    "COL",
    "BRA",
    "IND",
    "RUS",
    "ZAF",
    "CRI",
    "ROU",
    "BGR",
    "SAU",
    "ARE",
    "HKG",
    "HRV",
    "IDN",
    "MAR",
    "MKD",
    "SGP",
    "SRB",
    "ZMB",
    "ALB",
    "ARG",
    "CHN",
    "CYP",
    "MLT"
)
Country_names <- c(
    "Australia",
    "Austria",
    "Belgium",
    "Canada",
    "Czech Republic",
    "Denmark",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Iceland",
    "Ireland",
    "Italy",
    "Japan",
    "Republic of Korea",
    "Luxembourg",
    "Netherlands",
    "New Zealand",
    "Norway",
    "Poland",
    "Portugal",
    "Slovakia",
    "Spain",
    "Sweden",
    "Switzerland",
    "Turkey",
    "United Kingdom",
    "United States of America",
    "Estonia",
    "Israel",
    "Slovenia",
    "Mexico",
    "Chile",
    "Latvia",
    "Lithuania",
    "Colombia",
    "Brazil",
    "India",
    "Russia",
    "South Africa",
    "Costa Rica",
    "Romania",
    "Bulgaria",
    "Saudi Arabia",
    "United Arab Emirates",
    "Hong Kong",
    "Croatia",
    "Indonesia",
    "Morocco",
    "North Macedonia",
    "Singapore",
    "Serbia",
    "Zambia",
    "Albania",
    "Argentina",
    "China",
    "Cyprus",
    "Malta"
)
for (i in 1:length(Acronym)) {
    df$LOCATION[df$LOCATION == Acronym[i]] <- Country_names[i]
}


## World Giving Index

df_WGI <- read.csv("../Data/WGI_giving money.csv")



df_WGI <- df_WGI %>%
    select(
        LOCATION = Nation,
        TIME = year,
        Giving_money = index1.giving.money.,
        World_giving_index = index2.WGI.
    )

for (i in 1:length(Acronym)) {
    df_WGI$LOCATION[df_WGI$LOCATION == Acronym[i]] <- Country_names[i]
}

df_WGI$Giving_money <- as.integer(df_WGI$Giving_money)

df_WGI <- df_WGI[(df_WGI$Giving_money != "") &
    (df_WGI$Giving_money != "-"),]

df <- full_join(
    df,
    df_WGI,
    by = c("LOCATION", "TIME")
)

# 2. Prepare for multi-regression with fixed and time effects

## add new dummy codes of countries and time


df <- df %>%
    mutate(Australia = ifelse(df$LOCATION == "Australia", 1, 0)) %>%
    mutate(Austria = ifelse(df$LOCATION == "Austria", 1, 0)) %>%
    mutate(Belgium = ifelse(df$LOCATION == "Belgium", 1, 0)) %>%
    mutate(Canada = ifelse(df$LOCATION == "Canada", 1, 0)) %>%
    mutate(Czech_Republic = ifelse(df$LOCATION == "Czech Republic", 1, 0)) %>%
    mutate(Denmark = ifelse(df$LOCATION == "Denmark", 1, 0)) %>%
    mutate(Finland = ifelse(df$LOCATION == "Finland", 1, 0)) %>%
    mutate(France = ifelse(df$LOCATION == "France", 1, 0)) %>%
    mutate(Germany = ifelse(df$LOCATION == "Germany", 1, 0)) %>%
    mutate(Greece = ifelse(df$LOCATION == "Greece", 1, 0)) %>%
    mutate(Hungary = ifelse(df$LOCATION == "Hungary", 1, 0)) %>%
    mutate(Iceland = ifelse(df$LOCATION == "Iceland", 1, 0)) %>%
    mutate(Ireland = ifelse(df$LOCATION == "Ireland", 1, 0)) %>%
    mutate(Italy = ifelse(df$LOCATION == "Italy", 1, 0)) %>%
    mutate(Japan = ifelse(df$LOCATION == "Japan", 1, 0)) %>%
    mutate(Republic_of_Korea = ifelse(df$LOCATION == "Republic of Korea", 1, 0)) %>%
    mutate(Luxembourg = ifelse(df$LOCATION == "Luxembourg", 1, 0)) %>%
    mutate(Netherlands = ifelse(df$LOCATION == "Netherlands", 1, 0)) %>%
    mutate(New_Zealand = ifelse(df$LOCATION == "New Zealand", 1, 0)) %>%
    mutate(Norway = ifelse(df$LOCATION == "Norway", 1, 0)) %>%
    mutate(Poland = ifelse(df$LOCATION == "Poland", 1, 0)) %>%
    mutate(Portugal = ifelse(df$LOCATION == "Portugal", 1, 0)) %>%
    mutate(Slovakia = ifelse(df$LOCATION == "Slovakia", 1, 0)) %>%
    mutate(Spain = ifelse(df$LOCATION == "Spain", 1, 0)) %>%
    mutate(Sweden = ifelse(df$LOCATION == "Sweden", 1, 0)) %>%
    mutate(Switzerland = ifelse(df$LOCATION == "Switzerland", 1, 0)) %>%
    mutate(Turkey = ifelse(df$LOCATION == "Turkey", 1, 0)) %>%
    mutate(United_Kingdom = ifelse(df$LOCATION == "United Kingdom", 1, 0)) %>%
    mutate(United_States_of_America = ifelse(df$LOCATION == "United States of America", 1, 0)) %>%
    mutate(Estonia = ifelse(df$LOCATION == "Estonia", 1, 0)) %>%
    mutate(Israel = ifelse(df$LOCATION == "Israel", 1, 0)) %>%
    mutate(Slovenia = ifelse(df$LOCATION == "Slovenia", 1, 0)) %>%
    mutate(Mexico = ifelse(df$LOCATION == "Mexico", 1, 0)) %>%
    mutate(Chile = ifelse(df$LOCATION == "Chile", 1, 0)) %>%
    mutate(Latvia = ifelse(df$LOCATION == "Latvia", 1, 0)) %>%
    mutate(Lithuania = ifelse(df$LOCATION == "Lithuania", 1, 0)) %>%
    mutate(Colombia = ifelse(df$LOCATION == "Colombia", 1, 0)) %>%
    mutate(Brazil = ifelse(df$LOCATION == "Brazil", 1, 0)) %>%
    mutate(India = ifelse(df$LOCATION == "India", 1, 0)) %>%
    mutate(Russia = ifelse(df$LOCATION == "Russia", 1, 0)) %>%
    mutate(South_Africa = ifelse(df$LOCATION == "South Africa", 1, 0)) %>%
    mutate(Costa_Rica = ifelse(df$LOCATION == "Costa Rica", 1, 0)) %>%
    mutate(Romania = ifelse(df$LOCATION == "Romania", 1, 0)) %>%
    mutate(Bulgaria = ifelse(df$LOCATION == "Bulgaria", 1, 0)) %>%
    mutate(Saudi_Arabia = ifelse(df$LOCATION == "Saudi Arabia", 1, 0)) %>%
    mutate(United_Arab_Emirates = ifelse(df$LOCATION == "United Arab Emirates", 1, 0)) %>%
    mutate(Hong_Kong = ifelse(df$LOCATION == "Hong Kong", 1, 0)) %>%
    mutate(Croatia = ifelse(df$LOCATION == "Croatia", 1, 0)) %>%
    mutate(Indonesia = ifelse(df$LOCATION == "Indonesia", 1, 0)) %>%
    mutate(Morocco = ifelse(df$LOCATION == "Morocco", 1, 0)) %>%
    mutate(North_Macedonia = ifelse(df$LOCATION == "North Macedonia", 1, 0)) %>%
    mutate(Singapore = ifelse(df$LOCATION == "Singapore", 1, 0)) %>%
    mutate(Serbia= ifelse(df$LOCATION == "Serbia", 1, 0)) %>%
    mutate(Zambia = ifelse(df$LOCATION == "Zambia", 1, 0)) %>%
    mutate(Albania = ifelse(df$LOCATION == "Albania", 1, 0)) %>%
    mutate(Argentina = ifelse(df$LOCATION == "Argentina", 1, 0)) %>%
    mutate(China = ifelse(df$LOCATION == "China", 1, 0)) %>%
    mutate(Cyprus = ifelse(df$LOCATION == "Cyprus", 1, 0)) %>%
    mutate(Malta = ifelse(df$LOCATION == "Malta", 1, 0))




# Time variable 1970~2021

df <- df %>%
    mutate(y1970 = ifelse(df$TIME == 1970, 1, 0)) %>%
    mutate(y1971 = ifelse(df$TIME == 1971, 1, 0)) %>%
    mutate(y1972 = ifelse(df$TIME == 1972, 1, 0)) %>%
    mutate(y1973 = ifelse(df$TIME == 1973, 1, 0)) %>%
    mutate(y1974 = ifelse(df$TIME == 1974, 1, 0)) %>%
    mutate(y1975 = ifelse(df$TIME == 1975, 1, 0)) %>%
    mutate(y1976 = ifelse(df$TIME == 1976, 1, 0)) %>%
    mutate(y1977 = ifelse(df$TIME == 1977, 1, 0)) %>%
    mutate(y1978 = ifelse(df$TIME == 1978, 1, 0)) %>%
    mutate(y1979 = ifelse(df$TIME == 1979, 1, 0)) %>%
    mutate(y1980 = ifelse(df$TIME == 1980, 1, 0)) %>%
    mutate(y1981 = ifelse(df$TIME == 1981, 1, 0)) %>%
    mutate(y1982 = ifelse(df$TIME == 1982, 1, 0)) %>%
    mutate(y1983 = ifelse(df$TIME == 1983, 1, 0)) %>%
    mutate(y1984 = ifelse(df$TIME == 1984, 1, 0)) %>%
    mutate(y1985 = ifelse(df$TIME == 1985, 1, 0)) %>%
    mutate(y1986 = ifelse(df$TIME == 1986, 1, 0)) %>%
    mutate(y1987 = ifelse(df$TIME == 1987, 1, 0)) %>%
    mutate(y1988 = ifelse(df$TIME == 1988, 1, 0)) %>%
    mutate(y1989 = ifelse(df$TIME == 1989, 1, 0)) %>%
    mutate(y1990 = ifelse(df$TIME == 1990, 1, 0)) %>%
    mutate(y1991 = ifelse(df$TIME == 1991, 1, 0)) %>%
    mutate(y1992 = ifelse(df$TIME == 1992, 1, 0)) %>%
    mutate(y1993 = ifelse(df$TIME == 1993, 1, 0)) %>%
    mutate(y1994 = ifelse(df$TIME == 1994, 1, 0)) %>%
    mutate(y1995 = ifelse(df$TIME == 1995, 1, 0)) %>%
    mutate(y1996 = ifelse(df$TIME == 1996, 1, 0)) %>%
    mutate(y1997 = ifelse(df$TIME == 1997, 1, 0)) %>%
    mutate(y1998 = ifelse(df$TIME == 1998, 1, 0)) %>%
    mutate(y1999 = ifelse(df$TIME == 1999, 1, 0)) %>%
    mutate(y2000 = ifelse(df$TIME == 2000, 1, 0)) %>%
    mutate(y2001 = ifelse(df$TIME == 2001, 1, 0)) %>%
    mutate(y2002 = ifelse(df$TIME == 2002, 1, 0)) %>%
    mutate(y2003 = ifelse(df$TIME == 2003, 1, 0)) %>%
    mutate(y2004 = ifelse(df$TIME == 2004, 1, 0)) %>%
    mutate(y2005 = ifelse(df$TIME == 2005, 1, 0)) %>%
    mutate(y2006 = ifelse(df$TIME == 2006, 1, 0)) %>%
    mutate(y2007 = ifelse(df$TIME == 2007, 1, 0)) %>%
    mutate(y2008 = ifelse(df$TIME == 2008, 1, 0)) %>%
    mutate(y2009 = ifelse(df$TIME == 2009, 1, 0)) %>%
    mutate(y2010 = ifelse(df$TIME == 2010, 1, 0)) %>%
    mutate(y2011 = ifelse(df$TIME == 2011, 1, 0)) %>%
    mutate(y2012 = ifelse(df$TIME == 2012, 1, 0)) %>%
    mutate(y2013 = ifelse(df$TIME == 2013, 1, 0)) %>%
    mutate(y2014 = ifelse(df$TIME == 2014, 1, 0)) %>%
    mutate(y2015 = ifelse(df$TIME == 2015, 1, 0)) %>%
    mutate(y2016 = ifelse(df$TIME == 2016, 1, 0)) %>%
    mutate(y2017 = ifelse(df$TIME == 2017, 1, 0)) %>%
    mutate(y2018 = ifelse(df$TIME == 2018, 1, 0)) %>%
    mutate(y2019 = ifelse(df$TIME == 2019, 1, 0)) %>%
    mutate(y2020 = ifelse(df$TIME == 2020, 1, 0)) %>%
    mutate(y2021 = ifelse(df$TIME == 2021, 1, 0))


df <- df[order(df$LOCATION, df$TIME),]

df<- df[!is.na(df$LOCATION)&
        !is.na(df$TIME),]




# fill in missing values and add religion data

## . i  religion data

df_religion <- read.csv("../Data/WRP national data.csv")

df_rel <- df_religion %>%
    mutate(
        Chrstprotpct = chrstprotpct * 100, # Protestnat
        Chrstcatpct = chrstcatpct * 100, # Roman Catholic
        Chrstorthpct = chrstorthpct * 100, # Eastern Orthodox
        Chrstangpct = chrstangpct * 100, # Anglican
        Chrstothrpct = chrstothrpct * 100, # Others
        Christ_general_pct = chrstgenpct * 100,
        Islmsunpct = islmsunpct * 100, # Sunni
        Islmshipct = islmshipct * 100, # Shi'a
        Islam_general_pct = islmgenpct * 100,
        Budmahpct = budmahpct * 100, # Mahayana
        Budthrpct = budthrpct * 100, # Theravada
        Budothrpct = budothrpct * 100,
        Buddism_general = budgenpct * 100,
    ) %>%
    select(
        TIME = year,
        LOCATION = name,
        Chrstprotpct,
        Chrstcatpct,
        Chrstorthpct,
        Chrstangpct,
        Chrstothrpct,
        Christ_general_pct,
        Islmsunpct,
        Islmshipct,
        Islam_general_pct,
        Budmahpct,
        Budthrpct,
        Budothrpct,
        Buddism_general,
        Relegion_reliable_level = reliabilevel
    )


for (i in 1:length(Acronym)) {
    df_rel$LOCATION[df_rel$LOCATION == Acronym[i]] <- Country_names[i]
}

df <- left_join(
    df,
    df_rel,
    by = c("LOCATION", "TIME")
)

# Appendix


df_UN <- read.csv("../Data/UNdata_Export.csv")


df_UN <- df_UN[df_UN$Sex == "Both Sexes", ]

df_UN <- mutate(df_UN, id = rownames(df_UN))

df_UN <- gather(df_UN, key = Religion, value = Value, contains("l."))

df_UN <- spread(df_UN, key = Religion, value = Value)

write.csv(df_UN, "../DAta/test.csv")

df_UN <- read.csv("../Data/Religion_revised.csv")

df_UN[is.na(df_UN)] <- 0

df_UN_new <- aggregate(
    x = df_UN[c("Total", "Protestant", "Catholic", "Christian_Orthodox", "Christian", "Sunni_Muslims", "Shiite_Islam", "Islam", "Buddhist")],
    by = list(df_UN$Country.or.Area, df_UN$Year),
    FUN = sum
)

df_UN_new <- df_UN_new %>%
    mutate(
        Protestant = (Protestant / Total) * 100,
        Catholic = (Catholic / Total) * 100,
        Christian_Orthodox = (Christian_Orthodox / Total) * 100,
        Christian = (Christian / Total) * 100,
        Sunni_Muslims = (Sunni_Muslims / Total) * 100,
        Shiite_Islam = (Shiite_Islam / Total) * 100,
        Islam = (Islam / Total) * 100,
        Buddhist = (Buddhist / Total) * 100
    ) %>%
    select(
        LOCATION = Group.1,
        TIME = Group.2,
        Chrstprotpct = Protestant,
        Chrstcatpct = Catholic,
        Chrstorthpct = Christian_Orthodox,
        Christ_general_pct = Christian,
        Islmsunpct = Sunni_Muslims,
        Islmshipct = Shiite_Islam,
        Islam_general_pct = Islam,
        Buddism_general = Buddhist,
    )

df_UN_new[df_UN_new == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"

ls <- c(
    "Austria", "Bulgaria", "Germany", "Ireland", "Lithuania",
    "New Zealand", "Portugal", "Republic of Korea",
    "Romania", "Singapore", "Serbia", "Slovakia", "Slovenia", "South Africa",
    "Switzerland", "United Kingdom"
)
lm <- c(1995, 1996, 2000, 2001, 2002, 2005, 2006, 2010, 2011, 2013, 2015, 2016, 2018, 2020, 2021)


for (l in ls) {
    for (t in lm) {
        if (length(df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]) != 0) {
            m1 <- df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]$Chrstprotpct
            m2 <- df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]$Chrstcatpct
            m3 <- df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]$Chrstorthpct
            m4 <- df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]$Christ_general_pct
            m5 <- df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]$Islmsunpct
            m6 <- df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]$Islmshipct
            m7 <- df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]$Islam_general_pct
            m8 <- df_UN_new[(df_UN_new$LOCATION == l) & (df_UN_new$TIME == t), ]$Buddism_general
            if (length(m1) > 0) {
                df[(df$LOCATION == l) & (df$TIME == t), "Chrstprotpct"] <- m1
            }
            if (length(m2) > 0) {
                df[(df$LOCATION == l) & (df$TIME == t), "Chrstcatpct"] <- m2
            }
            if (length(m3) > 0) {
                df[(df$LOCATION == l) & (df$TIME == t), "Chrstorthpct"] <- m3
            }
            if (length(m4) > 0) {
                df[(df$LOCATION == l) & (df$TIME == t), "Christ_general_pct"] <- m4
            }
            if (length(m5) > 0) {
                df[(df$LOCATION == l) & (df$TIME == t), "Islmsunpct"] <- m5
            }
            if (length(m6) > 0) {
                df[(df$LOCATION == l) & (df$TIME == t), "Islmshipct"] <- m6
            }
            if (length(m7) > 0) {
                df[(df$LOCATION == l) & (df$TIME == t), "Islam_general_pct"] <- m7
            }
            if (length(m8) > 0) {
                df[(df$LOCATION == l) & (df$TIME == t), "Buddism_general"] <- m8
            }
        }
    }
}

# politics

df_pol <- read.csv("../Data/MPDataset_MPDS2021a.csv")
df_pol$date <- substr(df_pol$date, 1, 4)
df_pol$date <- as.integer(df_pol$date)

df_pol <- df_pol %>%
    mutate(Seats_pct = (absseat / totseats) * 100)

df_pol[is.na(df_pol$Seats_pct), "Seats_pct"] <- 0

df_pol_ <- aggregate(
    x = df_pol$Seats_pct,
    by = c(list(df_pol$countryname, df_pol$date)),
    FUN = max
)

df_pol <- df_pol %>%
    dplyr::group_by(countryname, date) %>%
    summarise(
        Max_seats_pct = max(Seats_pct),
        Seats_pct,
        per503,
        per504,
        rile,
        welfare
    ) %>%
    dplyr::ungroup()



df_pol$compare <- if_else(df_pol$Seats_pct == df_pol$Max_seats_pct, "yes", "no")

df_pol <- df_pol[df_pol["compare"] == "yes", ] %>%
    select(
        LOCATION = countryname,
        TIME = date,
        Max_seats_pct,
        Equality_pos = per503,
        Welfare_exp= per504, # pct of total votes
        Right_left = rile,
        Welfare = welfare
    )

df_pol[, "Election_year"] <- 1

df_pol[df_pol == "South Korea"] <- "Republic of Korea"

df_pol[df_pol == "United States"] <- "United States of America"

df_pol$Election_year <- 1



df <- left_join(
    df,
    df_pol,
    by = c("LOCATION", "TIME")
)

df[is.na(df$Election_year), "Election_year"] <- 0

# finally
df <- df %>%
    dplyr::group_by(LOCATION) %>%
    fill(Chrstprotpct, .direction = "updown") %>%
    fill(Chrstcatpct, .direction = "updown") %>%
    fill(Chrstorthpct, .direction = "updown") %>%
    fill(Chrstangpct, .direction = "updown") %>%
    fill(Chrstothrpct, .direction = "updown") %>%
    fill(Christ_general_pct, .direction = "updown") %>%
    fill(Islmsunpct, .direction = "updown") %>%
    fill(Islmshipct, .direction = "updown") %>%
    fill(Islam_general_pct, .direction = "updown") %>%
    fill(Budmahpct, .direction = "updown") %>%
    fill(Budthrpct, .direction = "updown") %>%
    fill(Buddism_general, .direction = "updown") %>%
    fill(Max_seats_pct, .direction = "down") %>%
    fill(Equality_pos, .direction = "down") %>%
    fill(Welfare_exp, .direction = "down") %>%
    fill(Right_left, .direction = "down") %>%
    fill(Welfare, .direction = "down") %>%
    dplyr::ungroup()

# 2. Take log

## a. Grants

#df$log_Grants_by_private_agencies_and_NGOs <- log(df$Grants_by_private_agencies_and_NGOs)

## b. Private Transfer

df$log_Private_transfer <- log(df$Private_transfer)



# remove abnormal data

## A. Exp var

### a. Relative redistributive effects

####  China, Japan,
df[(df$LOCATION == "China") | (df$LOCATION == "Japan"),"Relative_redistributive_effects"] <- NA


### b. Public transfer

#####　Costa Rica, Latvia(1995~6),Slovenia(only 1995: abnormal),Poland(1990~93)

df[(df$LOCATION == "Costa_Rica"), "Public_transfer"] <- NA
df[(df$LOCATION == "Latvia") & (df$TIME <= 1995), "Public_transfer"] <- NA
df[(df$LOCATION == "Slovenia") & (df$TIME <= 1998), "Public_transfer"] <- NA
df[(df$LOCATION == "Poland") & (df$TIME <= 1995), "Public_transfer"] <- NA




### c. Net public transfer

#### Lithuania

df[(df$LOCATION == "Lithuania"), "Net_public_transfer"] <- NA


## B. Target var

### a. Grants_by_private_agencies_and_NGOs

##### Czech Republic, Estonia, Latvia, Lithuania, Russia,


df[(df$LOCATION == "Czech Republic") | (df$LOCATION == "Estonia") | (df$LOCATION == "Latvia") | (df$LOCATION == "Lithuania") | (df$LOCATION == "Russia"), "log_Grants_by_private_agencies_and_NGOs"] <- NA

### b. Private transfer

##### Finland(2018?), Greece, Lithuania(~2002), Slovenia(~2003)


df[(df$LOCATION == "Finlnd") &  (df$TIME > 2015), "log_Private_transfer"] <- NA

df[(df$LOCATION == "Greece"), "log_Private_transfer"] <- NA

df[(df$LOCATION == "Lithuania") & (df$TIME < 2005), "log_Private_transfer"] <- NA

df[(df$LOCATION == "Slovenia") & (df$TIME < 2005), "log_Private_transfer"] <- NA

### c. Giving money

#### なし


## C. Control var

### a. Poverty rate

#### China, Japan,

df[(df$LOCATION == "China") | (df$LOCATION == "Japan"), "Poverty_rate"] <- NA

### b. Unemployment_rate_pct

df[(df$LOCATION == "Greece"), "Unemployment_rate_pct"] <- NA

write.csv(df, "../Data/panel_data.csv")
