# sandbox

pacman::p_load("readr",
               "readxl",
               "dplyr",
               "stringr",
               "tidyr",
               "ggplot2",
               "cowplot")
# data source: https://www.jodidata.org/oil/database/data-downloads.aspx
# https://www.jodidata.org/_resources/files/downloads/oil-data/jodi-oil-wdb-item-names-ver2017.pdf
# KBD Thousand Barrels per day (kb/d)
# KBBL Thousand Barrels (kbbl)
# KL Thousand Kilolitres (kl)
# KTONS Thousand Metric Tons (kmt)
# CONVBBL Conversion factor barrels/ktons 
input_country_code <- "US"
input_flow_breakdown <- "INDPROD"
rawdata <- readr::read_csv("./data/NewProcedure_Primary_CSV.csv",
                           col_types = "c") |>
  filter(str_detect(REF_AREA, input_country_code),
         !str_detect(OBS_VALUE,"x|N"),
         str_detect(FLOW_BREAKDOWN, input_flow_breakdown)) |>
  mutate(ref_area = stringr::str_trim(REF_AREA),
         ref_date = as.Date(paste0(TIME_PERIOD,"-01")),
         obs_value = as.numeric(OBS_VALUE)
         ) |> 
  distinct()

country_names <- readxl::read_excel("./data/jodi-oil-country-note.xlsx",
                                    sheet = "Country names") |>
  filter(NewName == input_country_code) |>
  distinct(Name, NewName)


# gas prices in the US
# https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=emm_epm0_pte_nus_dpg&f=m
gas_prices <- readr::read_csv("./data/U.S._All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
                              skip = 4) |>
  mutate(ref_date = as.Date(paste0(Month,"-01"), "%b %Y-%d")) |>
  rename(price=`U.S. All Grades All Formulations Retail Gasoline Prices Dollars per Gallon`)



jodi_plot <- rawdata  |>
  filter(ENERGY_PRODUCT=="CRUDEOIL",
         UNIT_MEASURE=="KBD") |>
  ggplot(aes(x = ref_date,
             y = obs_value)) +
  xlim(as.Date("2002-01-01"), as.Date("2022-11-01")) +
  geom_line(lwd = 1) +
  geom_vline(col = "red",
             xintercept = as.Date("2001-01-01")
  ) +
  geom_vline(col = "blue",
             xintercept = as.Date("2009-01-01")
  ) +
  geom_vline(col = "red",
             xintercept = as.Date("2016-01-01")
  ) +
  geom_vline(col = "blue",
             xintercept = as.Date("2020-01-01")
  ) +
  theme_bw() +
  ylab("Thousand Barrels per day (kb/d)") +
  xlab("Year") +
  ggtitle("Crude Oil Production and Gas Prices\nfrom Jan 2002 to Nov 2022 in the United States")


eia_plot <- gas_prices |>
  ggplot(aes(x = ref_date,
             y = price)) +
  xlim(as.Date("2002-01-01"), as.Date("2022-11-01")) +
  geom_line(lwd=1) +
  geom_vline(col = "red",
             xintercept = as.Date("2001-01-01")
  ) +
  geom_vline(col = "blue",
             xintercept = as.Date("2009-01-01")
          ) +
  geom_vline(col = "red",
             xintercept = as.Date("2016-01-01")
  ) +
  geom_vline(col = "blue",
                xintercept = as.Date("2020-01-01")
  ) +
  theme_bw() +
  ylab("Dollars per Gallon ($)") +
  xlab("Year") +
  labs(caption="Sources: Joint Organisations Data Initiative, U.S. Energy Information Administration")

plot_grid(plotlist=list(jodi_plot,
                        eia_plot), ncol=1, align='v')
