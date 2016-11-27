## choroplethr

install.packages("devtools")
install.packages("acs", clean=T)

library(devtools)
install_github("choroplethr", "trulia")
install_github('arilamstein/choroplethrZip@v1.5.0')

library(choroplethr)
library(choroplethrZip)
#- See more at: https://www.trulia.com/blog/tech/the-choroplethr-package-for-r/#sthash.FLwNmQa2.dpuf

## Per Capita Income
choroplethr_acs("B19301", "zip", endyear = 2014, zoom=c("oklahoma"))
## Total Population
choroplethr_acs("B01003", "zip", endyear = 2014, zoom=c("oklahoma"))

zip_choropleth_acs("B01003", endyear = 2014, state_zoom="oklahoma")

library(ggplot2)
data(df_pop_zip)
zip_choropleth(df_pop_zip, state_zoom="oklahoma", reference_map = TRUE)
zip_choropleth(df_pop_zip, state_zoom="oklahoma", reference_map = T)


zip_choropleth(df_pop_zip,
               state_zoom    = "new york",
               title         = "2012 New York State ZCTA Population Estimates",
               legend        = "Population",
               reference_map = TRUE)
## bingo
zip_choropleth(df_pop_zip,
               state_zoom    = "oklahoma",
               title         = "2012 ZCTA Population Estimates",
               legend        = "Population",
               reference_map = TRUE)

## yeah!!
zip_choropleth(df_pop_zip,
               state_zoom    = "oklahoma",
               county_zoom   = 40115,
               title         = "2012 ZCTA Population Estimates",
               legend        = "Population",
               reference_map = TRUE)

## yeah!!
ottawa = c("74358", "74339", "74354")
zip_choropleth(df_pop_zip,
               state_zoom    = "oklahoma",
               county_zoom   = 40115,
               zip_zoom      = c(ottawa),
               title         = "2012 ZCTA Population Estimates",
               legend        = "Population",
               reference_map = TRUE)

# Zoom ottawa county
zip_choropleth_acs("B01003", endyear = 2014, state_zoom="oklahoma", county_zoom = 40115)

library(ggplot2)
data(df_pop_zip)
choro = ZipChoropleth$new(df_pop_zip)
choro$title = "2012 ZCTA Population Estimates"
choro$ggplot_scale = scale_fill_brewer(name="Population", palette=2, drop=FALSE)
choro$set_zoom_zip(state_zoom="oklahoma", county_zoom=NULL, msa_zoom=NULL, zip_zoom=NULL)
choro$render()



# Per capita income of all ZCTAs in New York State
zip_choropleth_acs("B19301", state_zoom="new york")
zip_choropleth_acs("B19301", state_zoom="oklahoma")

county_choropleth(df_pop_county)
county_choropleth(df_pop_county, zoom = "oklahoma")

data(zipcode, package="zipcode", envir=environment())
df = data.frame(region=zipcode$zip, value = sample(100, nrow(zipcode), replace=TRUE))
zip_map(df, buckets=1, zoom = "oklahoma")

#- See more at: https://www.trulia.com/blog/tech/the-choroplethr-package-for-r/#sthash.xdmD1bVc.dpuf


zip_choropleth_acs(tableId="B19301", lod="state")
choroplethr_acs(tableId="B19301", map = "state")




state_choropleth_acs(tableId="B19301", lod="zip")

data(df_pop_state)
state_choropleth(df_pop_state, 
                 title      = "US 2012 State Population Estimates", 
                 legend     = "Population", 
                 num_colors = 1,
                 zoom       = c("california", "oregon", "washington"))

data(df_pop_county)
county_choropleth(df_pop_county,
                  title      = "2012 Population Estimates",
                  legend     = "Population",
                  state_zoom = c("california", "washington", "oregon"))

county_choropleth(df_pop_county,
                  title      = "2012 Population Estimates",
                  legend     = "Population",
                  num_colors = 1,
                  state_zoom = c("oklahoma"))

choroplethr_acs(tableId="B19301", lod="county")
choro