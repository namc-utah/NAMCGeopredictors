RAP2=ee$Image("projects/rap-data-365417/assets/vegetation-cover-v3")$select("bare ground")
RAP$Ban
plot(RAP2)
library(raster)


terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>%
  ee$ImageCollection$map(
    function(x) {
      date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
      name <- ee$String$cat("Terraclimate_pp_", date)
      x$select("pr")$rename(name)
    }
  )

plot(
  ee_nc_rain["X200101_Terraclimate_pp_2001_01_01"],
  main = "2001 Jan Precipitation - Terraclimate",
  reset = FALSE
)
