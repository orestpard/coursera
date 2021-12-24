library(dplyr)
data("airquality")
class(airquality)
airquality <- tbl_df(airquality)
class(airquality)
airquality

head(airquality)
tail(airquality)

select(airquality,Ozone,Solar.R,Day)
select(airquality, -(Wind:Month))

filter(airquality, Month > 5, Month < 9, Day <3)
arrange(airquality, Ozone, desc(Solar.R))
by_month <- group_by(airquality, Month)
View(by_month)
airquality <- filter(airquality, !is.na(Ozone))
summarise(by_month, min(Ozone), mean(Ozone), max(Ozone))


library(tidyr)

gather(airquality, wind, Temp, -Month)

