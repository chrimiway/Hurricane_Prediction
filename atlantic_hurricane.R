setwd("~/Desktop/Graduate/duke/MIDS/semester 1/data modeling/final project")
library(geosphere)
library(sp)
library(tidyverse)
library(mice)
library(lubridate)

# load data
col_names = c("index", "name", "length", "date", "time",
              "record_identifier", "status", "latitude", "longitude", 
              "maximum_sustained_wind", "minimum_pressure",
              "34kt_radii_northeastern", "34kt_radii_southeastern", "34kt_radii_southwestern", "34kt_radii_northwestern", 
              "50kt_radii_northeastern", "50kt_radii_southeastern", "50kt_radii_southwestern", "50kt_radii_northwestern",
              "64kt_radii_northeastern", "64kt_radii_southeastern", "64kt_radii_southwestern", "64kt_radii_northwestern")
hurricanes = read.csv("hurricanes.csv", header = FALSE, stringsAsFactors = FALSE, na.strings = c(-999, ""))
names(hurricanes) = col_names

# select hurricanes measured pressure
hurricanes_1979 = hurricanes[33125:length(hurricanes$index), ] %>%
  select("index", "length", "date", "time",
         "latitude", "longitude", 
         "record_identifier", "status",
         "maximum_sustained_wind", "minimum_pressure")

summary(hurricanes_1979)

# select hurricanes reached HU level
hurricanes_1979 = hurricanes_1979  %>%
  group_by(index) %>%
  filter(any(status == "HU"))

# find turing point
find_turning_point = function(dat) {
  #return(length(dat$status))
  for (i in seq(1, length(dat$index), 1)) {
    if (dat$status[i] == "HU") {
      return (i)
    }
  }
  return (FALSE)
}

# add turn points
hurricanes_1979_turing_points = hurricanes_1979 %>%
  group_by(index) %>%
  do(turn_index = find_turning_point(.))
hurricanes_1979_turing_points$turn_index = unlist(hurricanes_1979_turing_points$turn_index)

hurricanes_1979 = hurricanes_1979 %>%
  left_join(hurricanes_1979_turing_points)

# calculate turing direction
# use initial bearing to calculate direction
cal_direction = function(dat) {
  turing_point = dat$turn_index[1]
  if (dat$longitude[turing_point-1] == dat$longitude[turing_point] 
      && dat$latitude[turing_point-1] == dat$latitude[turing_point]) {
    return(bearingRhumb(c(dat$longitude[turing_point-1], dat$latitude[turing_point-1]),
                        c(dat$longitude[turing_point+1], dat$latitude[turing_point+1])))
  }
  b = bearingRhumb(c(dat$longitude[turing_point-1], dat$latitude[turing_point-1]),
                   c(dat$longitude[turing_point], dat$latitude[turing_point]))
  # if (b>=0 && b<45) {
  #   return("North")
  # } else if (b>=45 && b<90) {
  #   return("Northeast")
  # } else if (b>=90 && b<135) {
  #   return("East")
  # } else if (b>=135 && b<180) {
  #   return("Southeast")
  # } else if (b>=180 && b<225) {
  #   return("South")
  # } else if (b>=225 && b<270) {
  #   return("Southwest")
  # } else if (b>=270 && b<315) {
  #   return("West")
  # } else {
  #   return("Northwest")
  # }
  return(b)
}

# add turn direction
hurricanes_1979_turing_direction = hurricanes_1979 %>%
  group_by(index) %>%
  do(turn_direction = cal_direction(.))
hurricanes_1979_turing_direction$turn_direction = unlist(hurricanes_1979_turing_direction$turn_direction)

# vertical and horizontal movements
cal_zonol_movements = function(dat) {
  turing_point = dat$turn_index[1]
  return(dat$longitudep[turing_point] - dat$longitudep[turing_point - 1])
}

cal_meridional_movements = function(dat) {
  turing_point = dat$turn_index[1]
  return(dat$latitude[turing_point] - dat$latitude[turing_point - 1])
}

# CLIP
# 1.the day number: number of hour since it occurred
# 2.latitude
# 3.longitude
# 4.current meridional and zonal motion
# 5.meridional and zonal motion 12 h before forecast time
# 6.maximum wind of the cyclone.

## 1.the day number: number of hour since it occurred
time_span = function(hurricane) {
  turing_point = hurricane$turn_index[1]
  init_date = ymd_hm(hurricane$date[1]*10000+hurricane$time[1])
  turn_date = ymd_hm(hurricane$date[turing_point]*10000+hurricane$time[turing_point])
  return(as.numeric(difftime(turn_date, init_date, units = "hours")))
}

hurricanes_1979_time_span = hurricanes_1979 %>%
  group_by(index) %>%
  do(time_span = time_span(.))%>%
  ungroup()
hurricanes_1979_time_span$time_span = unlist(hurricanes_1979_time_span$time_span)

# 4.current meridional and zonal motion (Need to change with distGeo)
## A nautical mile is equal to one minute of latitude.
## A knot is one nautical mile per hour
find_motion = function(hurricane) {
  turing_point = hurricane$turn_index[1]
  
  # zonal: (6 hours)
  zonal_motion_6 = (hurricane$latitude[turing_point] - hurricane$latitude[turing_point - 1]) * 60 / 6
  # meridional
  meridional_motion_6 = (hurricane$longitude[turing_point] - hurricane$longitude[turing_point - 1]) * 60 / 6
  # zonal: (12 hours)
  zonal_motion_12 = (hurricane$latitude[turing_point-1] - hurricane$latitude[turing_point-2]) * 60 / 6
  # meridional
  meridional_motion_12 = (hurricane$longitude[turing_point-1] - hurricane$longitude[turing_point-2]) * 60 / 6
  
  return(abs(c(zonal_motion_6, meridional_motion_6, 
               zonal_motion_12, meridional_motion_12)))
}

hurricanes_1979_motion = hurricanes_1979 %>%
  group_by(index) %>%
  do(motion = find_motion(.))%>%
  mutate(zonal_motion_6 = motion[[1]], meridional_motion_6 = motion[[2]],
         zonal_motion_12 = motion[[3]], meridional_motion_12 = motion[[4]]) %>%
  ungroup() %>%
  select(-motion)


# combine to original table
hurricanes_1979_train = hurricanes_1979 %>%
  group_by(index) %>%
  slice(turn_index[[1]]-1) %>%
  left_join(hurricanes_1979_time_span) %>%
  left_join(hurricanes_1979_motion) %>%
  left_join(hurricanes_1979_turing_direction) %>%
  ungroup %>%
  select(-index, -length, -date, -time, -record_identifier, -status, -turn_index)

# EDA
plot(x = hurricanes_1979_train$latitude, y = hurricanes_1979_train$turn_direction)
plot(x = hurricanes_1979_train$longitude, y = hurricanes_1979_train$turn_direction)
plot(x = hurricanes_1979_train$maximum_sustained_wind, y = hurricanes_1979_train$turn_direction)
plot(x = hurricanes_1979_train$minimum_pressure, y = hurricanes_1979_train$turn_direction)
plot(x = hurricanes_1979_train$time_span, y = hurricanes_1979_train$turn_direction)
plot(x = hurricanes_1979_train$zonal_motion_6, y = hurricanes_1979_train$turn_direction)
plot(x = hurricanes_1979_train$meridional_motion_6, y = hurricanes_1979_train$turn_direction)
plot(x = hurricanes_1979_train$zonal_motion_12, y = hurricanes_1979_train$turn_direction)
plot(x = hurricanes_1979_train$meridional_motion_12, y = hurricanes_1979_train$turn_direction)

# base model:
reg = lm(turn_direction~maximum_sustained_wind + minimum_pressure +
           latitude + longitude + time_span +
           zonal_motion_6 + meridional_motion_6 +
           zonal_motion_12 + meridional_motion_12, data = hurricanes_1979_train)
summary(reg)

## 1.contour graph and sample some possible points from the final distribution
## 2.水平与垂直位移

## model time for Western North Pacific: 
### instrumented aircraft and trained flight crews(1946)
### weather satellite(mid-1960)