setwd("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project2")
getwd()

# LIBRARIES========================================================================================================================
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)

# READ FILES=======================================================================================================================
cta_station <- read.table(file = "C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project2/Data/cta_data.tsv", sep = "\t", header = TRUE, quote = "\"")
head(cta_station)
lat_long <- read.table(file = "C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project2/Data/lat_long.tsv", sep = "\t", header = TRUE, quote = "\"")
head(lat_long)

# STATS============================================================================================================================

# Basic stats
str(cta_station)
summary(cta_station)
dim(cta_station)

str(lat_long)
summary(lat_long)
dim(lat_long)

# FEATURE ENGG=====================================================================================================================

# Rename columns
names(cta_station)[1] <- 'station_id'
names(lat_long)[6] <- 'station_id'

# Extract line data and store in column 'line'
lat_long$line <- str_extract(lat_long$STATION_DESCRIPTIVE_NAME, "\\(.*\\)")
lat_long$line <- str_remove_all(lat_long$line, "[\\(\\)]")

# Data Pre-Processing
# date fix
cta_station$date <- as.Date(cta_station$date, '%m/%d/%Y')
str(cta_station)
# week_day, m, d, Y extraction
cta_station$year <- as.numeric(format(cta_station$date, format='%Y'))
cta_station$month <- as.numeric(format(cta_station$date, format='%m'))
cta_station$day <- as.numeric(format(cta_station$date, format='%d'))
cta_station$week_day <- wday(cta_station$date, label=TRUE)

# Add Month name
name_of_month <- function(df){
  df$month_name[df$month==1] <- 'Jan'
  df$month_name[df$month==2] <- 'Feb'
  df$month_name[df$month==3] <- 'Mar'
  df$month_name[df$month==4] <- 'Apr'
  df$month_name[df$month==5] <- 'May'
  df$month_name[df$month==6] <- 'Jun'
  df$month_name[df$month==7] <- 'Jul'
  df$month_name[df$month==8] <- 'Aug'
  df$month_name[df$month==9] <- 'Sep'
  df$month_name[df$month==10] <- 'Oct'
  df$month_name[df$month==11] <- 'Nov'
  df$month_name[df$month==12] <- 'Dec'
  
  return(df)
  
}
cta_station <- name_of_month(cta_station)

# CREATE FINAL DATA=================================================================================================================

# Get subset of lat_long data
loc_data = subset(lat_long, select = c('station_id', 'Location', 'line'))
loc_data <- loc_data %>% distinct(station_id, line, .keep_all = TRUE)

# Merge Data
final_data <- left_join(cta_station, loc_data, on="station_id")
final_data

str(final_data)

# Fill Missing Data
final_data$Location[final_data$stationname == 'Randolph/Wabash'] = "(41.884431, -87.626149)"
final_data$line[final_data$stationname == 'Randolph/Wabash'] = "Green, Orange, Pink, Purple & Brown Lines"

final_data$Location[final_data$stationname == 'Madison/Wabash'] = "(41.882023, -87.626098)"
final_data$line[final_data$stationname == 'Madison/Wabash'] = "Brown, Green, Orange, Pink & Purple Lines"

final_data$Location[final_data$stationname == 'Washington/State'] = "(41.8837, -87.6278)"
final_data$line[final_data$stationname == 'Washington/State'] = "Red Line"

final_data$Location[final_data$stationname == 'Homan'] = "(41.884914, -87.711327)"
final_data$line[final_data$stationname == 'Homan'] = "Green Line"

# Change data regarding Pink Line
final_data$line[final_data$stationname == "54th/Cermak" & final_data$date < "2006-06-25"] <- "Blue Line"

# Check for NULL
ok <- complete.cases(final_data)
dim(final_data[ok, ]) == dim(final_data)

str(final_data)
# write.table(final_data, file='final_data.tsv', quote=FALSE, sep='\t')

# Export data
no_of_chunks <- 30
f <- ceiling(1:nrow(final_data) / nrow(final_data) * 25)
res <- split(final_data, f)
map2(res, paste0("part_", names(res), ".csv"), write.csv)

# ALITER============================================================================================================================
# library(qdapRegex)
# lat_long$line = rm_between(lat_long$STATION_DESCRIPTIVE_NAME, '(', ')', extract=TRUE)
# ==================================================================================================================================
final_data %>%
  mutate(sign = case_when(
    rides < 0 ~ "Negative",
    rides == 0 ~ "Zero",
    rides > 0 ~ "Positive"
  ))

fig <- ggplot(data = final_data,
       aes(x = as.character(stationname), y = rides))+
  geom_bar(stat = "identity")+
  coord_flip()

print(fig)

date1 <- "2021-07-15"
date2 <- "2021-06-12"
date_df_1 <- subset(final_data, final_data$date == date1)
date_df_2 <- subset(final_data, final_data$date == date2)

difference_df <- inner_join(x=date_df_1, y=date_df_2, by="station_id")
difference_df <- subset(difference_df, select = c("station_id", "stationname.x", "date.x","date.y", "rides.x","rides.y", "line.x", "line_color.x")) 
difference_df$rides <- difference_df$rides.x - difference_df$rides.y

difference_df <- difference_df %>%
  rename( stationname = stationname.x,
          date_1 = date.x,
          date_2 = date.y,
          line = line.x)


print(difference_df)


difference_df %>%
  mutate(sign = case_when(
    rides < 0 ~ "Negative",
    rides > -1 ~ "Positive"
  ))

fig <- ggplot(data = difference_df,
              aes(x = difference_df$stationname, y = difference_df$rides, text = line))+
  geom_bar(stat = "identity", aes(fill=rides>0))+ 
  labs(x = "Ride Difference", y = "Stations") + scale_fill_discrete(name = "Ridership Change", ) +
  coord_flip()

print(fig)
str(difference_df)
