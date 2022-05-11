library(pacman)
p_load(data.table, lubridate, tidyverse, rlist)
#p_load(rio, readr, readxl, data.table, lubridate, sets, tidyverse)

#---------------------------------

hvd_paths <- 
  list.files(path = "./data/hvd", full.names = T)


sheet_names <- 
  lapply(hvd_paths, excel_sheets) %>% 
  unlist()

dates <- 
  (strsplit(sheet_names, " ", fixed = T) %>% 
  unlist())[c(F,F,F,F,T)] 
  
dupes <- duplicated(dates)

hvd_table <- 
  cbind(hvd_paths[!dupes], dates[!dupes]) %>% 
  data.frame(stringsAsFactors = F)

colnames(hvd_table) <- c("path", "raw_date")

dates_chars <- 
  (strsplit(hvd_table$raw_date, split = "", fixed = T) %>% 
  unlist())

temp_nums <- numeric(length(dates_chars)/2)
for(i in 1:(length(dates_chars)/2))
{
  temp_nums[i] <- paste0(dates_chars[((2*i)-1)],dates_chars[(2*i)])
}
months <- temp_nums[c(T,F)]
years <- temp_nums[c(F,T)]

hvd_table$month <- months
hvd_table$year <- years

hvd_table <-
  hvd_table %>% 
  arrange(year, month)

hvd_list <- vector("list", nrow(hvd_table))

hvd_list <- lapply(hvd_table$path, read_excel)

#save(hvd_list, file = "hvd_list_file")
#save(hvd_table, file = "hvd_table_file")

#--------------------------------------------------
load(file = "hvd_list_file")
hvd_list_raw <- hvd_list
load(file = "hvd_table_file")
tji_raw <- read_csv("tji_custodial-deaths.csv")

tji_temp <- tji_raw
tji_temp <- tji_temp[tji_temp$death_date > as.POSIXct("2016-5-1"),]
table(tji_temp$type_of_custody)
table(tji_temp$specific_type_of_custody_facility)
tji_temp <- tji_temp[tji_temp$type_of_custody != "POLICE CUSTODY (PRE-BOOKING)",] 
tji_temp <- tji_temp[tji_temp$specific_type_of_custody_facility != "CUSTODY OF LAW ENFORCEMENT PERSONNEL AFTER ARREST",]
tji_temp <- tji_temp[tji_temp$specific_type_of_custody_facility != "CUSTODY OF LAW ENFORCEMENT PERSONNEL DURING/FLEEING ARREST",]
tji_temp <- tji_temp[tji_temp$specific_type_of_custody_facility != "OTHER",]

#import?
tji_temp <- tji_temp[tji_temp$type_of_custody == "PRISON",]

tji <- tji_temp

#tji <- tji_raw[,c(6,7,8,9,10,11,13,14,15)]
#tji <- tji[tji$death_date > as.POSIXct("2016-5-1"),]

col_order <- c(1,2,3,5,6,13,14,4)

for (i in 1:length(hvd_list)) 
{
  hvd_list[[i]] <- hvd_list[[i]][,col_order]
  
  #THIS IS DANGEROUS
  
  colnames(hvd_list[[i]]) <- 
    c("SID Number", "TDCJ_Number", "Name", "Gender", "Race", "Offense_Code", "Offense", "Current_Facility", "data_date")
}

for (i in 1:length(hvd_list)) 
{
  hvd_list[[i]]$data_date <- hvd_table$raw_date[i]
  hvd_list[[i]] <- as.data.frame(hvd_list[[i]],stringsAsFactors = F)
}

for (i in 1:length(hvd_list_raw)) 
{
  val <- hvd_list_raw[[i]]
  
  if(sum(colnames(val) == 'DOB') != 0)
  {
    hvd_list[[i]]$dob_low <- (val$DOB - days(1))
    hvd_list[[i]]$dob_high <- (val$DOB + days(1))
  } else {
    
    hvd_list[[i]]$dob_low <- -1*years(hvd_list_raw[[i]]$Age) + as.POSIXct(ymd(paste(hvd_table$year[i], hvd_table$month[i], "01", sep = "-"))) - days(1)
    
    hvd_list[[i]]$dob_high <- -1*years(hvd_list_raw[[i]]$Age) + as.POSIXct(ymd(paste(hvd_table$year[i], hvd_table$month[i], "01", sep = "-"))) + years(1) + days(32)
  }

}

hvd_combined <- rbindlist(hvd_list)




names <- hvd_combined$Name
split <- str_split_fixed(names, ",", n = Inf)
last_names <- split[,1]
split <- split[,2]
split <- str_split(split, " ", n = Inf)
for(i in 1:length(split))
{
  temp <- !((split[[i]] == "JR") | (split[[i]] == "JR.") | (split[[i]] == "SR") | (split[[i]] == "SR.") | (split[[i]] == "I") | (split[[i]] == "II") | (split[[i]] == "III") | (split[[i]] == "IV") | (split[[i]] == "V") | (split[[i]] == "MR.") | (split[[i]] == "MR") | (split[[i]] == "MRS.") | (split[[i]] == "MRS"))
  split[[i]] <- paste(split[[i]][temp], sep = " ", collapse = " ")
}
prec_names <- unlist(split)

first_names <- c(t(str_split_fixed(prec_names, " ", 10)))[c(T,F,F,F,F,F,F,F,F,F)]

reorder_names <- paste(prec_names, last_names)
tji_custom_name <- paste(tji$name_first, tji$name_last)

tji_custom_full_name <- character(length(tji_custom_name))

for(i in 1:length(tji_custom_name))
{
  if(is.na(tji$name_middle[i]))
  {
    tji_custom_full_name[i] <- paste(tji$name_first[i], tji$name_last[i])
  } else {
    tji_custom_full_name[i] <- paste(tji$name_first[i], tji$name_middle[i], tji$name_last[i])
  }
}


hvd_f_l <- paste(first_names, last_names)

temp <-  strsplit(tji$name_full, split = " ", fixed = T)
for (i in 1:length(temp)) {
  temp[[i]] <- (temp[[i]][!((temp[[i]] == "JR") | (temp[[i]] == "JR.") | (temp[[i]] == "SR") | (temp[[i]] == "SR.") | (temp[[i]] == "I") | (temp[[i]] == "II") | (temp[[i]] == "III") | (temp[[i]] == "IV") | (temp[[i]] == "V") | (temp[[i]] == "MR.") | (temp[[i]] == "MR") | (temp[[i]] == "MRS.") | (temp[[i]] == "MRS"))])
  
  temp[[i]] <- c(temp[[i]][1], temp[[i]][length(temp[[i]])])
  temp[[i]] <- paste(temp[[i]], sep = " ", collapse = " ")
  
}

tji_f_l <- unlist(temp)


sum(tji_f_l %in% hvd_f_l)
no_match <- tji$name_full[!(tji_f_l %in% hvd_f_l)]
View(no_match)


names_matches_list <- lapply(1:nrow(hvd_combined), function(x) which(tji_f_l == hvd_f_l[x]))
# for(i in 1:nrow(hvd_combined))
# {
#   names_matches_list[[i]] <- which(tji_f_l == hvd_f_l[i])
# }

dob_matches_list <- lapply(1:nrow(hvd_combined), function(x) which((tji$date_of_birth >= hvd_combined$dob_low[x]) & (tji$date_of_birth <= hvd_combined$dob_high[x])))
# for(i in 1:nrow(hvd_combined))
# {
#   dob_matches_list[[i]] <- which((tji$date_of_birth >= hvd_combined$dob_low[i]) & (tji$date_of_birth <= hvd_combined$dob_high[i]))
# }

matches_list <- lapply(1:nrow(hvd_combined), function(x) intersect(names_matches_list[[x]], dob_matches_list[[x]]))
# for(i in 1:nrow(hvd_combined))
# {
#   matches_list[[i]] <- intersect(names_matches_list[[i]], dob_matches_list[[i]])
# }

#save(matches_list, file = "matches_list_file")
matches_lengths <- unlist(lapply(matches_list, length))
matches_vec <- unlist(matches_list)
matches_unique <- unique(matches_vec)
plot((1:(length(matches_lengths)/100)*100), cumsum(matches_lengths)[(1:(length(matches_lengths)/100)*100)])


table(unlist(lapply(matches_list, length)))

matches_list_na <- matches_list
matches_list_na[(unlist(lapply(matches_list, length)) == 0)] <- NA

hvd_combined$match_index <- unlist(matches_list_na)
hvd_matched_subset <- hvd_combined[(unlist(lapply(matches_list, length)) == 1),]
 
sid_crossref <- vector("list", max(hvd_matched_subset$match_index))
tdcj_crossref <- vector("list", max(hvd_matched_subset$match_index))
for (i in 1:nrow(hvd_matched_subset)) 
{
  temp_match <- hvd_matched_subset$match_index[i]
  temp_sid <- hvd_matched_subset$`SID Number`[i]
  temp_tdcj <- hvd_matched_subset$TDCJ_Number[i]
  sid_crossref[[temp_match]] <- c(sid_crossref[[temp_match]], temp_sid)
  tdcj_crossref[[temp_match]] <- c(tdcj_crossref[[temp_match]], temp_tdcj)
  
  # print(i)
}

sid_crossref <- lapply(sid_crossref, unique)
tdcj_crossref <- lapply(tdcj_crossref, unique)

sid_cr_len <- unlist(lapply(sid_crossref, length))
tdcj_cr_len <- unlist(lapply(tdcj_crossref, length))

errors <- hvd_matched_subset[hvd_matched_subset$match_index %in% which(sid_cr_len > 1),]

# dump everything with more than one match and hope it works out

hvd_matched_subset_filtered <- hvd_matched_subset[!(hvd_matched_subset$match_index %in% which(sid_cr_len > 1)),]


hospitals <- c("Hospital Galveston", "Young", "East Texas Treatment Facility", "West Texas Hospital")

pop_table <- as.data.frame(table(hvd_combined$Current_Facility, hvd_combined$data_date), stringsAsFactors = F)
colnames(pop_table) <- c("Facility", "Date", "Population")
pop_table$Population <- as.numeric(pop_table$Population)

hvd_table$posix <- as.POSIXct(ymd(paste(hvd_table$year, hvd_table$month, "01", sep = "-"))) + days(1)

for(i in 1:nrow(pop_table))
{
  pop_table$posix[i] <- hvd_table$posix[hvd_table$raw_date == pop_table$Date[i]]
}

ggplot(data = pop_table[pop_table$Facility == "Wynne",], mapping = aes(x = posix, y= Population)) +
  geom_line()

mean_pop <- summarise(group_by(pop_table, Facility), Mean = mean(Population, na.rm = T))

mean_pop_old <- mean_pop

for (i in 1:nrow(mean_pop)){
  
  mean_pop$Mean[i] <-
    (
      sum(pop_table[(pop_table$Facility == mean_pop$Facility[i]) & (pop_table$Population > 0),]$Population)
    ) /
    (
      length(pop_table[(pop_table$Facility == mean_pop$Facility[i]) & (pop_table$Population > 0),]$Population)
    )
  
}


for(i in 1:nrow(hvd_matched_subset_filtered))
{
  hvd_matched_subset_filtered$posix[i] <- hvd_table$posix[hvd_table$raw_date == hvd_matched_subset_filtered$data_date[i]]
}





death_table <- hvd_matched_subset_filtered[,c(8,12,13)]
for(i in 1:nrow(death_table))
{
  death_table$death_year[i] <- 
    (tji$death_date[death_table$match_index[i]] %>% 
    as.character() %>% 
    strsplit(split = "-") %>% 
    unlist())[1]
}

index_to_location <- data.table(sort(unique(death_table$match_index)))
colnames(index_to_location) <- "Index"

for(i in 1:nrow(index_to_location))
{
  index_to_location$death_year[i] <- unique(death_table[death_table$match_index == index_to_location$Index[i],death_year])
}






for(i in 1:nrow(index_to_location))
{
  temp_index <- index_to_location$Index[i]
  
  temp_table <- 
    death_table %>% 
    filter(match_index == temp_index)
  
  temp_table <- 
    temp_table %>% 
    arrange(posix)
  
  for(j in nrow(temp_table):1)
  {
    
    
    if((j == 1))
    {
      
      index_to_location$Location[i] <- temp_table$Current_Facility[j]
      
    } else {
      
      if(!(temp_table$Current_Facility[j] %in% hospitals))
      {
        index_to_location$Location[i] <- temp_table$Current_Facility[j]
        break()
      }
      
    }
    
  }
}

deaths_processed <- as.data.table(table(index_to_location[,-1]))
colnames(deaths_processed) <- c("Facility", "Year", "Deaths")


#-----------------




first_facilities <- character(length = max(hvd_matched_subset_filtered$match_index))
first_facilities <- rep(NA,length(first_facilities))
for(i in 1:max(hvd_matched_subset_filtered$match_index))
{
  temp_mat <- hvd_matched_subset_filtered[hvd_matched_subset_filtered$match_index == i,]
  
  if(nrow(temp_mat) > 0)
  {
    first_facilities[i] <- temp_mat$Current_Facility[temp_mat$posix == min(temp_mat$posix)]
  }
}

first_facilities <- as.data.frame(table(first_facilities), stringsAsFactors = F)
colnames(first_facilities) <- c("Facility", "Mortality")


last_facilities <- character(length = max(hvd_matched_subset_filtered$match_index))
last_facilities <- rep(NA,length(last_facilities))
for(i in 1:max(hvd_matched_subset_filtered$match_index))
{
  temp_mat <- hvd_matched_subset_filtered[hvd_matched_subset_filtered$match_index == i,]
  
  if(nrow(temp_mat) > 0)
  {
    last_facilities[i] <- temp_mat$Current_Facility[temp_mat$posix == max(temp_mat$posix)]
  }
}

last_facilities <- as.data.frame(table(last_facilities), stringsAsFactors = F)
colnames(last_facilities) <- c("Facility", "Mortality")


fac_combined <- merge(first_facilities, mean_pop, by = "Facility")
fac_combined$rate <- fac_combined$Mortality/fac_combined$Mean

private <- c("Bell", "Bradshaw", "Bridgeport", "Coleman", "Diboll", "East Texas Treatment Facility", "Estes", "Kyle", "Lindsey", "Willacy County", "B. Moore")
fac_combined$private <- F
fac_combined$private[fac_combined$Facility %in% private] <- T
