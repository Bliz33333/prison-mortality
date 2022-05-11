facilities

length(unique(hvd_stats$Current_Facility))

open_mat <- matrix(nrow = length(facilities), ncol = nrow(hvd_table))

rownames(open_mat) <- facilities
colnames(open_mat) <- hvd_table$posix

clean_mat <- open_mat

for (i in 1:ncol(open_mat)) 
{
  

  
  temp_facilities <-hvd_list_stats[[i]]$Current_Facility %>% 
    unique()
  
  
  open_mat[temp_facilities,i] <- T
  
}

open_mat_deaths <- 
  open_mat[fac_combined$Facility,]

stats_desired <- c("Percent_Male", "Percent_White", "Percent_Life_Sentence", "Average_Sentence_Nonlife", "Average_Age", "Population")

stats_mat_list <- list()


stats_mat_list[[1]] <- clean_mat
for (i in 1:ncol(open_mat)) 
{
  temp <- 
    hvd_list_stats[[i]] %>% 
    group_by(Current_Facility, Gender) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n/sum(n))
  
  temp_m <-
    temp %>% 
    filter(Gender == "M")
  
  temp_f <-
    temp %>% 
    filter(Gender == "F")
  
  stats_mat_list[[1]][temp_f$Current_Facility,i] <- 1- temp_f$freq
  stats_mat_list[[1]][temp_m$Current_Facility,i] <- temp_m$freq
  
}

stats_mat_list[[2]] <- clean_mat
for (i in 1:ncol(open_mat)) 
{
  temp <- 
    hvd_list_stats[[i]] %>% 
    group_by(Current_Facility, Race) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n/sum(n))
  
  temp_w <-
    temp %>% 
    filter(Race == "W")
  
  print(nrow(temp_w) - length(unique(temp$Current_Facility)))
  
  stats_mat_list[[2]][temp_w$Current_Facility,i] <- temp_w$freq

}

stats_mat_list[[3]] <- clean_mat
for (i in 1:ncol(open_mat)) 
{
  for(j in 1:nrow(open_mat))
  {
    if(length(filter(hvd_list_stats[[i]], "Current_Facility" == rownames(open_mat)[j])) > 0)
    {
      stats_mat_list[[3]][j,i] <-
        hvd_list_stats[[i]] %>% 
        filter(Current_Facility == rownames(open_mat)[j]) %>% 
        mutate(life = (grepl(pattern = "Life", .$Sentence, fixed = T) | grepl(pattern = "LWOP", .$Sentence, fixed = T)| grepl(pattern = "Death", .$Sentence, fixed = T) | as.numeric(.$Sentence) > 65)) %>% 
        select(life) %>%
        unlist() %>% 
        mean(na.rm = T)
    }
  }
  print(i)
}

stats_mat_list[[4]] <- clean_mat
for (i in 1:ncol(open_mat)) 
{
  for(j in 1:nrow(open_mat))
  {
    if(length(filter(hvd_list_stats[[i]], "Current_Facility" == rownames(open_mat)[j])) > 0)
    {
      stats_mat_list[[4]][j,i] <-
        hvd_list_stats[[i]] %>% 
        filter(Current_Facility == rownames(open_mat)[j], !(grepl(pattern = "Life", .$Sentence, fixed = T) | grepl(pattern = "LWOP", .$Sentence, fixed = T)| grepl(pattern = "Death", .$Sentence, fixed = T) | as.numeric(.$Sentence) > 65)) %>% 
        select(Sentence) %>% 
        unlist() %>% 
        as.numeric() %>% 
        mean()
    }
  }
}

stats_mat_list[[5]] <- clean_mat
for (i in 1:ncol(open_mat)) 
{
  for(j in 1:nrow(open_mat))
  {
    if(length(filter(hvd_list_stats[[i]], "Current_Facility" == rownames(open_mat)[j])) > 0)
    {
      stats_mat_list[[5]][j,i] <-
        hvd_list_stats[[i]] %>% 
        filter(Current_Facility == rownames(open_mat)[j]) %>% 
        select(Age) %>% 
        unlist() %>% 
        mean()
    }
  }
}

stats_mat_list[[6]] <- clean_mat
for (i in 1:ncol(open_mat)) 
{
  for(j in 1:nrow(open_mat))
  {
    if(length(filter(hvd_list_stats[[i]], "Current_Facility" == rownames(open_mat)[j])) > 0)
    {
      stats_mat_list[[6]][j,i] <-
        hvd_list_stats[[i]] %>% 
        filter(Current_Facility == rownames(open_mat)[j]) %>% 
        nrow() %>% 
        mean()
      
      if(stats_mat_list[[6]][j,i] < 5)
      {
        stats_mat_list[[6]][j,i] <- 0
      }
    }
  }
  print(i)
}

#THIS WAS DONE MANUALLY

interp_table <- matrix(nrow = 6, ncol = 3)
interp_table[,1] <- c(2016:2021)
interp_table[1,c(2,3)] <- c(1,2)
interp_table[2,c(2,3)] <- c(3,4)
interp_table[3,c(2,3)] <- c(4,5)
interp_table[4,c(2,3)] <- c(5,6)
interp_table[5,c(2,3)] <- c(7,7)
interp_table[6,c(2,3)] <- c(11,12)

data_avail <- matrix(data = F, nrow = nrow(open_mat), ncol = nrow(interp_table))
rownames(data_avail) <- rownames(open_mat)
colnames(data_avail) <- interp_table[,1]

for(i in 1:nrow(data_avail))
{
  for(j in 1:ncol(data_avail))
  {
    if(stats_mat_list[[6]][i,interp_table[j,2]] > 0 & stats_mat_list[[6]][i,interp_table[j,3]] > 0)
    {
      data_avail[i,j] <- T
    }
  }
}

weights_table <- interp_table
weights_table[,2] <- as.numeric(hvd_table[weights_table[,2],5])
weights_table[,3] <- as.numeric(hvd_table[weights_table[,3],5])
mid_posix <- numeric(6)
mid_posix[1] <- dmy("1/6/2016") %>% as.POSIXct() %>%  as.numeric()
mid_posix[2] <- dmy("1/6/2017") %>% as.POSIXct() %>%  as.numeric()
mid_posix[3] <- dmy("1/6/2018") %>% as.POSIXct() %>%  as.numeric()
mid_posix[4] <- dmy("1/6/2019") %>% as.POSIXct() %>%  as.numeric()
mid_posix[5] <- dmy("1/6/2020") %>% as.POSIXct() %>%  as.numeric()
mid_posix[6] <- dmy("1/6/2021") %>% as.POSIXct() %>%  as.numeric()
weights_table <- cbind(weights_table, mid_posix)

approx(y = c(5,10), x = c(weights_table[1,c(2,3)]), xout = weights_table[1,4])


yearly_template <- as.data.frame(matrix(nrow = nrow(clean_mat), ncol = 6, data = NA))
rownames(yearly_template) <- rownames(clean_mat)
colnames(yearly_template) <- c(2016:2021)

yearly_stat_list <- vector("list", length = 6)

for (l in 1:length(yearly_stat_list)) 
{
  temp_frame <- stats_mat_list[[l]]
  temp_data <- yearly_template
  for(i in 1:nrow(temp_data))
  {
    for(j in 1:ncol(temp_data))
    {
      if(data_avail[i,j])
      {
        
        if(weights_table[j,3] == weights_table[j,2])
        {
          
          temp_data[i,j] <- temp_frame[i,interp_table[j,2]]
          
        } else {
          
          temp_data[i,j] <- approx(x = weights_table[j,2:3], y = (temp_frame[i,interp_table[j,2:3]]), xout = weights_table[j,4])$y
          
        }
        
      }
    }
  }
  print(l)
  yearly_stat_list[[l]] <- temp_data
}

for (i in 1:length(yearly_stat_list)) {
  yearly_stat_list[[i]]$data <- stats_desired[i]
  yearly_stat_list[[i]]$Facility <- rownames(yearly_stat_list[[i]])
  yearly_stat_list[[i]] <- as.data.table(yearly_stat_list[[i]])
}

yearly_stat_list_melted <- vector("list", length = length(yearly_stat_list))

for(i in 1:length(yearly_stat_list_melted))
{
  yearly_stat_list_melted[[i]] <- melt(yearly_stat_list[[i]], id.vars = c("data", "Facility"), variable.name = "Year", value.name = "dat_val")
}

yearly_stat_table_melted <- list.rbind(yearly_stat_list_melted)

#test <- melt(yearly_stat_list[[1]], id.vars = c("data", "Facility"), variable.name = "Year", value.name = "dat_val")
yearly_stat_table <- dcast(yearly_stat_table_melted, Facility + Year ~ data, value.var = "dat_val")
yearly_stat_table <- 
  yearly_stat_table %>% 
  filter(!is.na(Population))



yearly_stat_table <- merge(yearly_stat_table, deaths_processed, by = c("Facility", "Year"))
yearly_stat_table$Mortality_per_10000 <- yearly_stat_table$Deaths/yearly_stat_table$Population * 10000
yearly_stat_table$Private <- yearly_stat_table$Facility %in% private
yearly_stat_table$Year <- as.factor(yearly_stat_table$Year)
yearly_stat_table$Hospital <- yearly_stat_table$Facility %in% hospitals
yearly_stat_table$Sex <- "Mixed"
yearly_stat_table$Sex[yearly_stat_table$Percent_Male > .95] <- "Male"
yearly_stat_table$Sex[yearly_stat_table$Percent_Male < .05] <- "Female"

new_melted <- melt(yearly_stat_table, id.vars = c("Facility","Year","Private"))
