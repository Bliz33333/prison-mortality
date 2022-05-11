selected_columns <- c(3,4,5,6,8,13, 17)
selected_colnames <- c("Name", "Current_Facility", "Gender", "Race", "Projected_Release", "Offense_Code", "Sentence", "Age")

hvd_list_stats <- list()

# for(i in 1:8)
# {
#   print(sum(is.na(hvd_list_raw[[i]]$DOB)))
# }
# 
# for(i in 9:13)
# {
#   print(sum(is.na(hvd_list_raw[[i]]$Age)))
# }
# 
# for(i in 1:13)
# {
#   print(sum(is.na(hvd_list_stats[[i]]$Age)))
# }
# 
# sum(is.na(hvd_stats$Age))

for (i in 1:length(hvd_list_raw)) {
  hvd_list_stats[[i]] <- hvd_list_raw[[i]][,selected_columns]
  
  if("Age" %in% colnames(hvd_list_raw[[i]]))
  {
    hvd_list_stats[[i]]$Age <- as.numeric(hvd_list_raw[[i]]$Age)
  } else  {
    hvd_list_stats[[i]]$Age <- as.numeric(floor((hvd_table$posix[i] - as.POSIXct(hvd_list_raw[[i]]$DOB))/365.2425))
  }
  
  hvd_list_stats[[i]]$Data_Date <- hvd_table$posix[i]
  
  colnames(hvd_list_stats[[i]]) <- c(selected_colnames, "Data_Date")
}

hvd_stats <- rbindlist(hvd_list_stats)
names(hvd_stats)<-make.names(names(hvd_stats))

facilities <- unique(hvd_stats$Current_Facility)

#Life sentences include death penalties
#this guy doesn't have a sentence LEA VELL,AARON RILEY
# life is 65
hvd_stats[is.na(hvd_stats$Name),]$Name <- "Blank"

hvd_stats <- 
  hvd_stats %>% 
  filter(Name != "LEA VELL,AARON RILEY")

descriptive_stats <- data.frame(matrix(nrow = length(facilities), ncol = 6))
rownames(descriptive_stats) <- facilities
colnames(descriptive_stats) <- c("Percent_Male", "Percent_White", "Percent_Life_Sentence", "Average_Sentence_Nonlife", "Average_Age","Percent_Violent_Crime")

for(i in facilities)
{
  descriptive_stats[i,1] <- 
    hvd_stats %>% 
    filter(Current_Facility == i) %>% 
    mutate(male = (Gender == "M")) %>% 
    select(male) %>%
    unlist() %>% 
    mean()
    
  descriptive_stats[i,2] <- 
    hvd_stats %>% 
    filter(Current_Facility == i) %>% 
    mutate(white = (Race == "W")) %>% 
    select(white) %>%
    unlist() %>% 
    mean()
  
  descriptive_stats[i,3] <- 
    hvd_stats %>% 
    filter(Current_Facility == i) %>% 
    mutate(life = (grepl(pattern = "Life", .$Sentence, fixed = T) | grepl(pattern = "LWOP", .$Sentence, fixed = T)| grepl(pattern = "Death", .$Sentence, fixed = T) | as.numeric(.$Sentence) > 65)) %>% 
    select(life) %>%
    unlist() %>% 
    mean()
  descriptive_stats[i,4] <- 
    hvd_stats %>% 
    filter(Current_Facility == i, !(grepl(pattern = "Life", .$Sentence, fixed = T) | grepl(pattern = "LWOP", .$Sentence, fixed = T)| grepl(pattern = "Death", .$Sentence, fixed = T) | as.numeric(.$Sentence) > 65)) %>% 
    select(Sentence) %>% 
    unlist() %>% 
    as.numeric() %>% 
    mean()
  descriptive_stats[i,5] <- 
    hvd_stats %>% 
    filter(Current_Facility == i) %>% 
    select(Age) %>% 
    unlist() %>% 
    mean()
  # descriptive_stats[i,6] <- 
  
  print(i)
}

descriptive_stats$Facility <- rownames(descriptive_stats)
fac_combined_raw <- fac_combined

for(i in 1:nrow(fac_combined))
{
  fac_combined$Percent_Male[i] <- descriptive_stats[descriptive_stats$Facility == fac_combined$Facility[i],1]
  fac_combined$Percent_White[i] <-  descriptive_stats[descriptive_stats$Facility == fac_combined$Facility[i],2]
  fac_combined$Percent_Life_Sentence[i] <-  descriptive_stats[descriptive_stats$Facility == fac_combined$Facility[i],3]
  fac_combined$Average_Sentence_Nonlife[i] <-  descriptive_stats[descriptive_stats$Facility == fac_combined$Facility[i],4]
  fac_combined$Average_Age[i] <-  descriptive_stats[descriptive_stats$Facility == fac_combined$Facility[i],5]
}

#-----------------------------------------------

ggplot(data = fac_combined, aes(x = Percent_Life_Sentence, y = rate, color = private)) +
  geom_point(alpha = 1) +
  xlab("Proportion of Inmates with a Life Sentence (65+ years)") +
  ylab("Relative Mortality Rate")

fac_colnames <- colnames(fac_combined)

fac_melted <- melt(fac_combined, id.vars = fac_colnames[c(1,5)], measure.vars = fac_colnames[-c(1,5)])

fac_melted$variable <-
  fac_melted$variable %>% 
  recode("Mortality" = "Number of Inmates who Died", "Mean" = "Average Inmate Population", "rate" = "Relative Mortality Rate" , "Percent_Male" = "Proportion Male", "Percent_White" = "Proportion White", "Percent_Life_Sentence" = "Proportion with Life Sentence", "Average_Sentence_Nonlife" = "Average Sentence Length", "Average_Age" = "Average Age")

fac_melted$private_text <-
  as.character(fac_melted$private)

fac_melted$private_text <-
  fac_melted$private_text %>%
  recode("TRUE" = "Private", "FALSE" = "Public")
  

fac_melted %>% 
  filter((variable != "Proportion Male") & (variable != "Relative Mortality Rate") & (variable != "Number of Inmates who Died")) %>% 
  ggplot(aes(y = value, color = private_text)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank()) +
  ylab("")
