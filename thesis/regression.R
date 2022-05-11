model1 <- lm(data = yearly_stat_table, Mortality_per_10000 ~ Private + Population + Percent_White + Percent_Male + Percent_Life_Sentence + Average_Sentence_Nonlife + Average_Age + Year + Hospital)
summary(model1)


model2 <- lm(data = yearly_stat_table, Mortality_per_10000 ~ Private + Population + Percent_White + Sex + Percent_Life_Sentence + Average_Sentence_Nonlife + Average_Age + Year + Hospital)
summary(model2)

model3 <- lm(data = yearly_stat_table, Mortality_per_10000 ~ Population + Percent_White + Sex + Percent_Life_Sentence + Average_Sentence_Nonlife + Average_Age + Year + Hospital)
summary(model3)


my_table <- yearly_stat_table

my_table <- my_table[,-c("Facility", "Year")]

colnames(my_table) -> temp
temp <- temp[-9]

my_chart <- as.data.table(matrix(nrow = 2, ncol = length(temp) + 1, data = -1))
my_chart[,1] <- c("Public", "Private")
for (i in 1:length(temp)) 
{
  my_chart[1,i+1] <- 
    my_table %>% 
    filter(!Private) %>% 
    select(temp[i]) %>% 
    unlist() %>% 
    mean()
  
  my_chart[2,i+1] <- 
    my_table %>% 
    filter(Private) %>% 
    select(temp[i]) %>% 
    unlist() %>% 
    mean()
}
colnames(my_chart)[-1] <- temp
colnames(my_chart)[1] <- "Public or Private"
my_chart <- t(my_chart)

write.csv(my_chart, "chart.csv")

yearly_stat_table_melted$data <-
  yearly_stat_table_melted$data %>% 
  recode("Mortality_per_10000" = "Mortality per 10,000 Inmates", "Population" = "Average Inmate Population" , "Percent_Male" = "Proportion Male", "Percent_White" = "Proportion White", "Percent_Life_Sentence" = "Proportion with Life Sentence", "Average_Sentence_Nonlife" = "Average Sentence Length", "Average_Age" = "Average Age")

yearly_stat_table_melted$Private <- yearly_stat_table_melted$Facility %in% private

yearly_stat_table_melted$private_text <-
  as.character(yearly_stat_table_melted$Private)

yearly_stat_table_melted <- yearly_stat_table_melted[!is.na(yearly_stat_table_melted$data),]

yearly_stat_table_melted$private_text <-
  yearly_stat_table_melted$private_text %>%
  recode("TRUE" = "Private", "FALSE" = "Public")


yearly_stat_table_melted %>% 
  filter(data != "Proportion Male") %>% 
  ggplot(aes(y = dat_val, color = private_text)) +
  geom_boxplot() +
  facet_wrap(~data, scales = "free_y") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank()) +
  ylab("")
