#### compare test results 

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)



#### Compare performance measures ####
r1 <- read_csv("./glm_full_report.csv")
r2 <- read_csv("./rpart_full_report.csv")


df <- rbind(r1, r2)


df %>% filter(model.type == "rpart" | model.type == "glm" , 
              name %in% c("test1", "test2", "test3")) %>% 
  gather(key = "measure", value = "value", 4:8) %>% 
  ggplot(aes(x = measure, y=value, fill=name)) + 
  geom_bar(stat="identity", position = "dodge", colour="black") + 
  facet_wrap(~ model.type) + 
  theme_minimal() + 
  labs(title = "Classification Performance Comparison",
       subtitle = "Logisitc and Dicision Tree ", 
       
       x = 'Performance Measure', 
       y = 'Value', 
       fill = "Test Case"
  ) 



#### compare variable importance ####

v1 <- read_csv("./glm_varimp.csv")
v2 <- read_csv("./rpart_varimp.csv")

vdf <- df <- rbind(v1, v2)


varImpRankPlot <- function(df, m, t){
  df %>% filter(model.type == m, test==t) %>% 
    arrange(desc(Overall)) %>% 
    head(15) %>%
    ggplot(aes(x=reorder(varName, Overall), y=Overall, fill=-Overall)) + 
    geom_bar(stat="identity") + 
    coord_flip() +
    theme( legend.position = "none", 
           axis.ticks.x = element_blank(), 
           axis.text.x = element_blank()) + 
    labs( title = "Variable Importance", 
          subtitle = paste(m, "-", t), 
           x = "Variable")
    
}


vdf %>% varImpRankPlot("rpart", "test1")
vdf %>% varImpRankPlot("glm", "test1")

vdf %>% varImpRankPlot("rpart", "test2")
vdf %>% varImpRankPlot("glm", "test2")

vdf %>% varImpRankPlot("rpart", "test3")
vdf %>% varImpRankPlot("glm", "test3")

