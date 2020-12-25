#Kaggle Survey Visualization


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("reshape2", "dplyr", "ggplot2" , "RColorBrewer", "caret", "ggpubr")
ipak(packages)

fp = "C:/SA_P/Kaggle/UserSurveyCompetition"
dat <- read.csv(file.path(fp, "kaggle_survey_2020_responses.csv"))
dat<- dat[2:20037,]

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
dat <- as.data.frame(apply(X= dat, MARGIN=2,FUN='trim'))
dat1<- select(dat, Q1, Q2, Q3, Q4, Q5, Q6, Q8, Q11, Q13, Q15, Q20, Q21, Q22, Q30)

dat1<- (dat1
        %>% rename(Age = Q1, gender = Q2, country = Q3, highest.edu = Q4, current.role = Q5, exp.year = Q6)
        %>% mutate(highest.edu = substr(highest.edu, 1,4),
                   highest.edu = ifelse(highest.edu =="Doct", "Doctoral",
                                        ifelse(highest.edu =="Mast", "Masters",
                                           ifelse(highest.edu =="Bach", "Bachelors",
                                             ifelse(highest.edu =="Prof","professional",
                                                    ifelse(highest.edu =="Some", "some_college",
                                                           "NoAnswer")))))
              )
     
)

dat2<- select(dat, -Time.from.Start.to.Finish..seconds.,-Q1, -Q2, -Q3, -Q4, -Q5,
              -Q6, -Q8, -Q11, -Q13, -Q15, -Q20, -Q21, -Q22, -Q30)

#Convert to 0 or 1 for binary variables
dummies_model <- dummyVars("~.", data = dat2, fullRank = T)
dat2 <- data.frame(predict(dummies_model, newdata = dat2))
dat2 <- cbind(dat1, dat2)

##################################
#Summary by country
##################################
country<- (dat2
           %>% group_by(country)
           %>% summarize(freq = n())
           %>% mutate(country = ifelse(country == "United States of America","USA",
                                       ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "UK",
                                              ifelse(country== "Iran, Islamic Republic of...", "Iran", country))))
)

top10<- (country
         %>% filter(country != "Other")
         %>% arrange(desc(freq))
         %>% filter(row_number() <= 10)
           )

c <- unique(top10$country)

for(i in 1:length(c)){
  dat.country = filter(dat2, country == c[i])
  num.dat <-  dat.country[, sapply(dat2, class) %in% c("numeric", "integer")]
  freq <- data.frame(apply(num.dat, 2, function(x) sum(x)/nrow(num.dat)))
  freq <- add_rownames(freq, "var_name")
  cnt_name <- rep(c[i], nrow(freq))
  freq <- cbind(cnt_name, freq)
  names(freq) <- c("country","var_name", "Percent")
  
  if(exists('freq.all')){
    freq.all <- rbind(freq.all,freq)
  } else {
    freq.all <- freq
  }
  
}

######################################
#More than one Programming Language
######################################
prog_by_edu<- (dat2
         %>% mutate(prog_all = (Q7_Part_1Python+Q7_Part_2R+Q7_Part_3SQL+Q7_Part_4C+
                                   Q7_Part_4C+Q7_Part_5C..+Q7_Part_8Julia+Q7_Part_8Julia+
                                   Q7_Part_9Swift+Q7_Part_10Bash+Q7_Part_11MATLAB+Q7_Part_12None+Q7_OTHEROther),
                    prog_1 = ifelse(prog_all == 1, 1, 0),
                    prog_2 = ifelse(prog_all == 2, 1, 0),
                    prog_3 = ifelse(prog_all == 3, 1, 0),
                    prog_4 = ifelse(prog_all == 4, 1, 0),
                    prog_5 = ifelse(prog_all == 5, 1, 0))
         %>% group_by(highest.edu)
         %>% summarize(prog_1 = sum(prog_1),
                       prog_2 = sum(prog_2),
                       prog_3 = sum(prog_3),
                       prog_4 = sum(prog_4),
                       prog_5 = sum(prog_5))
         
         )

#Stacked BarPlot 
prog_by_edu<- melt(prog_by_edu, id = c("highest.edu"))
prog_by_edu$Num <- substr(prog_by_edu$variable, 6,7)

ggplot(prog_by_edu, aes(fill=num, y=value, x=highest.edu)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Highest Education of Survey Participants",y="Number of Survey Participants")+
  ggtitle("Number of Programming Language Used By Education Level") +
  theme(legend.title = element_blank(),
        legend.position = c(0.95, 0.6),
        legend.text = element_text(size = 10))

#Number of Programming Language Used in top 10 countries
prog_by_country<- (dat2
               %>% mutate(prog_all = (Q7_Part_1Python+Q7_Part_2R+Q7_Part_3SQL+Q7_Part_4C+
                                        Q7_Part_4C+Q7_Part_5C..+Q7_Part_8Julia+Q7_Part_8Julia+
                                        Q7_Part_9Swift+Q7_Part_10Bash+Q7_Part_11MATLAB+Q7_Part_12None+Q7_OTHEROther),
                          prog_1 = ifelse(prog_all == 1, 1, 0),
                          prog_2 = ifelse(prog_all == 2, 1, 0),
                          prog_3 = ifelse(prog_all == 3, 1, 0),
                          prog_4 = ifelse(prog_all == 4, 1, 0),
                          prog_5 = ifelse(prog_all > 4, 1, 0))
               %>% group_by(country)
               %>% summarize(prog_1 = sum(prog_1),
                             prog_2 = sum(prog_2),
                             prog_3 = sum(prog_3),
                             prog_4 = sum(prog_4),
                             prog_5 = sum(prog_5))
               %>% filter(country %in% c)
               %>%mutate(country = ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "UK",
                                          ifelse(country == "United States of America", "USA", country)))
               
)

#Plot 
prog_by_country<- melt(prog_by_country, id = c("country"))
prog_by_country$Num <- substr(prog_by_country$variable, 6,7)

ggplot(prog_by_country, aes(fill=Num, y=value, x=country)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Top 10 Country of Survey Participants",y="Number of Survey Participants")+
  ggtitle("Number of Programming Language Used By Country") +
  theme(#legend.title = element_blank(),
        #legend.position = c(0.95, 0.75),
        legend.text = element_text(size = 10))


current_role <- dat2%>% group_by(current.role,highest.edu)%>% summarize(freq = n())

summary(dat2)

####################################
#Participants by country
####################################
country<- (dat2
           %>% group_by(country)
           %>% summarize(freq = n())
           %>% mutate(country = ifelse(country == "United States of America","USA",
                                       ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "UK",
                                              ifelse(country== "Iran, Islamic Republic of...", "Iran", country))))
)



#Mapping the countries
country.map <- country
names(country.map)<- c("region", "freq")
WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = country.map, map=WorldData,
           aes(fill=freq, map_id=region),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Kaggle Participants by country", x="", y="") +
  theme_bw()


####################################
#Summary by data columns
####################################

num.dat <- dat2[, sapply(dat2, class) %in% c("numeric", "integer")]
freq <- data.frame(apply(num.dat, 2, function(x) sum(x)/nrow(num.dat)))
freq <- add_rownames(freq, "var_name")
names(freq) <- c("var_name", "Percent")

#Programming Language use
prog.lang<- freq[1:13,]
prog_lang<- c("Python", "R", "SQL", "C", "C++","Java", "JavaScript", "Julia",
                     "Swift", "Bash", "Matlab", "None", "Other")
prog.lang<- cbind(prog_lang, prog.lang)

#Donut chart
prog.lang <- prog.lang %>%
  arrange(desc(prog_lang)) %>%
  mutate(lab.ypos = cumsum(Percent) - 0.5*Percent)


mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#D8BFD8","#4169E1",
            "#808000", "#800080", "#008080", "#9ACD32", "#DAA520", "#32CD32", "#9370DB")
ggplot(prog.lang, aes(x = 2, y = Percent, fill = prog_lang)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(Percent*100,0), "%")), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)


#Programming Platform use
prog.platform <- freq[14:25,]
platform_name<- c("Jupyter", "Rstudio", "Visualstudio", "VScode", "Pycharm", "Spyder", 
                         "Notepadplus", "Sublime_Text", "Vim_Emacs", "Matlab", "None",  "Other")   
prog.platform<- cbind(platform_name,prog.platform)

#Hosted Notebook use
hosted.notebook<- freq[26:39,]
hosted.notebook_name<- c("Kaggle", "Colab", "Azure", "Gradient", "Binder", "Codeclean",
                           "IBMstudio", "Amazonsage", "Amazonemr", "Googlecloud_ai", "Googlecloud_datalab",
                           "DataBrick", "None", "Other")
hosted.notebook<- cbind(hosted.notebook_name, hosted.notebook)

#Visualization library use
viz.lib<- freq[44:55,]
viz.lib_name<- c("Matplotlib", "Seaborn", "Plotly", "ggplot", "Shiny", "D3js", "Altair", 
                  "Bokeh","Geoplotlib", "Leaflet", "None", "Other")
viz.lib<- cbind(viz.lib_name, viz.lib)

#Donut chart
viz.lib <- viz.lib %>%
  arrange(desc(viz.lib_name)) %>%
  mutate(lab.ypos = cumsum(Percent) - 0.5*Percent)


mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#D8BFD8","#4169E1",
            "#808000", "#800080", "#008080", "#9ACD32", "#DAA520", "#32CD32", "#9370DB")
ggplot(viz.lib, aes(x = 2, y = Percent, fill =viz.lib_name)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(Percent*100,0), "%")), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+ xlim(0.5, 2.5)+
  ggtitle("Visualization Library Use")


####################################################
#Gender Analysis
####################################################

## Gender by country
gender_by_country<- (dat2
                     
                     %>% group_by(country, gender)
                     %>% summarize(participants = n())
                     %>% filter(country %in% c & gender %in% c("Man", "Woman"))
                     %>%mutate(country = ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "UK",
                                                ifelse(country == "United States of America", "USA", country)))
                     %>% left_join(., country)
                     %>% mutate(percent = participants/freq)
                     %>% rename(Gender = gender)
)

ggplot(gender_by_country, aes(fill=Gender, y=percent, x=reorder(country, -percent))) + 
  geom_bar(position="dodge", stat="identity")+
  labs(x="Top 10 Country of Survey Participants",y="Percent of Survey Participants")+
  ggtitle("Participants By Gender in Top 10 Country") +
  theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10))


#Gender by Education level
gender_by_edu<- (dat2
                 %>% group_by(highest.edu, gender)
                 %>% summarize(participants = n())
                 %>% filter(gender %in% c("Man", "Woman")))

gender_by_role<- (dat2
                 %>% group_by(gender, current.role)
                 %>% summarize(participants = n())
                 %>% filter(gender %in% c("Man", "Woman")))

#Stacked bar with two panel
p1<- ggplot(gender_by_edu, aes(fill=gender, y=participants, x=reorder(highest.edu, -participants))) + 
     geom_bar(position="stack", stat="identity")+
     labs(x="Number of Participants",y="Highest Education Level")+
     ggtitle("Education Level by Gender") +
     theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10))

p2<-  ggplot(gender_by_role, aes(fill=gender, y=participants, x=reorder(current.role, -participants))) + 
      geom_bar(position="stack", stat="identity")+
      labs(x="Number of Participants",y="Current Role")+
      ggtitle("Current Role by Gender") +
      theme(#legend.title = element_blank(),
      #legend.position = c(0.95, 0.75),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 30, hjust = 1))
#library(ggpubr)
ggarrange(p1, p2,ncol = 1, nrow = 2)


#Gender by ML experience
ml_exp<- (dat2
          %>% group_by(Q15, gender)
          %>% summarize(participants = n())
          %>% filter(gender %in% c("Man", "Woman"))
          %>% mutate(Q15 = ifelse(Q15 == "", "No Answer",
                                  ifelse(Q15 =="I do not use machine learning methods", "No ML Experience", Q15)))
          %>% ungroup())
man_ml <- filter(ml_exp, gender == "Man")

#Donut chart
man_ml <- (man_ml 
           %>% mutate(ML_Exp = Q15, percent = participants/sum(man_ml$participants))
           %>% arrange(desc(Q15)) 
           %>% mutate(lab.ypos = cumsum(percent) - 0.5*percent))


mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#D8BFD8","#4169E1",
            "#808000", "#800080", "#008080", "#9ACD32", "#DAA520", "#32CD32", "#9370DB")
mn<- ggplot(man_ml, aes(x = 2, y = percent, fill =ML_Exp)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(percent*100,0), "%")), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+ xlim(0.5, 2.5)+
  ggtitle("ML Exprience of Male Participants")


woman_ml <- filter(ml_exp, gender != "Man")
woman_ml <- (woman_ml 
             %>% mutate(ML_Exp = Q15, percent = participants/sum(woman_ml$participants))
             %>% arrange(desc(Q15)) 
             %>% mutate(lab.ypos = cumsum(percent) - 0.5*percent))

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#D8BFD8","#4169E1",
            "#808000", "#800080", "#008080", "#9ACD32", "#DAA520", "#32CD32", "#9370DB")
wn<- ggplot(woman_ml, aes(x = 2, y = percent, fill =ML_Exp)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(percent*100,0), "%")), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+ xlim(0.5, 2.5)+
  ggtitle("ML Exprience of Female Participants")

ggarrange(mn, wn,ncol = 2, nrow = 1)


#########################################
#Age Group and country
#########################################
age_by_country<- (dat2
                  %>% group_by(country, Age)
                  %>% summarize(participants = n())
                  %>%mutate(country = ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "UK",
                                             ifelse(country == "United States of America", "USA", country)))
                  %>% left_join(., country)
                  %>% mutate(percent = participants/freq)
                  %>% filter(country %in% c)
                  
            )


ggplot(age_by_country, aes(fill=Age, y=percent, x=reorder(country, -participants))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Top 10 countries",y="Age Group (Percent)")+
  ggtitle("Age Group by Top 10 Country") +
  theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10))



## R vs Python


