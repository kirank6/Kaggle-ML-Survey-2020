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
dat1<- select(dat, Q1, Q2, Q3, Q4, Q5, Q6, Q8, Q11, Q13, Q15, Q20, Q21, Q22, Q24, Q25, Q30)

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
              -Q6, -Q8, -Q11, -Q13, -Q15, -Q20, -Q21, -Q22, -Q24, -Q25, -Q30)

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

ggplot(prog_by_edu, aes(fill=Num, y=value, x=highest.edu)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Highest Education of Survey Participants",y="Number of Survey Participants")+
  ggtitle("Number of Programming Language Used By Education Level") +
  theme(legend.title = element_blank(),
        legend.position = c(0.95, 0.6),
        legend.text = element_text(size = 10))

#Number of Programming Language Used by countries
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
               #%>% filter(country %in% c)
               %>%mutate(country = ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "UK",
                                          ifelse(country == "United States of America", "USA",
                                                 ifelse(country== "Iran, Islamic Republic of...", "Iran",country))))
               
)


#Plot 
prog_by_country<- melt(prog_by_country, id = c("country"))
prog_by_country$Num <- substr(prog_by_country$variable, 6,7)
prog_by_country1<- prog_by_country%>% group_by(country)%>% summarise(sum=sum(value))%>%ungroup()
prog_by_country<- prog_by_country%>% left_join(.,prog_by_country1)%>% mutate(value= value/sum)

ggplot(prog_by_country, aes(fill=Num, y=value, x=country)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Country of Survey Participants",y="Number of Survey Participants")+
  ggtitle("Number of Programming Language Used By Country") +
  theme(#legend.title = element_blank(),
        #legend.position = c(0.95, 0.75),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1))


#ML Experience with Number of Programming Language
prog_by_LM_Exp<- (dat2
                   %>% mutate(prog_all = (Q7_Part_1Python+Q7_Part_2R+Q7_Part_3SQL+Q7_Part_4C+
                                            Q7_Part_4C+Q7_Part_5C..+Q7_Part_8Julia+Q7_Part_8Julia+
                                            Q7_Part_9Swift+Q7_Part_10Bash+Q7_Part_11MATLAB+Q7_Part_12None+Q7_OTHEROther),
                              prog_1 = ifelse(prog_all == 1, 1, 0),
                              prog_2 = ifelse(prog_all == 2, 1, 0),
                              prog_3 = ifelse(prog_all == 3, 1, 0),
                              prog_4 = ifelse(prog_all == 4, 1, 0),
                              prog_5 = ifelse(prog_all > 4, 1, 0))
                   %>% group_by(Q15)
                   %>% summarize(prog_1 = sum(prog_1),
                                 prog_2 = sum(prog_2),
                                 prog_3 = sum(prog_3),
                                 prog_4 = sum(prog_4),
                                 prog_5 = sum(prog_5),
                                 .groups = 'drop')
                  
)



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
  theme(legend.text = element_text(size = 10))


#Gender by Education level
#Normalized plot
gender_by_edu<- (dat2
                 %>% group_by(highest.edu, gender)
                 %>% summarize(participants = n(),.groups = 'drop')
                 %>% filter(gender %in% c("Man", "Woman")))
edu<- gender_by_edu%>% group_by(highest.edu)%>% summarise(total = sum(participants),.groups = 'drop')
gender_by_edu<- gender_by_edu%>% left_join(., edu)%>% mutate(percent = participants/total)

gender_by_role<- (dat2
                 %>% group_by(gender, current.role)
                 %>% summarize(participants = n(),.groups = 'drop')
                 %>% filter(gender %in% c("Man", "Woman")))
role<- gender_by_role%>% group_by(current.role)%>% summarise(total = sum(participants),.groups = 'drop')
gender_by_role<- gender_by_role%>% left_join(., role)%>% mutate(percent = participants/total)



#Stacked bar with two panel
p1<- ggplot(gender_by_edu, aes(fill=gender, y=percent, x=reorder(highest.edu, -participants))) + 
     geom_bar(position="stack", stat="identity")+
     labs(x="Percent of Participants",y="Highest Education Level")+
     ggtitle("Education Level by Gender") +
     theme(legend.text = element_text(size = 10))

p2<-  ggplot(gender_by_role, aes(fill=gender, y=percent, x=reorder(current.role, -participants))) + 
      geom_bar(position="stack", stat="identity")+
      labs(x="Percent of Participants",y="Current Role")+
      ggtitle("Current Role by Gender") +
      theme(legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 30, hjust = 1))

ggarrange(p1, p2,ncol = 1, nrow = 2)


#Gender by ML experience
ml_exp<- (dat2
          %>% group_by(Q15, gender)
          %>% summarize(participants = n(),.groups = 'drop')
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
                  %>% summarize(participants = n(),.groups = 'drop')
                  %>%mutate(country = ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "UK",
                                             ifelse(country == "United States of America", "USA",
                                                    ifelse(country== "Iran, Islamic Republic of...", "Iran" ,country))))
                  %>% left_join(., country)
                  %>% mutate(percent = participants/freq)
                  #%>% filter(country %in% c)
                  
            )


ggplot(age_by_country, aes(fill=Age, y=percent, x=reorder(country, -participants))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Top 10 countries",y="Age Group (Percent)")+
  ggtitle("Age Group by Country") +
  theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1))

#ML use Experience and model use

ml_model<- (dat2
            %>% group_by(Q15)
            %>% summarise(lin_log = sum(Q17_Part_1Linear.or.Logistic.Regression),
                          tree_models = sum(Q17_Part_2Decision.Trees.or.Random.Forests),
                          boosting_models = sum(Q17_Part_3Gradient.Boosting.Machines..xgboost..lightgbm..etc.),
                          baseyian_model = sum(Q17_Part_4Bayesian.Approaches),
                          evolutionay_model = sum(Q17_Part_5Evolutionary.Approaches),
                          neuralNet = sum(Q17_Part_6Dense.Neural.Networks..MLPs..etc.),
                          ConvNet = sum(Q17_Part_7Convolutional.Neural.Networks),
                          generative_model = sum(Q17_Part_8Generative.Adversarial.Networks),
                          recurrent_model = sum(Q17_Part_9Recurrent.Neural.Networks),
                          transformers = sum(Q17_Part_10Transformer.Networks..BERT..gpt.3..etc.)
                          #others = sum(Q17_OTHEROther),
                          #none = sum(Q17_Part_11None)
                          )
            # %>% mutate(Q15 = ifelse(Q15 == "", "No Answer",
            #                         ifelse(Q15 =="I do not use machine learning methods", "No ML Experience", Q15)))
            %>% ungroup()
            %>% filter(Q15 != "" & Q15 != "I do not use machine learning methods"))


ml_model<- melt(ml_model, id = "Q15")
names(ml_model)<- c("ML_Exp", "Models_used", "value")

ggplot(ml_model, aes(fill=Models_used, y=value, x=reorder(ML_Exp, -value))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Machine Learning Experience",y="Models Used")+
  ggtitle("Models Used by ML Experience") +
  theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 30, hjust = 1))


## R vs Python

r_python<- (dat2
            %>% group_by(Q15)
            %>% summarise(R = sum(Q7_Part_2R),
                          python = sum(Q7_Part_1Python), .groups = 'drop'
            )
            %>% ungroup()
            %>% filter(Q15 != "" & Q15 != "I do not use machine learning methods")
            %>% mutate(R_percent = R/sum(R),
                       python_percent = python/sum(python),
                       ML_Exp = Q15)
            %>% select(ML_Exp, R, python))
r_python<- melt(r_python, id = "ML_Exp")

ggplot(r_python, aes(fill=variable, y=value, x=reorder(ML_Exp, -value))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Machine Learning Experience",y="R/Python")+
  ggtitle("R/Python Use by ML Experience") +
  theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 30, hjust = 1))


###########################################
#Combining Kiran Jee's charts
###########################################

#Current role and Machine Learning Experience
wn_ml_exp <- (dat2
              %>% filter(gender == "Woman")
              %>% group_by(Q15, current.role)
              %>% summarize(participants = n(),.groups = 'drop')
              %>% mutate(Q15 = ifelse(Q15 == "", "No Answer",
                                      ifelse(Q15 =="I do not use machine learning methods", "No ML Experience", as.character(Q15))),
                         current.role = ifelse(current.role == "", "NoAnswer", as.character(current.role)))
              %>% ungroup()
              %>% rename(ML_Exp= Q15))


#Plot
ggplot(wn_ml_exp, aes(fill=ML_Exp, y=participants, x=reorder(current.role, -participants))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Current Role",y="Numer of Women Participants")+
  ggtitle("ML Experience by Current Role") +
  theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 30, hjust = 1))



#Coding Experience by age group
wn_coding_age<- (dat2
                 %>% filter(gender == "Woman")
                  %>% mutate(prog_all = (Q7_Part_1Python+Q7_Part_2R+Q7_Part_3SQL+Q7_Part_4C+
                                           Q7_Part_4C+Q7_Part_5C..+Q7_Part_8Julia+Q7_Part_8Julia+
                                           Q7_Part_9Swift+Q7_Part_10Bash+Q7_Part_11MATLAB+Q7_Part_12None+Q7_OTHEROther),
                             prog_1 = ifelse(prog_all == 1, 1, 0),
                             prog_2 = ifelse(prog_all == 2, 1, 0),
                             prog_3 = ifelse(prog_all == 3, 1, 0),
                             prog_4 = ifelse(prog_all == 4, 1, 0),
                             prog_5 = ifelse(prog_all > 4, 1, 0))
                  %>% group_by(Age)
                  %>% summarize(prog_1 = sum(prog_1),
                                prog_2 = sum(prog_2),
                                prog_3 = sum(prog_3),
                                prog_4 = sum(prog_4),
                                prog_5 = sum(prog_5),
                                .groups = 'drop')
                 #%>% filter(Q15 != "")
)


#Area Plot 
wn_coding_age<- melt(wn_coding_age, id = c("Age"))
wn_coding_age %>% ggplot(aes(x= Age, y=value)) + 
  geom_area(aes(colour = variable, group=variable, fill = variable))+
  labs(x="Age of Women Participants",y="Num of Women Participants")+
  ggtitle("Number of Programming Experience by Age") +
  theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 30, hjust = 1))
  
#Education by country

wn_edu_country<- (dat2
               %>% filter(gender == "Woman")
               %>% group_by(country, highest.edu)
               %>% summarize(women_num = n(), .groups = 'drop')
               %>% ungroup()
               
)

wn_country <- wn_edu_country%>% group_by(country)%>% summarize(total_w = sum(women_num),.groups = 'drop')
wn_edu_country<- (wn_edu_country
                  %>% left_join(., wn_country)
                  %>% mutate(percent_w = women_num/total_w,
                             country = ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "UK",
                                              ifelse(country == "United States of America", "USA",
                                                     ifelse(country== "Iran, Islamic Republic of...", "Iran" ,country))))
                  )



#Stacked BarPlot 
ggplot(wn_edu_country, aes(fill=highest.edu, y=percent_w, x=country)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Country",y="Percent of Women")+
  ggtitle("Women Education Level by Country") +
  theme(legend.text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, hjust = 1))




################################
##US survey participants 
##################################

#Coding Skill of US Participants
US_coding_ml_exp<- (dat2
                    %>% filter(country == "United States of America")
                    %>% mutate(prog_all = (Q7_Part_1Python+Q7_Part_2R+Q7_Part_3SQL+Q7_Part_4C+
                                           Q7_Part_4C+Q7_Part_5C..+Q7_Part_8Julia+Q7_Part_8Julia+
                                           Q7_Part_9Swift+Q7_Part_10Bash+Q7_Part_11MATLAB+Q7_Part_12None+Q7_OTHEROther),
                              prog_1 = ifelse(prog_all == 1, 1, 0),
                              prog_2 = ifelse(prog_all == 2, 1, 0),
                              prog_3 = ifelse(prog_all == 3, 1, 0),
                              prog_4 = ifelse(prog_all == 4, 1, 0),
                              prog_5 = ifelse(prog_all > 4, 1, 0))
                    %>% filter(Q15 !="I do not use machine learning methods" & Q15 != "")
                    %>% mutate(ml_exp = ifelse(Q15 == "Under 1 year", "1-",
                                               ifelse(Q15 == "1-2 years", "1-2", 
                                                      ifelse(Q15 == "2-3 years", "2-3",
                                                             ifelse(Q15 == "3-4 years", "3-4",
                                                                    ifelse(Q15 == "4-5 years", "4-5", 
                                                                           ifelse(Q15 == "5-10 years","5-10",
                                                                                  ifelse(Q15 == "10-20 years", "10-20", "20+"))))))))
                    %>% group_by(ml_exp, gender)
                    %>% summarize(prog_1 = sum(prog_1),
                                  prog_2 = sum(prog_2),
                                  prog_3 = sum(prog_3),
                                  prog_4 = sum(prog_4),
                                  prog_5 = sum(prog_5),
                                  .groups = 'drop')
                    %>% filter(gender %in% c("Man", "Woman"))
                    #%>% mutate(Q15 = ifelse(Q15 == "I do not use machine learning methods", "No ML Exp", as.character(Q15)) )
                  
)

#Area Plot 
US_coding_ml_exp<- melt(US_coding_ml_exp, id = c("ml_exp", "gender"))
US_coding_ml_exp$Num_prog<- US_coding_ml_exp$variable

US_coding_ml_exp %>%
  mutate(ml_exp = fct_relevel(name, 
                            "1-", "1-2", "2-3", 
                            "3-4", "4-5", "5-10", 
                            "10-20", "20+")) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity") +
  xlab("")

US_coding_ml_exp %>%
  mutate(ml_exp = factor(ml_exp, level=c("1-", "1-2", "2-3", 
                              "3-4", "4-5", "5-10", "10-20", "20+")))%>% 
  ggplot(aes(x= ml_exp, y=value)) + 
  geom_area(aes(colour = Num_prog, group=Num_prog, fill = Num_prog))+
  labs(x="Machine Learning Experience (Years)",y="Num of Participants")+
  ggtitle("Programming Language Skill by ML Experience") +
    theme(legend.text = element_text(size = 8))+facet_wrap("gender")


#US Kaggle survey participanst by Age and current role
us_age_role <- (dat2
          %>% filter(country == "United States of America")
          %>% group_by(Age, current.role)
          %>% summarize(num = n(), .groups = 'drop')
          %>% filter(current.role != "" & current.role != "Other")
          
)

#Bar chart
ggplot(us_age_role, aes(fill=current.role, y=num, x=Age)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Age of Survey Participants",y="Count of Participants")+
  ggtitle("US Survey Paricipants by Age and Current Role") +
  theme(legend.text = element_text(size = 8))
        

#Who are data scientist in the US
us_ds <- (dat2
          %>% filter(country == "United States of America" & current.role == "Data Scientist")
          %>% group_by(Age, highest.edu)
          %>% summarize(num = n(), .groups = 'drop')
          
          )

#Area chart
us_ds %>% ggplot(aes(x= Age, y=num)) + 
  geom_area(aes(colour = highest.edu, group=highest.edu, fill = highest.edu))+
  labs(x="Age of US Survey Participants",y="Num of Partcipants")+
  ggtitle("Data Scientists in US by Age and Education") +
  theme(#legend.title = element_blank(),
    #legend.position = c(0.95, 0.75),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 30, hjust = 1))

# Education and current role
us_edu <- (dat2
          %>% filter(country == "United States of America")
          %>% group_by(current.role, highest.edu, gender)
          %>% summarize(num = n(), .groups = 'drop')
          %>% ungroup()
          %>% filter(current.role != "" & current.role != "Other" & gender %in% c("Man", "Woman"))
         )

#Bar chart
ggplot(us_edu, aes(fill=highest.edu, y=num, x=reorder(current.role, -num))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Current Role",y="Count of Participants")+
  ggtitle("Highest Education by Current Role in the US") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = 1))+
  facet_wrap(~gender)





