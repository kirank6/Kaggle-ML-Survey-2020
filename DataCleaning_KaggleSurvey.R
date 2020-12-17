#Data Cleaning of Kaggle Survey Data

rm(list=ls())
cat('\14')


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("reshape2", "dplyr", "RODBC", "sqldf", "rgdal","ggplot2" , "tmap", "scales", "RColorBrewer")
ipak(packages)

fp = "C:/SA_P/Kaggle/UserSurveyCompetition"
dat <- read.csv(file.path(fp, "kaggle_survey_2020_responses.csv"))
dat<- dat[2:20037,]

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
dat <- as.data.frame(apply(X= dat, MARGIN=2,FUN='trim'))



dat1<- (dat
        %>% rename(time_taken=`Time.from.Start.to.Finish..seconds.`, age = Q1, gender=Q2,
                      country = Q3, highest.edu = Q4, current.role = Q5,coding_exp =Q6)
        %>% mutate(program.python = ifelse(Q7_Part_1 == "Python", 1,0),
                   program.R = ifelse(Q7_Part_2 == "R", 1,0),
                   program.sql = ifelse(Q7_Part_3 == "SQL", 1,0),
                   program.c = ifelse(Q7_Part_4 == "C", 1,0),
                   program.c_plus = ifelse(Q7_Part_5 == "C++", 1,0),
                   program.java = ifelse(Q7_Part_6 == "Java", 1,0),
                   program.js = ifelse(Q7_Part_7 == "Javascript", 1,0),
                   program.julia = ifelse(Q7_Part_8 == "Julia", 1,0),
                   program.swift = ifelse(Q7_Part_9 == "Swift", 1,0),
                   program.bash = ifelse(Q7_Part_10 == "Bash", 1,0),
                   program.matlab = ifelse(Q7_Part_11 == "MATLAB", 1,0),
                   program.none = ifelse(Q7_Part_12 == "None", 1,0),
                   program.other = ifelse(Q7_OTHER == "Other", 1,0),
                   
                   prog.recom = Q8,
                   
                   ide.jupyter = ifelse(Q9_Part_1 =="Jupyter (JupyterLab, Jupyter Notebooks, etc)",1,0),
                   ide.rstudio = ifelse(Q9_Part_2 == "RStudio", 1, 0),
                   ide.visualstudio = ifelse(Q9_Part_3 == "Visual Studio", 1, 0),
                   ide.vscode = ifelse(Q9_Part_4 == "Visual Studio Code (VSCode)", 1, 0),
                   ide.pycharm = ifelse(Q9_Part_5 == "PyCharm", 1, 0),
                   ide.spyder = ifelse(Q9_Part_6 == "Spyder", 1, 0),
                   ide.notepadplus   = ifelse(Q9_Part_7 == "Notepad++", 1, 0),
                   ide.sublime_Text = ifelse(Q9_Part_8 == "Sublime Text", 1, 0),
                   ide.vim.emacs = ifelse(Q9_Part_9 == "Vim / Emacs", 1, 0),
                   ide.matlab = ifelse(Q9_Part_10 == "MATLAB", 1, 0),
                   ide.none = ifelse(Q9_Part_11 == "None", 1, 0),
                   ide.other = ifelse(Q9_OTHER == "Other", 1, 0),
                   
                   hosted.notebook.kaggle = ifelse(Q10_Part_1 == "Kaggle Notebooks", 1, 0),
                   hosted.notebook.colab = ifelse(Q10_Part_2 == "Colab Notebooks", 1, 0),
                   hosted.notebook.azure = ifelse(Q10_Part_3 == "Azure Notebooks", 1, 0),
                   hosted.notebook.gradient = ifelse(Q10_Part_4 == "Paperspace / Gradient", 1, 0),
                   hosted.notebook.binder = ifelse(Q10_Part_5 == "Binder / JupyterHub", 1, 0),
                   hosted.notebook.codeclean = ifelse(Q10_Part_6 == "Code Ocean", 1, 0),
                   hosted.notebook.ibmstudio = ifelse(Q10_Part_7 == "IBM Watson Studio", 1, 0),
                   hosted.notebook.amazonsage = ifelse(Q10_Part_8 == "Amazon Sagemaker Studio", 1, 0),
                   hosted.notebook.amazonemr = ifelse(Q10_Part_9 == "Amazon EMR Notebooks", 1, 0),
                   hosted.notebook.googlecloud.ai = ifelse(Q10_Part_10 == "Google Cloud AI Platform Notebooks", 1, 0),
                   hosted.notebook.googlecloud.datalab = ifelse(Q10_Part_11 == "Google Cloud Datalab Notebooks", 1, 0),
                   hosted.notebook.databrick = ifelse(Q10_Part_12 == "Databricks Collaborative Notebooks", 1, 0),
                   hosted.notebook.none = ifelse(Q10_Part_13 == "None", 1, 0),
                   hosted.notebook.other = ifelse(Q10_OTHER == "Other", 1, 0),
                   computing.env = Q11,
                   
                   hardware.gpu = ifelse(Q12_Part_1 == "GPUs", 1, 0),
                   hardware.tpu = ifelse(Q12_Part_2 == "TPUs", 1, 0),
                   hardware.none = ifelse(Q12_Part_3 == "None", 1, 0),
                   hardware.other = ifelse(Q12_OTHER == "GPUs", 1, 0),
                   
                   tpu.usefreq = Q13,
                   
                   viz.lib.matplotlib = ifelse(Q14_Part_1 == "Matplotlib", 1, 0),
                   viz.lib.seaborn  = ifelse(Q14_Part_2 == "Seaborn", 1, 0),
                   viz.lib.plotly = ifelse(Q14_Part_3 == "Plotly / Plotly Express", 1, 0),
                   viz.lib.ggplot = ifelse(Q14_Part_4 == "Ggplot / ggplot2", 1, 0),
                   viz.lib.shiny  = ifelse(Q14_Part_5 == "Shiny", 1, 0),
                   viz.lib.d3js = ifelse(Q14_Part_6 == "D3 js", 1, 0),
                   viz.lib.altair = ifelse(Q14_Part_7 == "Altair", 1, 0),
                   viz.lib.bokeh = ifelse(Q14_Part_8 == "Bokeh", 1, 0),
                   viz.lib.geoplotlib = ifelse(Q14_Part_9 == "Geoplotlib", 1, 0),
                   viz.lib.leaflet = ifelse(Q14_Part_10 == "Leaflet / Folium", 1, 0),
                   viz.lib.none = ifelse(Q14_Part_11 == "None", 1, 0),
                   viz.lib.other = ifelse(Q14_OTHER == "Other", 1, 0),
                   
                   ml.experience = Q15,
                   ml.sklearn  = ifelse(Q16_Part_1 == "Scikit-learn", 1, 0),
                   ml.tensorflow = ifelse(Q16_Part_2 == "TensorFlow", 1, 0),
                   ml.keras = ifelse(Q16_Part_3 == "Keras", 1, 0),
                   ml.pytorch = ifelse(Q16_Part_4 == "PyTorch", 1, 0),
                   ml.fast_ai  = ifelse(Q16_Part_5 == "Fast.ai", 1, 0),
                   ml.mxnet = ifelse(Q16_Part_6 == "MXNet", 1, 0),
                   ml.Xgboost = ifelse(Q16_Part_7 == "Xgboost", 1, 0),
                   ml.lightGBM  = ifelse(Q16_Part_8 == "LightGBM", 1, 0),
                   ml.catboost  = ifelse(Q16_Part_9 == "CatBoost", 1, 0),
                   ml.prophet  = ifelse(Q16_Part_10 == "Prophet", 1, 0),
                   ml.h2o = ifelse(Q16_Part_11 == "H2O 3", 1, 0),
                   ml.Caret  = ifelse(Q16_Part_12 == "Caret", 1, 0),
                   ml.tidymodel = ifelse(Q16_Part_13 == "Tidymodels", 1, 0),
                   ml.jax  = ifelse(Q16_Part_14 == "JAX", 1, 0),
                   ml.none = ifelse(Q16_Part_15 == "None", 1, 0),
                   ml.other = ifelse(Q16_OTHER == "Other", 1, 0),
                   
                   mlmodel.logistic = ifelse(Q17_Part_1 == "Linear or Logistic Regression", 1,0),
                   mlmodel.DecTr_Rf = ifelse(Q17_Part_2 == "Decision Trees or Random Forests", 1,0),
                   mlmodel.boosting = ifelse(Q17_Part_3 == "Gradient Boosting Machines (xgboost, lightgbm, etc)", 1,0),
                   mlmodel.baysian = ifelse(Q17_Part_4 == "Bayesian Approaches", 1,0),
                   mlmodel.revolunary = ifelse(Q17_Part_5 == "Evolutionary Approaches", 1,0),
                   mlmodel.neuralnet = ifelse(Q17_Part_6 == "Dense Neural Networks (MLPs, etc)", 1,0),
                   mlmodel.convnet = ifelse(Q17_Part_7 == "Convolutional Neural Networks", 1,0),
                   mlmodel.gan = ifelse(Q17_Part_8 == "Generative Adversarial Networks", 1,0),
                   mlmodel.rnn = ifelse(Q17_Part_9 == "Recurrent Neural Networks", 1,0),
                   mlmodel.transfmr = ifelse(Q17_Part_10 == "Transformer Networks (BERT, gpt-3, etc)", 1,0),
                   mlmodel.none = ifelse(Q17_Part_11 == "None", 1,0),
                   mlmodel.other = ifelse(Q17_OTHER == "Other", 1,0),
                   
                   computervision.general = ifelse(Q18_Part_1 == "General purpose image/video tools (PIL, cv2, skimage, etc)", 1,0),
                   computervision.image_segment = ifelse(Q18_Part_2 == "Image segmentation methods (U-Net, Mask R-CNN, etc)", 1,0),
                   computervision.obj_detect = ifelse(Q18_Part_3 == "Object detection methods (YOLOv3, RetinaNet, etc)", 1,0),
                   computervision.img_classif = ifelse(Q18_Part_4 == "Image classification and other general purpose networks (VGG, Inception, ResNet, ResNeXt, NASNet, EfficientNet, etc)", 1,0),
                   computervision.generative_net = ifelse(Q18_Part_5 == "Generative Networks (GAN, VAE, etc)", 1,0),
                   computervision.none = ifelse(Q18_Part_6 == "None", 1,0),
                   computervision.other = ifelse(Q18_OTHER == "Other", 1,0),
                   
                   nlpmodel.word_embedding = ifelse(Q19_Part_1 == "Word embeddings/vectors (GLoVe, fastText, word2vec)", 1,0),
                   nlpmodel.encoder_decode = ifelse(Q19_Part_2 == "Encoder-decorder models (seq2seq, vanilla transformers)", 1,0),
                   nlpmodel.context_embedding = ifelse(Q19_Part_3 == "Contextualized embeddings (ELMo, CoVe)", 1,0),
                   nlpmodel.transformer_lang = ifelse(Q19_Part_4 == "Transformer language models (GPT-3, BERT, XLnet, etc)", 1,0),
                   nlpmodel.none = ifelse(Q19_Part_5 == "None", 1,0),
                   nlpmodel.other = ifelse(Q19_OTHER == "Other", 1,0),
                  
                   company_size = Q20,
                   datascience.persons = Q21,
                   ml_use_in_business = Q22,
                   
                   important_part.analyze_understand = ifelse(Q23_Part_1 == "Analyze and understand data to influence product or business decisions", 1, 0),
                   important_part.data_infrastructure = ifelse(Q23_Part_2 == "Build and/or run the data infrastructure that my business uses for storing, analyzing, and operationalizing data", 1, 0),
                   important_part.ml_prototyping = ifelse(Q23_Part_3 == "Build prototypes to explore applying machine learning to new areas", 1, 0),
                   important_part.ml_buildandrun = ifelse(Q23_Part_4 == "Build and/or run a machine learning service that operationally improves my product or workflows", 1, 0),
                   important_part.ml_experiment = ifelse(Q23_Part_5 == "Experimentation and iteration to improve existing ML models", 1, 0),
                   important_part.ml_research = ifelse(Q23_Part_6 == "Do research that advances the state of the art of machine learning", 1, 0),
                   important_part.none = ifelse(Q23_Part_7 == "None of these activities are an important part of my role at work", 1, 0),
                   important_part.other = ifelse(Q23_OTHER == "Other", 1, 0),
                   
                   annual.compensation = Q24,
                   ml.investment_in5year = Q25,
                  
                   cloud_platform.aws = ifelse(Q26_A_Part_1 == "Amazon Web Services (AWS)", 1,0),
                   cloud_platform.azure = ifelse(Q26_A_Part_2 == "Microsoft Azure", 1,0),
                   cloud_platform.google_cloud = ifelse(Q26_A_Part_3 == "Google Cloud Platform (GCP)", 1,0),
                   cloud_platform.ibm_cloud = ifelse(Q26_A_Part_4 == "IBM Cloud / Red Hat", 1,0),
                   cloud_platform.oracle_cloud = ifelse(Q26_A_Part_5 == "Oracle Cloud", 1,0),
                   cloud_platform.sap_cloud = ifelse(Q26_A_Part_6 == "SAP Cloud", 1,0),
                   cloud_platform.salesforce_cloud = ifelse(Q26_A_Part_7 == "Salesforce Cloud", 1,0),
                   cloud_platform.vmware_cloud = ifelse(Q26_A_Part_8 == "VMware Cloud", 1,0),
                   cloud_platform.alibaba_cloud = ifelse(Q26_A_Part_9 == "Alibaba Cloud", 1,0),
                   cloud_platform.tencent_cloud  = ifelse(Q26_A_Part_10 == "Tencent Cloud", 1,0),
                   cloud_platform.none = ifelse(Q26_A_Part_11 == "None", 1,0),
                   cloud_platform.other = ifelse(Q26_A_OTHER == "Other", 1,0),
                   
                   cloud_computing.amazonEC2 = ifelse(Q27_A_Part_1 == "Amazon EC2", 1,0),
                   cloud_computing.aws_lambda = ifelse(Q27_A_Part_2 == "AWS Lambda", 1,0),
                   cloud_computing.amazon_elastic = ifelse(Q27_A_Part_3 == "Amazon Elastic Container Service", 1,0),
                   cloud_computing.azure_cloud = ifelse(Q27_A_Part_4 == "Azure Cloud Services", 1,0),
                   cloud_computing.azure_container = ifelse(Q27_A_Part_5 == "Microsoft Azure Container Instances", 1,0),
                   cloud_computing.azure_function = ifelse(Q27_A_Part_6 == "Azure Functions", 1,0),
                   cloud_computing.google_cloud = ifelse(Q27_A_Part_7 == "Google Cloud Compute Engine", 1,0),
                   cloud_computing.google_function = ifelse(Q27_A_Part_8 == "Google Cloud Functions", 1,0),
                   cloud_computing.google_run = ifelse(Q27_A_Part_9 == "Google Cloud Run", 1,0),
                   cloud_computing.google_app = ifelse(Q27_A_Part_10 == "Google Cloud App Engine", 1,0),
                   cloud_computing.none = ifelse(Q27_A_Part_11 == "No / None", 1,0),
                   cloud_computing.other = ifelse(Q27_A_OTHER == "Other", 1,0),
                   
                   mlproduct_use.amazon_sagemaker = ifelse(Q28_A_Part_1 == "Amazon SageMaker",1,0),
                   mlproduct_use.amazon_forecast = ifelse(Q28_A_Part_2 == "Amazon Forecast",1,0),
                   mlproduct_use.amazon_rekogn = ifelse(Q28_A_Part_3 == "Amazon Rekognition",1,0),
                   mlproduct_use.azure_mlstudio = ifelse(Q28_A_Part_4 == "Azure Machine Learning Studio",1,0),
                   mlproduct_use.azure_cognitive = ifelse(Q28_A_Part_5 == "Azure Cognitive Services",1,0),
                   mlproduct_use.google_cloudAI = ifelse(Q28_A_Part_6 == "Google Cloud AI Platform / Google Cloud ML Engine",1,0),
                   mlproduct_use.google_videoAI = ifelse(Q28_A_Part_7 == "Google Cloud Video AI",1,0),
                   mlproduct_use.googe_cloud_NL = ifelse(Q28_A_Part_8 == "Google Cloud Natural Language",1,0),
                   mlproduct_use.google_cloud_vision = ifelse(Q28_A_Part_9 == "Google Cloud Vision AI",1,0),
                   mlproduct_use.none = ifelse(Q28_A_Part_10 == "No / None",1,0),
                   mlproduct_use.other = ifelse(Q28_A_OTHER == "Other",1,0),
                   

                   want_familarity_bigdata_product_next2year.mysql = ifelse(Q29_B_Part_1 == "MySQL",1,0),
                   want_familarity_bigdata_product_next2year.postgressql = ifelse(Q29_B_Part_2 == "PostgresSQL",1,0),
                   want_familarity_bigdata_product_next2year.sqlite = ifelse(Q29_B_Part_3 == "SQLite",1,0),
                   want_familarity_bigdata_product_next2year.oracledb = ifelse(Q29_B_Part_4 == "Oracle Database",1,0),
                   want_familarity_bigdata_product_next2year.mongodb = ifelse(Q29_B_Part_5 == "MongoDB",1,0),
                   want_familarity_bigdata_product_next2year.snowflake = ifelse(Q29_B_Part_6 == "Snowflake",1,0),
                   want_familarity_bigdata_product_next2year.ibmdb2 = ifelse(Q29_B_Part_7 == "IBM Db2",1,0),
                   want_familarity_bigdata_product_next2year.mssql = ifelse(Q29_B_Part_8 == "Microsoft SQL Server",1,0),
                   want_familarity_bigdata_product_next2year.access = ifelse(Q29_B_Part_9 == "Microsoft Access",1,0),
                   want_familarity_bigdata_product_next2year.azure_datalake = ifelse(Q29_B_Part_10 == "Microsoft Azure Data Lake Storage",1,0),
                   want_familarity_bigdata_product_next2year.amazon_redshift = ifelse(Q29_B_Part_11 == "Amazon Redshift",1,0),
                   want_familarity_bigdata_product_next2year.amazon_athena = ifelse(Q29_B_Part_12 == "Amazon Athena",1,0),
                   want_familarity_bigdata_product_next2year.amazon_dynamo = ifelse(Q29_B_Part_13 == "Amazon DynamoDB",1,0),
                   want_familarity_bigdata_product_next2year.google_bigquery = ifelse(Q29_B_Part_14 == "Google Cloud BigQuery",1,0),
                   want_familarity_bigdata_product_next2year.google_cloud_sql = ifelse(Q29_B_Part_15 == "Google Cloud SQL",1,0),
                   want_familarity_bigdata_product_next2year.google_firestore = ifelse(Q29_B_Part_16 == "Google Cloud Firestore",1,0),
                   want_familarity_bigdata_product_next2year.none = ifelse(Q29_B_Part_17 == "None",1,0),
                   want_familarity_bigdata_product_next2year.google_other = ifelse(Q29_B_OTHER == "Other",1,0),
                   
                   big_dataproduct_use = Q30,
                   
                   bi_tooluse.Amazon_quicksight = ifelse(Q31_A_Part_1 == "Amazon QuickSight",1,0),
                   bi_tooluse.Microsoft_powerBI = ifelse(Q31_A_Part_2 == "Microsoft Power BI",1,0),
                   bi_tooluse.google_data_studio = ifelse(Q31_A_Part_3 == "Google Data Studio",1,0),
                   bi_tooluse.tableau = ifelse(Q31_A_Part_5 == "Tableau",1,0),
                   
                   public_share_platform.github = ifelse(Q36_Part_4 == "GitHub",1,0),
                   public_share_platform.personal_blog  = ifelse(Q36_Part_5 == "Personal blog",1,0),
                   public_share_platform.Kaggle = ifelse(Q36_Part_6 == "Kaggle",1,0),
                   public_share_platform.colab = ifelse(Q36_Part_7 == "Colab",1,0),
                   public_share_platform.shiny = ifelse(Q36_Part_8 == "Shiny",1,0),
                   public_share_platform.no_share = ifelse(Q36_Part_9 == "I do not share my work publicly",1,0),
                  
                   dscourse_completion_in.coursera = ifelse(Q37_Part_1 == "Coursera",1,0),
                   dscourse_completion_in.edx = ifelse(Q37_Part_2 == "edX",1,0),
                   dscourse_completion_in.kaggle = ifelse(Q37_Part_3 == "Kaggle Learn Courses",1,0),
                   dscourse_completion_in.datacamp = ifelse(Q37_Part_4 == "DataCamp",1,0),
                   dscourse_completion_in.fast_ai = ifelse(Q37_Part_5 == "Fast.ai",1,0),
                   dscourse_completion_in.udacity = ifelse(Q37_Part_6 == "Udacity",1,0),
                   dscourse_completion_in.udemy = ifelse(Q37_Part_7 == "Udemy",1,0),
                   dscourse_completion_in.linkedin = ifelse(Q37_Part_8 == "LinkedIn Learning",1,0),
                   dscourse_completion_in.cloud_certification = ifelse(Q37_Part_9 == "Cloud-certification programs (direct from AWS, Azure, GCP, or similar)",1,0),
                   dscourse_completion_in.university_degree = ifelse(Q37_Part_10 == "University Courses (resulting in a university degree)",1,0),
                   
                   mlcourse_want_in2yrs.google_cloud_autoML = ifelse(Q34_B_Part_1 == "Google Cloud AutoML",1,0),
                   mlcourse_want_in2yrs.h2o_ai = ifelse(Q34_B_Part_2 == "H20 Driverless AI",1,0),
                   mlcourse_want_in2yrs.aatabricks_autoML  = ifelse(Q34_B_Part_3 == "Databricks AutoML",1,0),
                   mlcourse_want_in2yrs.datarobot_autoML = ifelse(Q34_B_Part_4 == "DataRobot AutoML",1,0),
                   mlcourse_want_in2yrs.tpot = ifelse(Q34_B_Part_5 == "Tpot",1,0),
                   mlcourse_want_in2yrs.auto_Keras = ifelse(Q34_B_Part_6 == "Auto-Keras",1,0),
                   mlcourse_want_in2yrs.auto_sklearn  = ifelse(Q34_B_Part_7 == " Auto-Sklearn",1,0),
                   mlcourse_want_in2yrs.auto_ml  = ifelse(Q34_B_Part_8 == "Auto_ml",1,0),
                   mlcourse_want_in2yrs.xcessiv = ifelse(Q34_B_Part_9 == "Xcessiv",1,0),
                   mlcourse_want_in2yrs.mlbox = ifelse(Q34_B_Part_10 == " MLbox",1,0),
                   mlcourse_want_in2yrs.none = ifelse(Q34_B_Part_11 == "None",1,0),
                   mlcourse_want_in2yrs.other = ifelse(Q34_B_OTHER == "Other",1,0)
                   
                   
                   )
        
  
)

dat1 <- dat1[, c(1:7,356:554)]

#write.csv(dat1, file.path(fp, "cleanedsurveydata.csv"))

dat1<- (dat1
        %>% mutate(highest.edu = ifelse(highest.edu =="Doctoral degree", "Doctoral",
                                        ifelse(highest.edu =="Masterâ???Ts degree", "Masters",
                                               ifelse(highest.edu =="Bachelorâ???Ts degree", "Bachelors",
                                                      ifelse(highest.edu =="Professional degree", "professional",
                                                             ifelse(highest.edu =="Some college/university study without earning a bachelorâ???Ts degree", "some_college",
                                                                           "NoAnswer")))))
        )
                   
)



