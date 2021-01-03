#Load the library used for Bayesian style t Test
library(rjags)
source("BEST.R")
library(plotly)

#Load Data
DataFile = "survey_data_v2.csv"
df = read.csv(DataFile, header=T)

#Numerical questions for analysis
NQ1 = "How.many.hours.a.day.do.you.typically.spend.on.your.job."
NQ2 = "How.many.hours.a.day.do.you.typically.spend.preparing.meals."
NQ3 = "How.many.hours.a.week.do.you.spend.on.housework..outside.of.cooking.meals."
NQ4 = "How.many.hours.a.day.do.you.spend.on.electronics.for.leisure."
NQ5 = "How.many.hours.a.day.are.you.spending.with.your.family.members.or.roommates."
NQ6 = "How.many.months.will.it.take.for.the.economy.to.recover."

#Manage for outliers in Question 6 (based on survey instructions)
mask = df[NQ6] > 60
df[mask,NQ6] = 60

#Create function to run Bayesian Analysis
Bayes_ABTest = function(Survey_Number_A, Survey_Number_B, question){
  
  #Filtering between survey responses
  mask_A = df['Survey'] == Survey_Number_A
  mask_B = df['Survey'] == Survey_Number_B

  #Create data vectors  
  A = df[mask_A,question]
  B = df[mask_B,question]

  # Run the Bayesian analysis:
  mcmcChain = BESTmcmc( A , B ) 

  # Plot the results of the Bayesian analysis:
  postInfo = BESTplot( A , B , mcmcChain , pairsPlot=TRUE )
  
  # Show detailed summary info on console:
  show( postInfo ) 
}

#Create function for graphing
bayes_graph = function(less_0, greater_0){
  
  #x labels
  Question <- c('Q1 Job', 'Q2 Meals', 'Q3 Housework', 'Q4 Leisure', 'Q5 Family, Roomates')
  
  #create data
  data <- data.frame(Question, less_0, greater_0)
  
  fig <- plot_ly(data, x = ~Question, y = ~less_0, type = 'bar', name = 'Probability difference is less than 0',
                 marker = list(color = 'rgba(255, 212, 96, 2)'))
  
  fig <- fig %>% add_trace(y = ~greater_0, name = 'Probability difference is greater than 0',
                           marker = list(color = 'rgba(168, 216, 234, 2)'))
  
  fig <- fig %>% layout(yaxis = list(title = 'Probability (%)'), barmode = 'stack')
  
  fig  
  
}

#Run Analysis
Survey_A = 1
Survey_B = 2
Question = NQ2

Bayes_ABTest(Survey_A, Survey_B, Question)

#Survey 1 vs 2 values
Prob_diff_neg_12 = c(43.4, 17.2, 41.0, 97.8, 99.5)
Prob_diff_pos_12 = rep(100, 5) - Prob_diff_neg_12

#Survey 3 vs 4 values
Prob_diff_neg_34 = c(6.5, 89.4, 42.2, 98.5, 99.9)
Prob_diff_pos_34 = rep(100, 5) - Prob_diff_neg_34

#Survey 3 vs 4 values
Prob_diff_neg_56 = c(0, 98.2, 68.4, 88.6, 99.9)
Prob_diff_pos_56 = rep(100, 5) - Prob_diff_neg_56


#Graphs
bayes_graph(Prob_diff_neg_12, Prob_diff_pos_12) #Survey 1 vs 2

bayes_graph(Prob_diff_neg_34, Prob_diff_pos_34) #Survey 3 vs 4

bayes_graph(Prob_diff_neg_56, Prob_diff_pos_56) #Survey 5 vs 6


econ_12 = 99.9
econ_34 = 64.9
econ_56 = 52.2







