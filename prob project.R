new.function<-function()
{
  
  require(plyr)
  setwd("C:/Users/umair/Desktop/Prob Project Data")
  suicide_data=read.csv("Suicide_DATA.csv")  #read file
  print(suicide_data)
  View(suicide_data)
  
  suicide_data$age<- factor(suicide_data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))
  
  as.matrix(suicide_data)
  summary(suicide_data)
  
  ################Freq Distribution table#############################
  
  for (i in suicide_data)
  {
    z1 <-data.frame(table(suicide_data$country))
    result_country=mutate(z1, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    z2 <-data.frame(table(suicide_data$year))
    result_year=mutate(z2, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq1=cumsum(relFreq))
    
    z3 <-data.frame(table(suicide_data$sex))
    result_sex=mutate(z3, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    z4 <-data.frame(table(suicide_data$age))
    result_age=mutate(z4, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    z5 <-data.frame(table(suicide_data$suicides_no))
    result_suicide_no=mutate(z5, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    z6 <-data.frame(table(suicide_data$population))
    result_population=mutate(z6, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    z7 <-data.frame(table(suicide_data$suicides.100k.pop))
    result_suicide_per_100k=mutate(z7, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    z8 <-data.frame(table(suicide_data$gdp_for_year....))
    result_gpd_year=mutate(z8, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    z9 <-data.frame(table(suicide_data$gdp_per_capita....))
    result_gpd_capita=mutate(z9, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    z10 <-data.frame(table(suicide_data$generation))
    result_generation=mutate(z10, relFreq=prop.table(Freq), Cumulative_Freq=cumsum(Freq), Cumulative_Relative_Freq=cumsum(relFreq))
    
    
  }
  for (j in suicide_data)
  {
    
    print(result_country)
    print(result_age)
    print(result_year)
    print(result_generation)
    print(result_gpd_capita)
    print(result_gpd_year)
    print(result_sex)
    print(result_population)
    print(result_suicide_no)
    print(result_suicide_per_100k)
    
  }
  
  
  ###########################################BAR CHART####################################
  Mode <- function(suicide_data)
  {
    ux <- unique(suicide_data)
    ux[which.max(tabulate(match(suicide_data, ux)))]
  }
  for (i in suicide_data) 
  {
    print("COUNTRY")
    print(Mode(suicide_data$country))
    print("YEAR")
    print(Mode(suicide_data$year))
    print("SEX")
    print(Mode(suicide_data$sex))
    print("AGE")
    print(Mode(suicide_data$age))
    print("SUICIDE_NO")
    print(Mode(suicide_data$suicides_no))
    print("POPULATION")
    print(Mode(suicide_data$population))
    print("SUICIDE 100k POP")
    print(Mode(suicide_data$suicides.100k.pop))
    print("GDP FOR YEAR")
    print(Mode(suicide_data$gdp_for_year....))
    print("GDP  PER CAPITA")
    print(Mode(suicide_data$gdp_per_capita....))
    print("GENERATION")
    print(Mode(suicide_data$generation))
    
  }
  
  counts<-table(suicide_data$country)
  barplot(counts, main="Country", beside = TRUE, legend = TRUE)
  
  counts_gender<-table(suicide_data$sex)
  barplot(counts_gender, main="Gender", beside = TRUE, legend = TRUE)
  
  
  counts_year<-table(suicide_data$year)
  barplot(counts_year, main="Year", beside = TRUE, legend = TRUE)
  
  
  counts_generation<-table(suicide_data$generation)
  barplot(counts_generation, main="Year", beside = TRUE, legend = TRUE)

#################PercentPieChart###################################
  x<-subset(suicide_data$suicides_no,suicide_data$sex=="male")
  x<-sum(x)
  y<-subset(suicide_data$suicides_no,suicide_data$sex=="female")
  y<-sum(y)
  z<-c(x,y)
  piepercent<- round(100*z/sum(z), 1)
  piepercent
  pie(z, labels = piepercent ,col = rainbow(length(z)),main="Percentage of Men & Women Commited Suicide",sub="From 1985 to 2016") 
  legend("topright", c("Men","Women"), cex = 1.5,ncol=3,fill = rainbow(length(z)))
  ############################################################################
  
  ############################GGPLOT######################################
  
  NO.Of_Suicidies<-suicide_data$suicides_no
  Year<-suicide_data$year
  Sex<-factor(suicide_data$sex)
  ggplot(suicide_data,aes(x=NO.Of_Suicidies,y=Year,color=Sex),)+geom_point(shape=19)+ labs(x="No.of Suicides",y="year",title="No. of Suicide In Each Year")
  
  
#########################################################################
  
######################################HISTROGRAM#####################################  
  suicideRate = (suicide_data$suicides_no/suicide_data$population)*100
  ggplot(suicide_data,aes(x = suicideRate, fill = age, alpha = 0.5)) + 
    geom_histogram()+labs(title = "Distribution of suicide rates across age groups",
                          x="Suicide rates (in %age)")
  
  
###########################################################################################
  
###########################################################################################
  Generation<-suicide_data$generation
  suicideRate = (suicide_data$suicides_no/suicide_data$population)*100
  ggplot(suicide_data,aes(x = suicideRate, fill =Generation, alpha = 0.5)) + 
    facet_wrap(~ suicide_data$generation) +
    guides(alpha = FALSE) +
    geom_histogram()+labs(title = "Distribution of suicide rates across Generation",
                          x="Suicide rates (in %age)")
  
  
  
  
  
  

  }
  new.function()
