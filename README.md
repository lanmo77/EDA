# EDA






# Intruduction

The main aim for this analysis is trying to find some hidden geographic pattern and social economic relationship of environmental quality. The EQI refers to the measurement that estimates overall environmental quality at the county level for the United States.

# EQI
## Data Preparation
  
Data reference: https://developer.epa.gov/usepa-environmental-quality-index-eqi-and-associated-domain-indices-by-county-for-the-united-states/
  
This dataset contains the finalized Environmental Quality Index (EQI), and an index for each of the associated domains (air, water, land, built environment, and sociodemographic environment).     
```{r, warnings=FALSE,message=FALSE}
library(googleVis)
library(dplyr)
library(tidyr)
library(plotly)
#setwd("~/Desktop/DATA VIS PROJECT")

EQI=read.csv("EQI_RESULTS_2013JULY22.CSV")
EQI=EQI[,c(1:10)] # Since there are lots of missing values after 11th column, I will only keep the first 10
sum(is.na(EQI)) # Check for NA's
dim(EQI)


# rucc oissrural-urban continuum codes
names(EQI)=c('stfips','county_name','state','rucc','air','water','land','sociod','built','EQI') 


EQI_state=EQI%>%
  group_by(state)%>%
  summarize(
    County.count=n(),
    Air=mean(air),
    Water=mean(water),
    Land=mean(land),
    Sociod=mean(sociod),
    Built=mean(built),
    EQI=mean(EQI)
  )
save(EQI_state,file='EQI_state.RData')

EQI_rucc=EQI%>%
  group_by(rucc)%>%
  summarize(
    county.count=n(),
    Air=mean(air),
    Water=mean(water),
    Land=mean(land),
    Sociod=mean(sociod),
    Built=mean(built),
    EQI=mean(EQI)
  )
save(EQI_rucc,file='EQI_rucc.Rdata')
```


# Basic Analysis
## Star Plot
```{r}
state_name=as.character(unlist(EQI_state[,1]))
palette(rainbow(6, s = 0.6, v = 0.75))
stars(EQI_state[,3:8],labels = state_name,draw.segments = T,main='Star Plot of EQI ')
legend("bottomright",  title="EQI Index",
  	c('Air','Water','Land','Sociod','Built','EQI'),fill=rainbow(6, s = 0.6, v = 0.75))
```
  
  
<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="https://github.com/lanmo77/EDA/blob/master/fig/Starplot.png?raw=true" float = "center" ALT="image" width="400">
  </span>
</div>
  
From this plot we can see the value of different index vary a lot from states to states. One possible reason for the difference between AK and other states might be the uniqe geographic location.



  
  
# MDS
```{r}

D=dist(EQI_state[,3:8])
EQI_MDS=cmdscale(D,k=2,eig = T)
EQI.x=EQI_MDS$points[,1]
EQI.y=EQI_MDS$points[,2]
EQI.df=data_frame(EQI.x,EQI.y)

p=ggplot(EQI.df,aes(x=EQI.x,y=EQI.y,label=state_name))+geom_point()+
  xlab('Coordinate 1')+ ylab('Coordinate 2')+
  ggtitle('MDS Visualization')+geom_label()

p


```

  
<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="https://github.com/lanmo77/EDA/blob/master/fig/MDS.png?raw=true" float = "center" ALT="image" width="400">
  </span>
</div>
  

From this plot we can see that HI and AK are the outliers. Other states clustered together. The main aim for this plot is try to find some relationship between the two index and their geographic lobation. But actually, we cannot see a very obvious relationship between them.



# Bubble plot
data reference: http://www.usgovernmentspending.com/gdp_by_state  

```{r}
deathrate=read.csv("raw_data.csv", header=FALSE)
names(deathrate)=c('state','death')
gdp=read.csv("usgs_state_2016.csv")[-45,c(1,6)]
names(gdp)=c('state','gdp')
abb=read.csv("us_states.csv", header=FALSE)[,c(2,3)]
abb=data.frame(matrix(unlist(abb),ncol=2))
names(abb)=c('state','abb')
deathrate=left_join(deathrate,abb,by='state')
gdp=left_join(gdp,abb,by='state')

data4=left_join(deathrate,gdp,by='abb')[,c(3,2,5)]
names(data4)[1]='state'
data4=left_join(data4,EQI_state,by='state')


plot_ly(data4, x = gdp, y = EQI, text = state,
        mode = "markers", color = death, size = death, opacity = death)

```

  
<div style="text-align: center;" align = "center">
  <span class="slide" style="float:center;width: 80%;">
  <IMG SRC="https://github.com/lanmo77/EDA/blob/master/fig/Bubble.png?raw=true" float = "center" ALT="image" width="400">
  </span>
</div>
  

From this bubble plot we can see that states with high gpd and EQI have less death rate. We cannot draw a final conclusion from this plot that they are the causes of death but this plot shows a interesting research direction.

```{r}




```





