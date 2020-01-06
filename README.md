# Using Google Trends to find top resolutions for England
* Trends data from google, using the gtrendsR package and Google AI Platforms Notebook (R-flavored)
* Defining "top resolution" as area with search intrest that peaks in weeks 52, 53, and 1
* Guided by survey results
  * TODO: determine these resolutions w/ statistics... trend analysis, factor analysis to collapse like terms(?)
  
# References: 
* https://www.r-bloggers.com/understanding-new-years-resolutions-with-google-trends-in-r/
* https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf
* https://cran.r-project.org/web/packages/ggseas/vignettes/ggseas.html
* https://support.google.com/trends/answer/4365533?hl=en
* https://poseidon01.ssrn.com/delivery.php?ID=908065002113091025029029002112016092002057081068083017073028122026122026111000076025031050122061114096018115065008093017080070000023046060084070116010087092079074119027093041070085004083002081123010108064124090072077011111005024007069087073120003022093&EXT=pdf

```R
install.packages("gtrendsR")
library(scales)
library(gtrendsR) 
library(ggplot2)
library(lubridate)
library(dplyr)
```
```R
# quick look at last trend over last 5y
res <- gtrends(c("gym", "diet", "sleep"), geo = c("GB-ENG"), time = c("today+5-y"), gprop = c("web"), onlyInterest = TRUE)
plot(res)
```
![trend](https://github.com/unterbrink/2020trends/blob/master/graphs/output_2_0.png?raw=true)
```R
# resolution from Google Trends varies by amount of time selected... so, stitch together 2 5-year ranges...
res5yr <- gtrends(c("gym", "diet", "sleep"), geo = c("GB-ENG"), time = c("2015-01-04 2020-01-05"), gprop = c("web"), onlyInterest = TRUE)
res10yr <- gtrends(c("gym", "diet", "sleep"), geo = c("GB-ENG"), time = c("2009-12-27 2014-12-28"), gprop = c("web"), onlyInterest = TRUE)

res <- bind_rows(res5yr$interest_over_time, res10yr$interest_over_time)
```
```R
# Plot
options(repr.plot.width=16, repr.plot.height=16)

plot<-ggplot(data=res, aes(x=as.Date(date), y=hits,group=keyword,col=keyword))+
        geom_line()+xlab('Time')+ylab('Relative Interest')+ theme_bw()+
        theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+ggtitle("Google Search Volume") +
        scale_x_date(breaks = "2 years", 
                     minor_breaks = "1 year", 
                     date_labels = "%Y",
                     limits = c(min = as.Date("2010-01-01"), max= as.Date("2020-02-15")),
                     expand=c(0,0))
plot
```
![trend](https://github.com/unterbrink/2020trends/blob/master/graphs/output_4_1.png?raw=true)
```R
# center by keyword, scale

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# res2 <- gtrends(c("gym", "diet", "sleep"), geo = c("GB-ENG"), gprop = c("web"), onlyInterest = TRUE)
# res2 <- res2$interest_over_time

resC <- res %>%
        filter(date > as.Date("2014-11-01")) %>%
        group_by(keyword) %>%
        mutate(cent = scale(hits))

head(resC)
summary(resC)
```




          date                          hits            geo           
     Min.   :2014-11-02 00:00:00   Min.   : 19.00   Length:810        
     1st Qu.:2016-02-14 00:00:00   1st Qu.: 33.00   Class :character  
     Median :2017-05-31 12:00:00   Median : 38.00   Mode  :character  
     Mean   :2017-05-31 12:00:00   Mean   : 44.73                     
     3rd Qu.:2018-09-16 00:00:00   3rd Qu.: 57.75                     
     Max.   :2019-12-29 00:00:00   Max.   :100.00                     
         time             keyword             gprop              category
     Length:810         Length:810         Length:810         Min.   :0  
     Class :character   Class :character   Class :character   1st Qu.:0  
     Mode  :character   Mode  :character   Mode  :character   Median :0  
                                                              Mean   :0  
                                                              3rd Qu.:0  
                                                              Max.   :0  
        weeknum         yearnum        monthnum         nye         
     Min.   : 1.00   Min.   :2014   Min.   : 1.0   Min.   :0.00000  
     1st Qu.:14.00   1st Qu.:2016   1st Qu.: 4.0   1st Qu.:0.00000  
     Median :27.50   Median :2017   Median : 7.0   Median :0.00000  
     Mean   :27.31   Mean   :2017   Mean   : 6.7   Mean   :0.04074  
     3rd Qu.:41.00   3rd Qu.:2018   3rd Qu.:10.0   3rd Qu.:0.00000  
     Max.   :53.00   Max.   :2019   Max.   :12.0   Max.   :1.00000  
          cent         
     Min.   :-2.34607  
     1st Qu.:-0.66065  
     Median :-0.06888  
     Mean   : 0.00000  
     3rd Qu.: 0.47304  
     Max.   : 4.67653  



```R
plot<-ggplot(data=resC, aes(x=as.Date(date), y=cent, group=keyword,col=keyword))+
        geom_line()+xlab('Time')+ylab('Relative Interest')+ theme_bw()+
        theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+ggtitle("Google Search Volume")

plot
plot + facet_grid(keyword~.)
```
```R
# Encode var for new years -- 1 week pre and post Jan1
res$weeknum <- week(res$date)
res$yearnum <- year(res$date)
res$monthnum <- month(res$date)

# res[res$weeknum == 53, "weeknum"] <- 52

res$nye <- 0
res[res$weeknum == 52, "nye"] <- 1
res[res$weeknum == 1, "nye"] <- 1

summary(res)

nyeplot <- ggplot(data = res, aes(x = as.factor(nye), y = hits, fill = keyword)) +
    geom_boxplot()
nyeplot

```

          date                 hits            geo                time          
     Min.   :2009-12-27   Min.   : 16.00   Length:1569        Length:1569       
     1st Qu.:2012-06-24   1st Qu.: 33.00   Class :character   Class :character  
     Median :2014-12-28   Median : 41.00   Mode  :character   Mode  :character  
     Mean   :2014-12-28   Mean   : 45.57                                        
     3rd Qu.:2017-07-02   3rd Qu.: 58.00                                        
     Max.   :2019-12-29   Max.   :100.00                                        
       keyword             gprop              category    weeknum     
     Length:1569        Length:1569        Min.   :0   Min.   : 1.00  
     Class :character   Class :character   1st Qu.:0   1st Qu.:14.00  
     Mode  :character   Mode  :character   Median :0   Median :27.00  
                                           Mean   :0   Mean   :26.65  
                                           3rd Qu.:0   3rd Qu.:40.00  
                                           Max.   :0   Max.   :53.00  
        yearnum        monthnum           nye         
     Min.   :2009   Min.   : 1.000   Min.   :0.00000  
     1st Qu.:2012   1st Qu.: 4.000   1st Qu.:0.00000  
     Median :2014   Median : 7.000   Median :0.00000  
     Mean   :2014   Mean   : 6.532   Mean   :0.04015  
     3rd Qu.:2017   3rd Qu.:10.000   3rd Qu.:0.00000  
     Max.   :2019   Max.   :12.000   Max.   :1.00000  

```R
# Regression time
res2 <- filter(res, year(date) > 2014)

linmod <- lm(hits ~ date + 
             keyword *
             nye,
             data = res2)
summary(linmod)

res2$predicted <- predict(linmod)   # Save the predicted values
res2$residuals <- residuals(linmod) # Save the residual values

ggplot(res2, aes(x = date, y = hits, color = keyword)) + 
    geom_point() +
    geom_line(aes(y = predicted))
```


    
    Call:
    lm(formula = hits ~ date + keyword * nye, data = res2)
    
    Residuals:
        Min      1Q  Median      3Q     Max 
    -18.506  -3.213  -0.303   2.955  37.144 
    
    Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
    (Intercept)      -2.490e+01  7.441e+00  -3.346 0.000859 ***
    date              3.994e-08  4.957e-09   8.058 2.93e-15 ***
    keywordgym        2.774e+01  5.642e-01  49.162  < 2e-16 ***
    keywordsleep     -8.008e-01  5.642e-01  -1.419 0.156195    
    nye               1.193e+01  2.038e+00   5.854 7.10e-09 ***
    keywordgym:nye    1.106e+01  2.882e+00   3.838 0.000134 ***
    keywordsleep:nye -2.599e+00  2.882e+00  -0.902 0.367464    
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 6.32 on 776 degrees of freedom
    Multiple R-squared:  0.831,	Adjusted R-squared:  0.8297 
    F-statistic: 635.9 on 6 and 776 DF,  p-value: < 2.2e-16

```R
weekplot <- ggplot(data = res, aes(x = as.factor(weeknum), y = hits, fill = keyword)) +
    geom_boxplot()
weekplot
weekplot + facet_grid(keyword~.)

monthplot <- ggplot(data = res, aes(x = as.factor(monthnum), y = hits, fill = keyword)) +
    geom_boxplot()
monthplot
monthplot + facet_grid(keyword~.)
```
