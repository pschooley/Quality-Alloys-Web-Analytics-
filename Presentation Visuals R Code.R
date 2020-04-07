#R Code used to make presentation visuals 
#code is in order of each visual in powerpoint presentation

# Sales Promotion Through Website Line Graph

library (ggplot2)
library (readxl)

WeeklyVisits <- read_excel("Web_Analytics_Case.xls", sheet='Weekly Visits', skip = 4, col_names = TRUE)
Financials <- read_excel("Web_Analytics_Case.xls", sheet='Financials', skip = 4, col_names = TRUE)
Lbs_Sold <- read_excel("Web_Analytics_Case.xls", sheet='Lbs. Sold', skip = 4, col_names = TRUE)
DailyVisits <- read_excel("Web_Analytics_Case.xls", sheet='Daily Visits', skip = 4, col_names = TRUE)



weeks <- WeeklyVisits$`Week (2008-2009)`
visits<- WeeklyVisits$Visits
profit<- Financials$Profit

obs <- data.frame(weeks,visits,profit)



# Adding a new column called WeekNo
obs$WeekNo[1] <- 1
for(i in 2:nrow(obs)) {
        obs$WeekNo[i] <- obs$WeekNo[i-1] + 1}

# Dropping Week column
obs = subset(obs, select = -c(1) )


# Using WeekNo as X
p <- ggplot(obs, aes(x = WeekNo))

# adding the profit data
p <- p + geom_area(aes(y = profit, colour = "profit"), fill= "green4", alpha = 0.2)

# adding the visits data, transformed to match roughly the range of the profit
p <- p + geom_line(aes(y = visits*25, colour = "visits"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~ 0.04*., name = "visits"))

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("green4", "blue"))
p <- p + labs(y = "profit",
              x = "WeekNo",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
p

#Comparison vs Industry
Comparison <- data.frame(y=c('Industry Bounce Rate','QA Bounce Rate','Industry Visit2Inquiry','QA Visit2Inquiry'),x=c(55.2,67.2,2.2,0.8))
q <- ggplot(data=Comparison, aes(x=reorder(y,x), y=x)) +  geom_bar(colour="black", fill="steelblue2",stat="identity") + coord_flip()
q <- q + theme(axis.title = element_blank())
q

#Visits by Region Graph
regions <- data.frame(Regions = c('South America','Northern America','Central America',
                                  'Western Europe','Eastern Asia','Northern Europe',
                                  'Southern Asia','South-Eastern Asia','Southern Europe',
                                  'Eastern Europe'),
                      Visits  = c(22616,17509,6776,5214,3228,2721,2589,1968,1538,1427))
f <- ggplot(data=regions, aes(x=reorder(Regions,Visits), y=Visits)) +  geom_bar(colour="black", fill="palegreen2",stat="identity") + coord_flip()
f <- f + xlab("Regions")
f

#Referral Sites Graph
sites <- data.frame(refsites=c('googleads.g.doubleclick.net','pagead2.googlesyndication.com','sedoparking.com','globalspec.com','searchportal.information.com','freepatentsonline.com','thomasnet.com','mu.com','mail.google.com','psicofxp.com'),
                    Visits=c(15626,8044,3138,693,582,389,379,344,337,310))
t <- ggplot(data=sites, aes(x=reorder(refsites,-Visits), y=Visits)) +  geom_bar(colour="black", fill='goldenrod1',stat="identity") 
t <- t + theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5)) + ggtitle("Visits by referral site")
t <- t + theme(axis.title = element_blank())
t

