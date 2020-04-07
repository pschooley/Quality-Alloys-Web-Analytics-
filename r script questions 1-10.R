# Quantitative Analysis Web Quality Alloys Web Analytics Case for Team 7
#Analysis Questions 1-10

# Loading sheets for questions 1-2 from excel file my_df
library("readxl")
library("stringr")
library("ggplot2")
# xlsx files
library(readxl)
my_df1 <- read_excel(
        "Web_Analytics_Case.xls",
        sheet = "mydf_1",
        col_types = c("text",
                      "numeric", "numeric", "numeric",
                      "numeric")
)
View(my_df1)

#############################################################################################
# 1. Using data in the Weekly Visits and Financials worksheets, creat four column charts for
# unique visits, profit over time, and pounds sold. You do not have to indicate on these charts the cutoffs
# for the four periods
#############################################################################################                                                                                               "numeric", "numeric"))

# Create the four barplots for the chart, export, and save

#Unique Visits
barplot(
        my_df1$`Unique Visits`,
        names.arg = my_df1$`Week (2008-2009)`,
        main = "FIGURE 1. UNIQUE WEBSITE VISITS PER WEEK 2008-2009",
        ylab = "Unique Visits",
        col = c("Blue"),
        las = 2,
        cex.names = 0.6,
        cex.axis = 0.5,
        space = 0.3,
        cex.main = 0.75
)
#Revenue
barplot(
        my_df1$Revenue,
        names.arg = my_df1$`Week (2008-2009)`,
        main = "FIGURE 2. QA REVENUE ($) PER WEEK 2008-2009",
        ylab = "Revenue ($)",
        col = c("Blue"),
        las = 2,
        cex.names = 0.6,
        cex.axis = 0.5,
        space = 0.3,
        cex.main = 0.75
)
#Profit
barplot(
        my_df1$Profit,
        names.arg = my_df1$`Week (2008-2009)`,
        main = "FIGURE 3. QA PROFIT ($) PER WEEK",
        ylab = "Profit ($)",
        col = c("Blue"),
        las = 2,
        cex.names = 0.6,
        cex.axis = 0.5,
        space = 0.3,
        cex.main = 0.75
)
#Lbs. Sold
barplot(
        my_df1$`Lbs. Sold`,
        names.arg = my_df1$`Week (2008-2009)`,
        main = "FIGURE 4. LBS. SOLD BY QA PER WEEK",
        ylab = "Lbs. Sold",
        col = c("Blue"),
        las = 2,
        cex.names = 0.6,
        cex.axis = 0.5,
        space = 0.3,
        cex.main = 0.75
)

#############################################################################################
# 2. Using the same data, calculate the following summary statistics for visits, unique visits, revenue,
# profit, lbs. sold: mean, median, std. deviation, minimum, maximum
# for all four periods: initial, pre-promotion, promotion, post-promotion
#############################################################################################
#reading in dataset
library(readxl)
my_df <- read_excel(
        "Web_Analytics_Case.xls",
        sheet = "my_df2",
        col_types = c(
                "numeric",
                "numeric",
                "numeric",
                "numeric",
                "numeric",
                "numeric"
        )
)
View(my_df)
#df shows visits, unique visits,revenue, profit, and Lbs sold overtime
#grouping by time period and viewing

initial <- my_df[1:14,]
pre_promotion <- my_df[15:35,]
promotion <- my_df[36:52,]
post_promotion <- my_df[53:66,]

View(pre_promotion)
View(initial)
View(promotion)
View(post_promotion)

#calculating summary stats for 5 given variables in different stages
library("formattable")
library("fBasics")

stats_initial <-
        basicStats(initial)[c("Mean", "Median", "Stdev", "Minimum", "Maximum"),]
stats_pre_promotion <-
        basicStats(pre_promotion)[c("Mean", "Median", "Stdev", "Minimum", "Maximum"),]
stats_promotion <-
        basicStats(promotion)[c("Mean", "Median", "Stdev", "Minimum", "Maximum"),]
stats_post_promotion <-
        basicStats(post_promotion)[c("Mean", "Median", "Stdev", "Minimum", "Maximum"),]

#setting row and column names
rownames(stats_initial) <-
        c("Mean", "Median", "Std. Dev", "Minimum", "Maximum")
rownames(stats_pre_promotion) <-
        c("Mean", "Median", "Std. Dev", "Minimum", "Maximum")
rownames(stats_promotion) <-
        c("Mean", "Median", "Std. Dev", "Minimum", "Maximum")
rownames(stats_post_promotion) <-
        c("Mean", "Median", "Std. Dev", "Minimum", "Maximum")
colnames(stats_initial) <-
        c("Visits",
          "Unique Visits",
          "Page Views",
          "Revenue",
          "Profit",
          "Lbs. Sold")
colnames(stats_pre_promotion) <-
        c("Visits",
          "Unique Visits",
          "Page Views",
          "Revenue",
          "Profit",
          "Lbs. Sold")
colnames(stats_promotion) <-
        c("Visits",
          "Unique Visits",
          "Page Views",
          "Revenue",
          "Profit",
          "Lbs. Sold")
colnames(stats_post_promotion) <-
        c("Visits",
          "Unique Visits",
          "Page Views",
          "Revenue",
          "Profit",
          "Lbs. Sold")

#formating tables
initial_table <- formattable(stats_initial)
promotion_table <- formattable(stats_promotion)
pre_promotion_table <- formattable(stats_pre_promotion)
post_promotion_table <- formattable(stats_post_promotion)

#Rounding to two decimal places, exporting, and saving
table_one <- round(initial_table, 2)
table_one
table_two <- round(pre_promotion_table, 2)
table_two
table_three <- round(promotion_table, 2)
table_three
table_four <- round(post_promotion_table, 2)
table_four
View(pre_promotion_table)
View(promotion_table)
View(post_promotion_table)


#############################################################################################
# 3.Create a column chart of the mean visits over the four periods—that is,
#Create four more such charts, this time using the mean unique visits, mean revenue, mean profit, and mean pounds
#sold statistics
#############################################################################################

#Reading in the excel file
period_means <-
        read_excel("Web Analytics Case Student Spreadsheet.xls",
                   sheet = "Descrip Analysis",
                   range = "H47:M51")
View(period_means)
names(period_means) <-
        str_replace_all(names(period_means), c(" " = ""))


#Naming the periods and fixing period levels
periods <-
        factor(
                period_means$...1,
                levels = c("Initial", "Pre-Promotion", "Promotion", "Post Promotion")
        )

#Creating bar graph of mean visits over time
Visits_bar <-
        ggplot(data = period_means, aes(x = periods, y = Visits)) +
        geom_bar(stat = "identity")
Visits_bar
#Analysis: Promotion period had the highest visits over time.
#Might indicate marketing efforts were working.

#Creating bar graph of mean unique visits over time
Unique_Visits_bar <-
        ggplot(data = period_means, aes(x = periods, y = UniqueVisits)) +
        geom_bar(stat = "identity")
Unique_Visits_bar

#Creating bar graph of mean revenue over time
Revenue_bar <-
        ggplot(data = period_means, aes(x = periods, y = Revenue)) +
        geom_bar(stat = "identity")
Revenue_bar
#Analysis: Revenue sees a gradual decrease. Peaks in visits do not translate to revenue

##Creating bar graph of mean profits over time
Profit_bar <-
        ggplot(data = period_means, aes(x = periods, y = Profit)) +
        geom_bar(stat = "identity")
Profit_bar
#Analysis: Profit sees a gradual decrease. Peaks in visits do not translate to profit


#Creating bar graph of mean lbs sold over time
Lbs_Sold_bar <-
        ggplot(data = period_means, aes(x = periods, y = LbsSold)) +
        geom_bar(stat = "identity")
Lbs_Sold_bar
#Analysis: Lbs Sold sees a decrease. Peaks in visits do not translate to revenue


#############################################################################################
#4. Write one or two paragraphs summarizing your findings thus far. Be sure to describe the
#behavior of each variable. Indicate what the results seem to show about the relationships
#between the variables, and the apparent effect(s) of the promotion. (In the next section you’ll
#explore this further; feel free to make any conjectures here that seem reasonable.) Be sure to
#support your verbiage with your analysis results.
#############################################################################################

#The initial analysis of the mean visits and unique visits showcases that there was a higher number of clients visiting the website during the promotional period.
#It recorded its highest traffic of about 1,800 for both mean visits and unique visits compared to
#their lowest period (the pre-promotion period) of about 550 visits. This could indicate that
#the marketing strategy used in the promotion was effective in bringing in new clients.
#Questions to ask: Does it translate into revenue and sales? Do the financials indicate that the
#marketing strategy produced a high ROI?

#The initial analysis of the mean revenue, profit and Lbs. sold indicate that the was a decline from the initial period to the post promotion period.
#The Lbs. sold shows a decline during the different periods. Mean revenue saw a drop from about $600,000 to about $370,000 in the post promotion period.
#This translated to the mean profits obtained as well; a drop from about $200,000 to about $110,000.
#It can be seen that the high number of mean visit and unique visits did not translate to revenue
#and profit. Could this be because the website was not designed to allow users to enter orders over the web.
#Questions to ask: Does this data truly represent each periods sales? Further information should be gathered

#############################################################################################
# 5. Start by taking a look at revenue and pounds sold. (Before proceeding, what does your
# intuition say about the relationship between these two variables?)
#Create a scatter diagram of revenue versus pounds sold. (Revenue should be on the y, or vertical, axis.)
#Determine the correlation coefficient of revenue and pounds sold.
#############################################################################################

# Import dataset from sheet "Financials"
Financial_data <-
        read_excel("Web Analytics Case Student Spreadsheet.xls",
                   sheet = "Financials",
                   range = "A5:E71")
View(Financial_data)

# Remove space in column name
names(Financial_data) <-
        str_replace_all(names(Financial_data), c(" " = ""))

# Plot the scatter plot to see the trend between Revenue and Lbs.Sold
ggplot(Financial_data, aes(x = Revenue, y = Lbs.Sold)) + geom_point() + ggtitle("Scatter plot (Revenue and Lbs. Sold)")

# Calculate the corelation between Revenue and Lbs.Sold
Financial_cor = subset(Financial_data, select = c(Revenue, Lbs.Sold))
cor(Financial_cor)

#############################################################################################
#6.Now create the scatter diagram of revenue versus visits. (Given your previous work, what do you expect this plot to look like?)
#Determine the correlation coefficient of revenue and visits.
#############################################################################################

# Import dataset from sheet "Weekly Visits"
Visit_data <-
        read_excel("Web Analytics Case Student Spreadsheet.xls",
                   sheet = "Weekly Visits",
                   range = "A5:H71")
View(Visit_data)

# Merge Financial data and Visit data
All_data <-
        merge(Financial_data, Visit_data, by.x = "Week(2008-2009)", by.y = "Week (2008-2009)")

# Plot the scatter plot to see the trend between Revenue and Visits
ggplot(All_data, aes(x = Revenue, y = Visits)) + geom_point()  + ggtitle("Scatter plot (Revenue and Visits)")


# Calculate the corelation between Revenue and Visits
Revenue_Visits = subset(All_data, select = c(Revenue, Visits))
cor(Revenue_Visits)

#############################################################################################
# 7.Summarize your results. In particular, elaborate on the implications of the relationship
#between revenue and number of visits to the website. Feel free to examine any other variable pairs
#you think might be important.
#############################################################################################

# Calculate the corelation for all variable
All_data_cor <- All_data[-c(1)]
All_data_cor <- cor(All_data_cor)
View(All_data_cor)

#############################################################################################
#8. QA is interested in modeling data critical to their business. For example, if data for a
#particular variable appears to be reasonably approximated by a normal distribution, with a
#predictable mean and standard deviation, future values for that variable can be reasonably
#estimated. The purpose of the following exercise is to pursue this modeling process.
#The Lbs. Sold worksheet contains the pounds of material sold per week from January 3, 2005,
#through the week of July 19, 2010.
#############################################################################################
library (readxl)

# Read the data, skip first lines
lbs_sold <-
        read_excel(
                "Web Analytics Case Student Spreadsheet.xls",
                sheet = 'Lbs. Sold',
                skip = 4,
                col_names = TRUE
        )

#############################################################################################
# a) Determine the following summary values for this data: mean, median, standard
# deviation, minimum, and maximum.
#############################################################################################

# get mean, median,  quartiles
summary(lbs_sold$`Lbs. Sold`)

# get standard deviation
std_dev <- round(sd(lbs_sold$`Lbs. Sold`), 2)

#############################################################################################
# b) Create a histogram of the pounds of material sold data
#############################################################################################

# plot histogram of data
hist(lbs_sold$`Lbs. Sold`)

#############################################################################################
# c) Describe the histogram. Does it appear bell-shaped?
#############################################################################################
"Yes the data is represented in a bell shaped=esque form around 18.6k as center."
"A little skewed towards the upper-half which invites for a clarification using"
"empyrical rule method for example."

#############################################################################################
# d) Determine how well this data follows the Empirical Rule by completing the following table
#############################################################################################

# Compute values required for Z-score and Actual observation sumation
mean <- round(mean(lbs_sold$`Lbs. Sold`), 2)
observations <- nrow(lbs_sold)


lbs_sold$'z-score' <- (lbs_sold$`Lbs. Sold` - mean) / std_dev

# sort by z-score
sorted_LbsSold <- lbs_sold[order(lbs_sold$`z-score`), ]

# group by std deviation level
sorted_LbsSold$StdGroup <-
        ifelse(abs(sorted_LbsSold$`z-score`) <= 1, 1,
               ifelse(abs(sorted_LbsSold$`z-score`) <= 2, 2,
                      3))

# generate Empyrical Rule table
Interval <-
        c('mean ? 1 std. dev.', 'mean ? 2 std. dev.', 'mean ? 3 std. dev.')
Theorical_data_percentage <- c(0.68, 0.95, 0.99)
Theorical_Obs <- observations * Theorical_data_percentage
Actual_Obs <-
        c(
                sum(sorted_LbsSold$StdGroup == 1),
                sum(sorted_LbsSold$StdGroup < 3),
                sum(sorted_LbsSold$StdGroup < 4)
        )

EmpyRule <-
        data.frame(Interval,
                   Theorical_data_percentage,
                   Theorical_Obs,
                   Actual_Obs)

#############################################################################################
# e) Refine your analysis by completing the following table for the pounds sold data.
#############################################################################################
Refined_Interval <-
        c(
                'mean + 1 std. dev.',
                'mean - 1 std. dev.',
                '1 std. dev. to 2 std. dev.',
                '-1 std. dev. to -2 std. dev.',
                '2 std. dev. to 3 std. dev.',
                '-2 std. dev. to -3 std. dev.'
        )
Refined_Theorical_data_percentage <-
        c(0.34, 0.34, 0.135, 0.135, 0.02, 0.02)
Refined_Theorical_Obs <-
        observations * Refined_Theorical_data_percentage

# group by std deviation level
sorted_LbsSold$Refined_StdGroup <-
        ifelse(
                sorted_LbsSold$`z-score` <= -2,
                -3,
                ifelse(
                        sorted_LbsSold$`z-score` <= -1,
                        -2,
                        ifelse(
                                sorted_LbsSold$`z-score` <=  0,
                                -1,
                                ifelse(
                                        sorted_LbsSold$`z-score` <=  1,
                                        1,
                                        ifelse(sorted_LbsSold$`z-score` <=  2,  2,
                                               3)
                                )
                        )
                )
        )

# computing sum for every level
Refined_Actual_Obs <-
        c(
                sum(sorted_LbsSold$Refined_StdGroup == 1),
                sum(sorted_LbsSold$Refined_StdGroup == -1),
                sum(sorted_LbsSold$Refined_StdGroup == 2),
                sum(sorted_LbsSold$Refined_StdGroup == -2),
                sum(sorted_LbsSold$Refined_StdGroup == 3),
                sum(sorted_LbsSold$Refined_StdGroup == -3)
        )



Refined_EmpyRule <-
        data.frame(
                Refined_Interval,
                Refined_Theorical_data_percentage,
                Refined_Theorical_Obs,
                Refined_Actual_Obs
        )

#############################################################################################
# f) How well does the data for pounds of material sold seem to follow the normal
# (bell-shaped) distribution?
#############################################################################################
"The data skewness is more noticeable from this distribution"
"However, using the new distribution, its histogram still holds a bell-shaped form"
"Still skewed towards the left but hopefully not in a significant way to consider other dist."

hist(sorted_LbsSold$StdGroup)
hist(sorted_LbsSold$Refined_StdGroup)

#############################################################################################
# g) Determine the skewness and kurtosis for the pounds sold data. Are these values
# consistent with your analysis of the pounds of material sold data?
#############################################################################################
library(e1071)

# computint skewness and kurtosis
Lbs_skewness <- round(skewness(lbs_sold$`Lbs. Sold`), 4)
Lbs_kurtosis <- round(kurtosis(lbs_sold$`Lbs. Sold`), 4)


#############################################################################################
#9. 9.	As part of the analysis, the number of daily visits to the QA website over the period May 25,
# 2008–August 29, 2009, was also collected. The material below is the output for the daily visits data using the same analysis you did in the previous problem
#for the pounds of material sold per week data. Your task in this problem is to use the two sets of
#output to write a paragraph or two comparing the distribution of the pounds sold data with that of
#the daily visit data. That is, is one more “normal” than the other? How do you know?
#(Note that the daily visit data is provided in the Quality Alloys Data spreadsheet; however,
#you may rely here solely on the output below.)
#############################################################################################

#On comparing the distribution of pounds sold and daily visits data, it becomes clear that the
#distribution of pounds sold is more 'normal' than the distribution of daily visits. The Daily visits
#data is skewed to the right and is more skewed than the pounds sold data. While pounds sold has a
#skewness of 0.6258 and kurtosis of 0.5090, the daily visits has a larger skewness of 2.17 and
#kurtosis of 5.86.

#As we compare both the distributions using the empirical rules, we can clearly observe the difference
#between pounds sold and daily visits. For Dily visits, the first standard deviation from the mean
#was theoretically 312 and the actual observation turned out to be 392. The difference here is much
#larger than that of pounds sold where the theoretical was 197 and actual only 201.
#Similar observations in intervals lead us to understand that the pounds distribution turned out to be
#more normal than daily visits.

#############################################################################################
#10.Represent each set of data graphically. In each case, write a sentence or two capturing
#the main conclusion(s) you draw
#############################################################################################
#All Traffic Sources
library(readxl)
traffic <- read_excel(
        "Web Analytics Case Student Spreadsheet.xls",
        sheet = "all_traffic",
        col_types = c("text",
                      "numeric")
)
View(traffic)
#making bar chart
all_traffic_bar <-
        ggplot(data = traffic, aes(x = traffic$`All Traffic Sources`, y = traffic$Visits)) +
        geom_bar(stat = "identity")
#fixing axis labels
print(all_traffic_bar + labs(y = "Number of Visits", x = "All Traffic Sources"))

#Top Ten Referring Sites
library(readxl)
refer <- read_excel(
        "Web Analytics Case Student Spreadsheet.xls",
        sheet = "referring_sites",
        col_types = c("text",
                      "numeric")
)
View(refer)
#making bar chart
refer_sites <-
        ggplot(data = refer, aes(x = refer$`Top Ten Referring Sites`, y = refer$Visits)) +
        geom_bar(stat = "identity")
refer_sites_bar <-
        refer_sites + theme(axis.text.x = element_text(angle = 90))
#fixing axis labels
print(refer_sites_bar + labs(y = "Number of Visits", x = "Top Ten Referring Sites"))


#Top Ten Search Engine Sources of Visits
library(readxl)
engine <- read_excel(
        "Web Analytics Case Student Spreadsheet.xls",
        sheet = "engine",
        col_types = c("text",
                      "numeric")
)
View(engine)
#making bar chart
search_engine <-
        ggplot(data = engine,
               aes(
                       x = engine$`Top Ten Search Engine Sources of Visits`,
                       y = engine$Visits
               )) +
        geom_bar(stat = "identity")
search_engine_bar <-
        search_engine + theme(axis.text.x = element_text(angle = 90))
#fixing axis labels
print(
        search_engine_bar + labs(y = "Number of Visits", x = "Top Ten Search Engine Sources of Visits")
)

#Top Ten Geographic Sources by Sub Continent Region

library(readxl)
geographic <-
        read_excel(
                "Web Analytics Case Student Spreadsheet.xls",
                sheet = "geography",
                col_types = c("text",
                              "numeric")
        )
View(geographic)
#making bar chart
geographic_bar <-
        ggplot(
                data = geographic,
                aes(
                        x = geographic$`Top Ten Geographic Sources by Sub Continent Region`,
                        y = geographic$Visits
                )
        ) +
        geom_bar(stat = "identity")
geo_bar <-
        geographic_bar + theme(axis.text.x = element_text(angle = 90))
#fixing axis labels
print(
        geo_bar + labs(y = "Number of Visits", x = "Top Ten Geographic Sources by Sub Continent Region")
)
#Top Ten Browsers Used
library(readxl)
browse <- read_excel(
        "Web Analytics Case Student Spreadsheet.xls",
        sheet = "browsers",
        col_types = c("text",
                      "numeric")
)
View(browse)
#making bar chart
browsers <-
        ggplot(data = browse, aes(x = browse$`Top Ten Browsers Used`, y = browse$Visits)) +
        geom_bar(stat = "identity")
browser_bar <-
        browsers + theme(axis.text.x = element_text(angle = 90))
#fixing axis labels
print(browser_bar + labs(y = "Number of Visits", x = "Top Ten Browsers Used"))

#Tope Ten Operating Systems Used

library(readxl)
ops <- read_excel(
        "Web Analytics Case Student Spreadsheet.xls",
        sheet = "operating",
        col_types = c("text",
                      "numeric")
)
View(ops)

#making bar chart
operating <-
        ggplot(data = ops, aes(
                x = ops$`Top Ten Operating Systems Used`,
                y = ops$Visits
        )) +
        geom_bar(stat = "identity")
operating_bar <-
        operating + theme(axis.text.x = element_text(angle = 90))
#fixing axis labels
print(operating_bar + labs(y = "Number of Visits", x = "Top Ten Browsers Used"))
