#### ED & STATISTICS 2024
#### STATISTICAL PROGRAMMING ASSIGNMENT 1
#### GETTING STARTED IN R, BASIC PROGRAMMING, DATA WRANGLING
#### DATA VISUALIZATION

#### SECTION 1 (follow instructions from outside/inside R)

#### SECTION 2 DATA WRANGLING AND PLOTTING


# Step 2.2) Creating objects
# a) Create a vector object called "integer.v" with integers from 1 to 100
integer.v <-c(1:100)  

# 2) Create a vector of words called "factor.v" containing the names of 5 colours 
factor.v <-c("red", "black", "blue", "yellow", "white")

# Step 2.3) Installing packages 
# install.packages("tidyverse") # for data wrangling and plotting
# install.packages("here")# for saving/reading files to/from the directory we chose
# a) load them
library(tidyverse)
library(here)

# Step 2.4) Load datafile
beaches<-read.csv(here("data", "sydneybeaches.csv"))
View(beaches)

# Step 2.5) Preview basic information from this data set 
# (These are just different options you can choose from in future)
str(beaches)
glimpse(beaches)
head(beaches)
tail(beaches)

# Step 2.6) Obtain a basic summary of your data set
summary(beaches)

# Cleaning up the data frame
#install.packages(janitor)
library(janitor)

# Step 2.7) Wrangling: tidying the data-set up check the starting point
names(beaches)
# a) Reformat column names to be consistently written fully in UPPER CASE 
beaches<-select_all(beaches, toupper)
# b) Reformat column names to be consistently  written fully in lower case
beaches<-select_all(beaches, tolower)
# c) Reformat column names to be in lower case and change spaces to underscores
beaches<-clean_names(beaches)
# d) rename the last column name (its the response variable and we don't want to type 
# that long thing over and over. 
# in parenthesis enter (name of file, newname = oldname) 
beaches<-rename(beaches, entnumber = enterococci_cfu_100ml)
# check out how the change has been made:
names(beaches)

# Step 2.8) Learn to use "the pipe" argument %>% to make many operations in one step
# Clear console and environment first (to start with an unchanged file again)
# and reload the dataset:
beaches<-read.csv(here("data", "sydneybeaches.csv"))
# this is how piping works: 
# new_object<-old_object %>% 
#             do this thing () %>% 
#             then do this thing () %>% 
#             and lastly do this thing () 
cleanbeaches<-beaches%>% # (take beaches pipe it in to the below and name it clean beaches)
              clean_names()%>%
              rename(entnumber = enterococci_cfu_100ml)

# Step 2.9) Exploration of rows of the dataset
# a) Filter the data: Which beach has the most extreme levels of bacteria?
cleanbeaches<-cleanbeaches%>%
              arrange(desc(entnumber))
# b) if you don't specify "desc" the default is to order the data from lowest to highest
cleanbeaches<-cleanbeaches%>% 
              arrange(entnumber)
# c) Lets suppose that we live at "Coogee Beach" and we want to query the data from there, 
# lets use the functions arrange and filter together: 
cgbeach<-cleanbeaches%>% 
          filter(site =="Coogee Beach")%>% 
          arrange(desc(entnumber))
# d) Filter data from more than one site. When you want to do this, you can use the 
# %in% operator like this:
cg.and.bon<-cleanbeaches%>%
            filter(site %in% c("Coogee Beach", "Bondi Beach"))%>%
            arrange(desc(entnumber))

# Step 2.10) Obtaining summary statistics
# There are better ways to compare beaches than to sort them. 
# Lets use the summarise function, which is really useful, especially used in combination 
# with the function group_by
# with summarise you can get, mean, maximum, standard deviation, and more
# From your basic summaries above, you may remember we have some NAs (missing values) 
# that will mess up any calculation. Inserting the na.rm = TRUE 
# instructs R to exclude NAs from all calculations

# a) Filter the two beaches and obtain the statistics max, mean and sd
summarystats<-cleanbeaches%>%
              filter(site %in% c("Coogee Beach", "Bondi Beach"))%>%
              group_by(site)%>%
              summarise(maxbacteria = max(entnumber, na.rm = TRUE), 
                        meanbacteria = mean(entnumber, na.rm = TRUE), 
                        sdbacteria = sd(entnumber, na.rm = TRUE))

# b) actually you can just get this nice summary table for all beaches to see 
# Coongee and Bondi in a larger context by just "shading" the filtering instruction. 
# Remember that everything starting with a "#" is commented out and not read as code: 
summarystats<-cleanbeaches%>%
              #filter(site %in% c("Coogee Beach", "Bondi Beach"))%>%
              group_by(site)%>%
              summarise(maxbacteria = max(entnumber, na.rm = TRUE), 
                        meanbacteria = mean(entnumber, na.rm = TRUE), 
                        sdbacteria = sd(entnumber, na.rm = TRUE))

# c) this is how the summary by council with mean, sd and median would look like
summarystats2<-cleanbeaches%>%
              group_by(council)%>%
              summarise(meanbacteria = mean(entnumber, na.rm = TRUE), 
                        sdbacteria = sd(entnumber, na.rm = TRUE), 
                        medbacteria = median(entnumber, na.rm = TRUE))

# d) You can also compute summary statistics grouping by council and site:
summarystats3<-cleanbeaches%>%
                group_by(council, site)%>%
                summarise(meanbacteria = mean(entnumber, na.rm = TRUE), 
                sdbacteria = sd(entnumber, na.rm = TRUE), 
                medbacteria = median(entnumber, na.rm = TRUE))

# Step 2.11) Reshape files from long - to wide format and back 
# a) load new data:
wbeaches<-read.csv(here("data", "widesydneybeaches.csv"))
# View the data set (from very first to very last! rows). You need to transpose it so that 
# sites (which are now column headings) are integrated as the second row (after years), 
# and the numeric values (mean number of enterococci) are shown as a third column named 
# "mean.bugs"

# b) Make the data long using the function gather, entering 3 pieces of information:
# key = site (the column headings that will become rows) 
# Value = the values that will go under new column "mean.bugs"
# the first and very last entry of "site"
longbeaches<-wbeaches%>%
            gather(key = site, value = mean.bugs, "Avalon.Beach":"Woolwich.Baths")

# c) Reshape the file back into wide format using the function "spread" entering:
# key = column of which, values are to become column headings
# value = column that contains the numeric
new.widebeaches<-longbeaches%>%
                 spread(key = site, value = mean.bugs)

# Step 2.12) More data wrangling: creating or collapsing columns in the dataset:  
# using functions separate and unite. 

# a) We will work again with the file sydneybeaches.csv and clean up the columns again 
beaches<-read.csv(here("data", "sydneybeaches.csv"))
cleanbeaches<-beaches%>% 
              clean_names()%>%
              rename(entnumber = enterococci_cfu_100ml)

# b) Separate the date column into day, month, year. 
# note that R has read "date" as a character. Although this is not entirely correct 
# (a package lubridate that we wont learn now can fix this), right now we will
# take use "date" to explain the use of "separate" because character variables 
# can be separated in different columns:
cleanbeaches1<-cleanbeaches%>%
               separate(date, c("day", "month", "year"), remove=FALSE)
# remove = FALSE retains the original "date" variable
# remove = TRUE removes the original "date" variable

# c) Unite data from site and council
cleanbeaches2<-cleanbeaches%>%
               unite(council_site, council:site, remove=FALSE)
# remove = FALSE retains the original variables
# remove = TRUE removes the original variables

# Step 2.13) Compute new variables using function mutate
# Compute a new variable (i.e. the log-transformed version of entnumber)
# for this you use the function "mutate". Not only does it allow you to transform your
# variable (e.g. log-transform), but also to compute new numeric or logical variables

# a) Lets log-transform first
cleanbeaches3<-cleanbeaches%>%
               mutate(logbugs = log(entnumber))
# b) Lets create a new numerical variable 
cleanbeaches3<-cleanbeaches%>%
               mutate(bugdiff = entnumber - lag(entnumber))
# c) Lets create a new LOGICAL (true/false) variable that will tell us whether a certain 
# number of bacteria is higher than the overall mean (true) or not (false)
# because NA values are not handled well when computing means, 
# add the na.rm instruction right at the point of mean calculations
cleanbeaches3<-cleanbeaches%>%
               mutate(higher_all = entnumber > mean(entnumber, na.rm = TRUE))
# May be smart to check that trues/falses are well assigned, so lets compute the 
# overall mean and check the file. 
meanbugs = mean(cleanbeaches$entnumber, na.rm = TRUE)
# mean is = 57.59511
# now view the file and a few rows under higher_all to check for correctness

# d) Practice piping all instructions in one
cleanbeaches_new <-cleanbeaches%>%
                  separate(date, c("day", "month", "year"), remove=FALSE)%>%
                  mutate(logbug = log(entnumber))%>%
                  mutate(bugdiff = entnumber - lag(entnumber))%>%
                  mutate(higher_all = entnumber > mean(entnumber, na.rm = TRUE))%>%
                  group_by(site)%>%
                  mutate(higher_site = entnumber > mean(entnumber, na.rm = TRUE))

# e) Write this last dataset to a file that we will use for plotting
write.csv(cleanbeaches_new, here("data", "cleanbeaches_new.csv"))

#### SECTION 3 BASIC PLOTTING OF RAW DATA USIN GGPLOT
# We will work with the new csv we just created, but start anew. 
# Clean console and environment again now.  

# Step 3.1) Reload packages we need and install a new nice one useful for plotting raw data
# (ggbeeswarm) and read in the dataset
library(tidyverse)
library(here)
#install.packages("ggbeeswarm")
library(ggbeeswarm)
plotbeaches<-read.csv(here("data", "cleanbeaches_new.csv"))

# Step 3.2) Make a first basic ggplot
# What ggplot expects: 
# The name of the dataframe you will use (not needed when you pipe things with %>%)
# aes = Aesthetics: what you want your plot to look like starting with:
# What goes on the x axis, 
# what goes on the y axis, 
# what type of plot you want = boxplot, dot plot, violin?
plotbeaches%>%
      ggplot(aes(x = year, y= entnumber))+
      geom_point() 
# we can see here that R is understanding "year" as an integer and plotting it in a continuous scale: 
str(plotbeaches$year)
# Tell R you want year to create a new object fyear that reads year as a factor:
fyear<-as.factor(plotbeaches$year)
# another way of doing this is by coercing R into reading year as factor (this will overwrite year)
plotbeaches$year <-as.factor(plotbeaches$year)

# Step 3.3) Try out other geometries and configurations 
# a) the geometry "geom_jitter" will show the variation more clearly
plotbeaches%>%
  ggplot(aes(x = year, y= entnumber))+ # or fyear
  geom_jitter()
# b) plot site in the x axis to view variation across site
plotbeaches%>%
  na.omit()%>% # lets do this so we stop seeing the missing values warning
  ggplot(aes(x = site, y= entnumber))+
  geom_jitter()
# c) as the x axis got to crowded try flipping the plot and place sites in the y axis
plotbeaches%>%
  na.omit()%>%
  ggplot(aes(x = site, y= entnumber))+
  geom_jitter()+ 
  coord_flip()
# d) lets start using some colour to distinguish the year information in the plot
plotbeaches%>%
  na.omit()%>%
  ggplot(aes(x = site, y= entnumber, color = year))+ # or fyear
  geom_jitter()+ 
  coord_flip()

# Step 3.4) Faceting plots: although this is somewhat illustrative, there are still lots of dots on top of each other

# a) Plot separately per council and colour the year
plotbeaches%>%
  na.omit()%>% 
  ggplot(aes(x = year, y= entnumber, colour = year))+
  geom_jitter()+
  facet_wrap(~council)
# b) An important think to notice here is that one point in the Waverly council has
# an extreme value (lets don't call it an outlier yet until you go through data exploration)
# lets see if we can remove it to show the patterns of the rest better
plotbeaches%>%
  na.omit()%>%
  filter(entnumber <10000)%>% # shift here between 1000 and 10000 to see the effect
  ggplot(aes(x = year, y= entnumber, colour = year))+
  geom_jitter()+
  facet_wrap(~council)
# c) lets compare two beaches only
plotbeaches%>%
  na.omit()%>%
  filter(entnumber <1000)%>%
  filter(site %in% c("Coogee Beach", "Bondi Beach"))%>%
  ggplot(aes(x = year, y= entnumber, colour = year))+
  geom_jitter()+
  facet_wrap(~site)
# d) if you omit the facet_wrap part and colour code per site, you could view 
# both beaches in one plot
plotbeaches%>%
  na.omit()%>%
  filter(entnumber <1000)%>%
  filter(site %in% c("Coogee Beach", "Bondi Beach"))%>%
  ggplot(aes(x = year, y= entnumber, colour = site))+
  geom_jitter()#+
  #facet_wrap(~site)

# Step 3.5) Last step: exporting into to a jpg/pdf to use in manuscript or report
# this will save the last plot you made
ggsave(here("output", "coogeebondi.png"))

### SECTION 4 INSTALLING AND PEAKING INTO RMARKDOWN
#install.packages("rmarkdown")


