#Clean your workspace

rm(list=ls())

#Find out what's in your working directory

dir()

#setwd("C:/Users/YOUR_PATH") #not needed in JupyterHub on moodle
#Set the timezone (important for the correct handling of date-time objects)

Sys.setenv(TZ = "Etc/GMT-1") #

#define path arguments later used in the script

functionPath <- "../functions/"    # where functions are stored

outputPath <- "../out/"            # where data output and log files can be stored

#and output from previous analysis can be loaded)

inputPath <- "../data/"            # input data are in this folder.
#install.packages("tidyverse")

library(tidyverse)
getwd()

measurement <- read_csv(file=paste0(inputPath,"measurement.csv"), comment="#")
head(measurement)
#Add answer Q 1 here
# measurement.csv has 3 comment lines (the lines starting with #).
# They give info about the data: who modified it (A. Hund, original data
# from E. Frossard), the purpose (demonstration of data handling in R) and
# what the data is (ETH excursion in plant nutrition, 14 and 15 April 2014).
# So comment lines can be used to describe the data inside the file itself,
# e.g. author, source, date, units or notes. read_csv ignores them because
# of comment="#".
1:10 |> mean()
mean(1:10)
years <- factor(2008:2012)

as.numeric(as.character(years)) #nesting in the base package
years |> as.character() |> as.numeric() #piping in the tidyverse
design <- read_csv(file=paste0(inputPath,"design.csv"), comment="#")

entry <- read_csv(file=paste0(inputPath, "entry.csv"), comment="#")

trait <- read_csv(file=paste0(inputPath, "trait.csv"), comment="#") |>

                  select(trait.id, trait.label, trait.name)

method <- read_csv(file=paste0(inputPath, "method.csv"), comment="#") |>

                  select(method.id, method.label)
# ungraded question: the SPAD method measures the chlorophyll content of the
# leaf with a handheld meter (e.g. Minolta SPAD-502) without damaging it.
# It measures the light going through the leaf at 650 nm (red) and 940 nm
# (infrared) and gives a relative value from 0 to 99.9, not the absolute
# chlorophyll content. Usually it is measured on the flag leaf.
entry |> 

  count(variety) |> 

  filter(n > 1)
entry |> 

  count(entry.id) |> 

  filter(n > 1)
#Add answer Q 2 here
design |>
  count(plot.id) |>
  filter(n > 1)
trait |>
  count(trait.id) |>
  filter(n > 1)
# both give a tibble with 0 rows, so no id appears more than once
# -> plot.id in design and trait.id in trait are unique
#Do a left join

dat.long <- design |> 

  left_join(entry, by="entry.id") |> 

  left_join(measurement, by="plot.id") |> 

  left_join(trait, by="trait.id")
#Add answer Q 3 here
dat.long <- dat.long |>
  left_join(method, by="method.id")
#Identify all ids using the grep function. 

str(dat.long)

ids<-names(dat.long)[grep(".id", names(dat.long))]



#Add additional columns which are considered factors

Factors <- c(ids, "block", "row", "range", "variety", "fertilizer")



dat.long<-dat.long |> dplyr::mutate(across(all_of(Factors), as.factor))

str(dat.long)
dat.long$timestamp <- as.POSIXct(strptime(dat.long$timestamp, "%Y.%m.%d"))

dat.long$date <-as.Date(dat.long$timestamp)



#Lubridate offers useful functions to extract components from dates:

  #The week of the year

  week(dat.long$timestamp)
  #The quarter of the year

  lubridate::quarter(dat.long$timestamp) 
  #We add a new column “DOY” to the tibble

  dat.long$DOY <- lubridate::yday(dat.long$timestamp) #The day of the year

  str(dat.long$DOY)
dat.long <- dat.long |> mutate(treat = paste(fertilizer, variety, sep="."))

write_rds(dat.long, file=paste0(outputPath, "dat.long.rds"))
# dat.longFull.rds is not in my out folder, so I save dat.long (which now
# has the method table) under this name too, otherwise read_rds fails later
write_rds(dat.long, file=paste0(outputPath, "dat.longFull.rds"))
#Only the day of the year (DOY) 104

  #Defin what you want to plot - Create a plot that uses Row and Range as x and y axis

    p1 <- ggplot(dat.long |> filter(DOY == 104), aes(x=row, y=range))

  #Add a geometry that fills this plot with a raster colored by SPAD values

    p2 <- p1 + geom_raster(aes(fill=`value`)) 

  #Add the treatment levels as text

    p3 <- p2 + geom_text(aes(label=treat), size=3) 
plot(p1)

plot(p2)

plot(p3)
#Both days together - this make only sense in combination with facet_grid()

    p_facet <- ggplot(dat.long , aes(x=row, y=range)) +

          geom_raster(aes(fill=`value`)) + 

          geom_text(aes(label=treat), size=3)

          

  #Define a different raster for each measuring date using facet_grid

    p_facet <- p_facet + facet_grid( ~ DOY)

    

    plot(p_facet)
p<-ggplot(dat.long, aes(row, range)) + 

  geom_raster(aes(fill = `value`)) +

  geom_text(aes(label=treat), size=3) +

  facet_grid( ~ DOY)



  ggsave(p, filename = paste0(outputPath, "rastergraph.png"))

  ggsave(p, filename = paste0(outputPath, "rastergraph.svg"), device="svg")
#Add answer Q 4 here
# The png is a raster graphic, it is made of pixels. When I zoom in a lot
# it gets pixelated and the text gets blurry. The svg is a vector graphic,
# the shapes and text are saved as instructions, so it stays sharp at every
# zoom level and the elements (text, colours, ...) can still be edited
# later, e.g. in Inkscape or Illustrator.
# For further processing or scaling the figure I would prefer the svg.
  #Prepare the panel

  p <- ggplot(dat.long, aes(x=date, y=value)) #Define what you want to plot

  #plot the lines and define groups to be coloured

  p <- p + geom_line(aes(x=date, y=value, group=plot.id, colour=factor(block)))

  #Set the defined colours 

  p <- p + scale_color_discrete()

  #Define a different panel for each Variety-by-fertilizer treatment

  p <- p + facet_grid(variety ~ fertilizer)

  #Rotate the x-axis to be readable. 

  p<-p + theme(axis.text.x = element_text(angle = 90))
#Add the code for Q 5 here
# fertilizer is a factor at this point, so I turn it back into numbers
# (as.character first, like in the years example) to get a number axis
dat.plot <- dat.long |> mutate(fertilizer = as.numeric(as.character(fertilizer)))
p <- ggplot(dat.plot, aes(x=fertilizer, y=value))
p <- p + geom_point(aes(x=fertilizer, y=value, colour=factor(block)))
p <- p + scale_color_discrete()
p <- p + facet_grid(date ~ variety)
p <- p + theme(axis.text.x = element_text(angle = 90))
plot(p)
dat.long<-read_rds(file=paste0(outputPath, "dat.longFull.rds"))



dat.wide<-dat.long |> select(-measurement.id, -timestamp , -DOY)



dat.wide<-dat.wide |> pivot_wider(names_from = c(trait.label, method.label, date),



                                   values_from=value)



head(dat.wide)
#Add the code for Q 5 here
# Q6: the values of trait.label, method.label and date were pasted together
# with "_" and became the names of the new columns, e.g.
# Fl0Chl_SPAD001_2014-04-13. The numbers from value are now in these new
# columns (one column per date). trait.label, method.label, date and value
# are no longer columns, and each plot has only one row now (18 instead of 36).
TraitsWide<-dat.wide |> select(contains("Fl0Chl"))

plot(TraitsWide)
#There are many options how to include functions into a pipe

test<-dat.long |> split(dat.long$date)

str(test)
#Type your answer here.
# Q7: split gives a list with 2 elements, one for each date (2014-04-13 and
# 2014-04-14). Each element is a tibble with the 18 plots of that day and
# all 18 columns of dat.long. So the data is just split into 2 parts by
# date, nothing is lost.
#There are many options how to include functions into a pipe

aov1<-dat.long |> split(dat.long$date) |> 

  purrr::map(function(df) aov(value ~ variety * fertilizer + block, data=df)) 

#You can use "\" as anonyoumous function shortcut.and the dot "." for the dataframe.

aov1<-dat.long |> split(dat.long$date) |>

  map( \(.) aov(value ~ variety * fertilizer + block, data=.)) 



#this is how the "%>%" operator would be used:

#aov1<-dat.long %>% split(dat.long$date) %>%

#map(~aov(value ~ variety * fertilizer + block, data=.)) #



aov1
aov.tab<-aov1 |> map(.f=~summary(.x))

aov.tab<-aov1 |> map(summary) #same



#Access the ANOVA table to do some summary statistics

str(aov.tab) #A list in a list
str(aov.tab[[1]][[1]]$`Pr(>F)`) #
#

Pr.val<-aov.tab |> map(function(x){x[[1]]$`Pr(>F)`})

Pr.val
#Type your code here
# Q8
F.val <- aov.tab |> map(function(x){x[[1]]$`F value`})
F.val
