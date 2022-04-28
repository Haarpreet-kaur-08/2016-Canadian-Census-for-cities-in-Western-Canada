library(readr)
library(readxl)
library(canlang)
library(readxl)
library(repurrrsive)
library(testthat)
library(tidyverse)


################### Question -1 ########################

abbotsford <- read_excel("C:/Users/admin/Documents/data/abbotsford_lang.xlsx", 2)
View(abbotsford)

calgary <- read.csv("C:/Users/admin/Documents/data/calgary_lang.csv")
View(calgary)

edmonton <- read_excel("C:/Users/admin/Documents/data/edmonton_lang.xlsx")
View(edmonton)

columns <- c('category', 'language', 'mother_tongue', 'most_at_home', 'most_at_work', 'lang_known')
kelowna <- read.csv("C:/Users/admin/Documents/data/kelowna_lang.csv", skip = 6, sep = ';',header=F)

colnames(kelowna) <- columns
View(kelowna)

vancouver <- read.csv("C:/Users/admin/Documents/data/vancouver_lang.csv")
View(vancouver)

url <- "https://github.com/ttimbers/canlang/raw/master/inst/extdata/victoria_lang.tsv"
victoria <- read_tsv(url)
View(victoria)

destfile <- "C:/Users/admin/Documents/data/victoria_lang.tsv"
download.file(url, destfile)


#2 method which shows same result as given in Assignment
library("dplyr")
mydir = "data"
myfiles1 <- list.files(path=mydir) 
raw.files <- data_frame(filename = list.files('data/'))
raw.file.paths <- raw.files  %>%
  mutate(filepath = paste0("data/", filename))
b <- str_split_fixed(raw.file.paths$filename, "_", n = 2)
data <- data.frame(raw.file.paths,b)
View(data)
data <- data[c(-3,-4,-7),-4]

data <- select(data,c(filename,X1,filepath))
colnames(data) <- c('File', 'Object name to bind to the data frame', 'File location')
data
data <- data.frame(data)
data$File.location[data$File.location == "data/victoria_lang.tsv"] <- "https://github.com/ttimbers/canlang/raw/master/inst/extdata/victoria_lang.tsv"
View(data)


########## Question-2 ###################

#devtools::install_github("ttimbers/canlang")
library(canlang)
head(region_lang)
que2 <- region_lang %>%
  filter(language == "Mandarin") %>% arrange(desc(most_at_home))
que_2 <- que2[2,1]
class(que_2)
length(que_2)
typeof(que_2)
mandarin2 <- filter(que_2)$region
class(mandarin2)
length(mandarin2)
typeof(mandarin2)
mandarin2


################# QUESTION-3 ####################

que3 <- region_lang %>% filter(region_lang$region=='Toronto') %>% arrange(desc(most_at_home)) %>% head(5) %>% select(c(region,language,most_at_home))

Language <- que3$language

que_3 <- region_data %>% filter(region == 'Toronto') %>%
  select('region','population') 
perc_pop <- (que3$most_at_home/que_3$population)*100
d <- data.frame(Language,perc_pop)
View(d)


###################Qestion-4###########################

#install.packages("tidyr")
library(tidyr)
library(readr)

DF <- read_csv("data/departure_bay_temperature.csv", 
                                      skip = 2)
View(DF)
library(dplyr)
temps_tidy <- DF %>% gather(Month, temperature, Jan:Dec) %>% drop_na() 
temps_tidy <- temps_tidy %>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

View(temps_tidy)
str(temps_tidy)
View(temps_tidy)


library(plotly)
library(hrbrthemes)
p <- temps_tidy %>%
  ggplot(aes(x=Year, y=temperature)) +
  geom_smooth(aes(col=Month),size=1, method="lm") +
  theme_ipsum() + labs(title= "Comparison of Sea Surface Temperature over Time of Year (i.e Month)", xlab="YEAR", ylab="Temperature")+ theme(plot.title =element_text(size=14,face="bold", color="firebrick"),
                                                                                                                                             axis.title.x = element_text(size=12,color = "Black",face="bold"),
                                                                                                                                             axis.title.y = element_text(size=12,color="Black",face="bold"))+theme_bw()
ggplotly(p) 


q <-  ggplot(temps_tidy,aes(x = Year, y = temperature)) +
  geom_smooth(color = "firebrick4") +
  facet_wrap( ~ ordered(Month), ncol = 3) + theme_bw(base_size = 15)
  # adjust the x axis breaks
ggplotly(q)


#The plot shows that Sea Surface temperatue is maximum for August and July. Moreover, is minimum for January and December for all years

########################## Question-5########################
library(readr)
tidy_lang <- read_delim("data/language_diversity.csv", 
                                 delim = "\t", escape_double = FALSE, 
                                 trim_ws = TRUE)
View(tidy_lang)

tidy_lang <- tidy_lang %>% spread(Measurement, Value)
View(tidy_lang)

q <-  ggplot(tidy_lang,aes(x = Langs, y = Population)) +
  geom_point(aes(col=Country)) + labs(title= "Number of Language spoken in each Country against Country Population", xlab="Language", ylab="Population")+ theme(plot.title =element_text(size=14,face="bold", color="firebrick"),
                                                                                                                                                           axis.title.x = element_text(size=12,color = "Black",face="bold"),
                                                                                                                                                           axis.title.y = element_text(size=12,color="Black",face="bold"))+theme_bw()
ggplotly(q) 


# Question-6
set.seed(2020) # this makes the random number process below reproducible
random_matrix <- matrix(rexp(200, rate=.1), ncol=20)
random_matrix

small_matrix <- random_matrix[1:5,6:15]
small_matrix

#Hence subset matrix contains 10 columns and 5 rows
