
#================================================================================
# TITLE : BIO-DIVERSITY IN ASIA OCEANIA.
#================================================================================
# IMPORT LIBRARIES
#install.packages("wordcloud2")
#install.packages("naniar")
#install.packages("dplyr")
library(dplyr)
library(Xmisc) #strip functions can be used
library(leaflet) # used for interactive maps
library(dslabs)
library(ggridges)
library(plotly)
library(tidyr)
library(wordcloud2)
library(stringr)
library(naniar)

# IMPORT DATASETS


biodiv_asia_oceania <- read.csv('BIODIVERSITY_ASIA_OCEANIA.csv')
countries_code <- read.csv('countries_codes_and_coordinates.csv') 



# PRE-PROCESSING


filtered_biodiv <- biodiv_asia_oceania %>%
  select( gbifID = ï..gbifID,occurrenceID, class, order, family, genus, species, taxonRank, scientificName,
          verbatimScientificName, verbatimScientificNameAuthorship, countryCode, individualCount,
          decimalLatitude, decimalLongitude, day, month, year, taxonKey, speciesKey, basisOfRecord)




filtered_biodiv1 <- filtered_biodiv %>% rowwise() %>%
  mutate(Scientific_names =paste0(setdiff(strsplit(as.character(verbatimScientificName),split = " ")[[1]],
                                          strsplit(as.character(verbatimScientificNameAuthorship)," ")[[1]] ),
                                  collapse = ' ') ) %>% 
  select(-verbatimScientificName,-verbatimScientificNameAuthorship)%>%
  rowwise() %>%
  mutate(Authors = paste0(setdiff(strsplit(as.character(scientificName),split = " ")[[1]],
                                  strsplit(as.character(Scientific_names)," ")[[1]] ),
                          collapse = ' ') )%>% select(-scientificName)

countries_code$countryCode <- countries_code$countryCode %>% strip(char = " ")

countries_code$CODE <- countries_code$CODE %>% strip(char = " ")

main_data      <- filtered_biodiv1 %>% left_join(countries_code, by= "countryCode")

main_data      <- main_data %>% filter(!(Scientific_names==""))


colnames(main_data)
summary.default(main_data)
#====================================================================================================================================


# 
#1) Objective 1 : To check the Total occurrences of each class in ASIA and Oceania

# Filtering the data in order to remove balnk values in class column and summarising.

class_occur <- main_data %>% 
  group_by(class) %>%
  filter(!(class == ""))%>%
  summarise(Class_count=n())

# Visualization : Using package plotly

plot_ly(
  type = 'scatterpolar',                 # Type = scatterpolar,gives radarchart
  r = class_occur$Class_count,
  theta = class_occur$class,
  fill = 'toself'
)

#here we can see that the total occurrences of Mammalia > Repitilia > Amphibia.
#but their count is close enough so we can conclude that all three classes are
#almost equally populated in Asia and Oceania

#if we have to check the occurrences distribution on a yearly scale
# grouping the data by year and class so that we get a year column for slider.

class_occur_y <- main_data %>% 
  group_by(year, class) %>%
  filter(!(class == "")&!(year==""))%>%
  summarise(Class_count=n())

plot_ly(
  type = 'scatterpolar',
  r = class_occur_y$Class_count,
  theta = class_occur_y$class,
  fill = 'toself',
  frame = class_occur_y$year
)




#2) objective 2 : ploting population of each class in different region/country
#Grouping the given columns

country_class <- main_data %>%
  group_by(Country, CODE, 
           lat= Latitude..average.,
           lon= Longitude..average.,class)%>%
  summarise(count = n())

#Using a Choropleth map to show how much is the data populated in different regions

choropleth <- 
  plot_ly(data = country_class,
          type='choropleth', 
          locations=country_class$CODE,
          z=country_class$count,                        # it gives intensity to the map
          text=country_class$Country, 
          colorscale="Reds", 
          frame = country_class$class)%>%
  layout(g <- list(
    scope = 'asia',
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5,
    lataxis = list(range=c(min(country_class$lat),max(country_class$lat))),
    lonaxis = list(range=c(min(country_class$lon),max(country_class$lon)))
  ))
choropleth
#Conclusion : The total occurrences for all the three class is the maximum in Philippines,
# Class Mammalia is spread across Asia and Oceania but we can see most of the 
# Amphibians and Reptiles are populated in the countries near oceans or on islands.



#Objective 3 - To find which class has highest number of fossils

main_data_copy_1 <- data.frame(main_data)
class_fossil_data <- main_data_copy_1 %>% select(class,basisOfRecord)
class_fossil_data <- class_fossil_data %>% filter(basisOfRecord=="FOSSIL_SPECIMEN")

# Replacing blank values with NA with the help of "naniar" library
class_fossil_data <- class_fossil_data %>% replace_with_na(replace = list(class= ""))
class_fossil_data <- class_fossil_data %>% replace_with_na(replace = list(basisOfRecord = ""))

# Removing null values
class_fossil_data <- class_fossil_data %>% drop_na()


# Calculated the count of Fossils with respect to Class

Class_Fossil_Record <- class_fossil_data %>% group_by(class, basisOfRecord) %>% mutate(count_total = n())
Distinct_Class_Fossil <- distinct(Class_Fossil_Record)

# Arranged the class with maximum number of Fossils
Distinct_Class_Fossil_Count <- arrange(Distinct_Class_Fossil, desc(count_total))

# Created a pie chart with ggplot

pie_gg <- ggplot(data = Distinct_Class_Fossil_Count, aes(x="", y = count_total, fill = class)) +
  geom_col(color = "black") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(count_total)), 
            position = position_stack(vjust = 0.5), size = 3) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ggtitle("Pie chart of Class and it's Fossil Count") + 
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0.5,size = 18),
        legend.key.size = unit(1,"cm"))

pie_gg





# Conclusion- We have found that class Mammalia has highest number of Fossils around 28586.




# =======================================================================================================

#making a copy of the original data
main_data_copy = data.frame(main_data)
main_data_copy = main_data_copy %>% replace_with_na(replace = list(Authors = ""))
#replacing blank values with NA with the help of "naniar" library


#Objective 4 - To find some interesting facts about amphibian fossils
#filtering main data so that we get amphibian species fossils
fossil <- main_data_copy%>% 
  filter(basisOfRecord=="FOSSIL_SPECIMEN",
         class == "Amphibia",
         taxonRank == 'SPECIES')


#mapping the amphibian fossils on the map on the basis of latitude
#and longitude of where they were found using the leaflet library and
#esri world imagery map layout for satellite view of the earth map

plot <- leaflet()%>% addTiles() %>% addCircleMarkers(data = fossil,
                                                     lat = ~decimalLatitude,
                                                     lng = ~decimalLongitude,
                                                     radius = ~0.5,
                                                     color = "#000000",
                                                     popup = ~Scientific_names
                                                     ) %>% addProviderTiles(providers$Esri.WorldImagery)

plot


#Conclusion - We find an interesting plot on the map which is in the middle of the desert
#upon inspection we find that the plot belongs to a fossil of a frog Kizylkuma 
#antiqua which is a marine frog that went extinct 90-95 million
#years ago. What's interesting is that frogs need water or some sort of moisture
#to stay alive or else their skin dries out which results in death.
#The fossil of the foremen tioned frog was found in the Kyzylkum Desert of 
#Uzbekistan which is the 15th largest desert in the world.
#So in conclusion, for the frog to live there millions of years ago means there is
#a possibility of some water body near to the frog for it to stay alive that is 
#there was some waterbody where there's a desert in present time.



#=======================================================================
#

#Objective 5 - To find which author got the most citations in the data.


#Upon inspection of the authors column it was found that it had brackets and
#years associated with the name which are not required for our analysis.
#Hence, we remove them using regular expressions.
main_data_copy = main_data_copy %>% mutate(Author_name = gsub("\\(|\\,.*", "", Authors))
authors = main_data_copy %>% select(Author_name) %>% drop_na()
freq_author_name = authors %>% count(Author_name)



#upon inspecting the freq_author_name it was found out that some author's  name had
#special characters due to wrong text encoding. So changing there names

freq_author_name$Author_name = str_replace(freq_author_name$Author_name,
                                           "DumÃ©ril & Bibron", "Duméril & Bibron")

freq_author_name$Author_name = str_replace(freq_author_name$Author_name,
                                           "GÃ¼nther", "Günther")

#we got the author who has the most citations. Let's make a wordcloud to understand it better
wordcloud2(data=freq_author_name, size=0.5, color='random-dark', shape = 'pentagon')




#Conclusion - The author with most number of citations is Boulenger. George 
#albert boulenger FRS (Fellow of Royal Society) who gave scientific names
#to more than 2,000 new animal species especially fishes





#==========================================================================================



#Country -> Class -> Scientific_names -> count(n) -> max(count) in that country

#objecrive 6 : To understand which class appears most frequently (max(count)) under corresponding Countries.

# Data Wrangling : 1) Group the main data on Class and Country , arrange the counts in descending order
#2) Each Country is displayed with each class count.
#3) Displaying each country having max(count) of class 
#4) Displaying which Class has the maximum count in India.





max_country_class <-main_data  %>% group_by(Country,CODE,class) %>%
  summarise( count = n())%>% group_by(class,Country,CODE)%>%
  arrange(desc(count))%>%ungroup()%>%
  group_by(Country)%>%top_n(1,count)
max_country_class <- max_country_class %>% left_join(countries_code, by = "CODE")

max_choropleth <- 
  plot_ly(data = max_country_class,
          type='choropleth', 
          locations= max_country_class$CODE,
          z=max_country_class$count,          # it gives intensity to the map
          text= max_country_class$Country, 
          colorscale="Reds", 
          frame = max_country_class$class)%>%
  layout(g <- list(
    scope = 'asia',
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5,
    lataxis = list(range=c(min(country_class$lat),max(country_class$lat))),
    lonaxis = list(range=c(min(country_class$lon),max(country_class$lon)))
  ))
max_choropleth



# India has the maximum count of Mammalia .
class_india <- m%>% filter(Country == "India")

#Conclusion :
# India has the maximum count of Mammalia



#objective 7 : To plot only the max(order) found in India. 

# Data Wrangling : 1) Droping NA from year and 0.00 values from latitude , unselect gbifID, occurenceID columns
#2) Takonkeys(Primary keys) may be repeated several times but the place where it is found is different hence we don't remove the duplicate.
#3) Filtering Rodentia order
#4) Plot using leaflet(addTiles,addMarkers)

india <- main_data%>% select(!gbifID)%>%filter(Country == "India",!(decimalLatitude ==0.00),!is.na(year),!(order ==""))



# find the exact Order having maximum count in India
a <- india%>%group_by(order) %>% summarise(count_order = n())%>% arrange(desc(count_order))%>% top_n(count_order, n=1)


# plot order Rodentia under class Mammalia using leaflet on India
rodentia_india <- india %>% filter(order == "Rodentia", !(family == ""))


plot <- leaflet() %>%
  addTiles() %>% 
  addMarkers(lng = rodentia_india$decimalLongitude, lat = rodentia_india$decimalLatitude,
             popup = paste("Family", rodentia_india$family, "<br>","Year:", rodentia_india$year,"<br>",
                           "Basis of record :",rodentia_india$basisOfRecord,"<br>",
                           "Scientic name :",rodentia_india$Scientific_names,"<br>",
                           "Author :", rodentia_india$Authors))
plot


#

# objective 8 : To Find which order of class decreases throughout the centuries. 

# Data Wrangling : 1) Divide the main data century-wise (1700-1899) and (1900-2020)
#2) Separate 3 classes from the filtered data.
#3) From each class group all the orders and take the count.
#4) Removing all the NA's or blank values from the class data
#5) Merging the values and Comparing the count


# divide 1700 till 1900
yearly <- main_data %>% filter( (year >=1700) & (year <1900))

# divide yearly by 3 class
class_amp <- yearly %>% filter(class =="Amphibia")

class_mam <- yearly %>% filter(class =="Mammalia")

class_rep <- yearly %>% filter(class =="Reptilia")



# divide each class by order and count the number of each class
# by taking the min of all the counts, go to the class data n filter for only by min(order)
# plot


# Amphibia
order_amp <- class_amp %>% group_by(order) %>% summarise(count_order = n())
order_amp <- order_amp[-1,]
order_amp <- order_amp %>% arrange(count_order, desc = FALSE)

#Mamalia
order_mam <- class_mam %>% group_by(order) %>% summarise(count_order = n())
order_mam <- order_mam[-1,]
order_mam <- order_mam%>% arrange(count_order, desc = FALSE)

#Reptilia
order_rep <- class_rep %>% group_by(order) %>% summarise(count_order = n())
order_rep <- order_rep[-1,]
order_rep <- order_rep %>% arrange(count_order, desc = FALSE)



# Dividing year right after 1900
yearly2 <- main_data %>% filter( year >=1900)

# divide yearly by 3 class
class_amp2 <- yearly2 %>% filter(class =="Amphibia")

class_mam2 <- yearly2 %>% filter(class =="Mammalia")

class_rep2 <- yearly2 %>% filter(class =="Reptilia")



# divide each class by order and count the number of each class
# by taking the min of all the counts, go to the class data n filter for only by min(order)
# plot


# Amphibia
order_amp2 <- class_amp2 %>% group_by(order) %>% summarise(count_order = n())
order_amp2 <- order_amp2[-1,]
order_amp2 <- order_amp2 %>% arrange(count_order, desc = FALSE)

#Mamalia
order_mam2 <- class_mam2 %>% group_by(order) %>% summarise(count_order = n())
order_mam2 <- order_mam2[-1,]
order_mam2 <- order_mam2%>% arrange(count_order, desc = FALSE)

#Reptilia
order_rep2 <- class_rep2 %>% group_by(order) %>% summarise(count_order = n())
order_rep2 <- order_rep2[-1,]
order_rep2 <- order_rep2 %>% arrange(count_order, desc = FALSE)

# Conclusion :
# Order Didelphimorphia and Order Sirenia of Class Mammalia are slowly on the verge of extinction in the region of Asia and Oceania.

#Sirenian populations are easily depleted by activities of people, often falling victim to hunting,
#drowning in nets, collisions with boats, and habitat degradation. They are protected by law and international agreements in almost all countries,
#but in many areas populations remain very low or are extirpated as a result of past and present human activities.

#In order Didelphimorphia Gracilinanus aceramarcae, Marmosa andersoni, and Marmosops handleyi are considered Critically Endangered by the IUCN. 
# Although none of these species face direct threats from humans, all are facing rampant habitat destruction that has strong negative conservation implications.
#Other species are facing conservation threats, especially those with restricted distributions and found in habitats affected by high rates of deforestation, but much more information is necessary to correctly assess their status.

#===============================================================================

