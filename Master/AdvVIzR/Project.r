# -------------------------------- Color code
# YInMn Blue: #395a7f
# Air superiority blue: #6e9fc1
# Uranian Blue: #a3cae9
# Anti-flash white: #e9ecee
# Silver: #acacac
windowsFonts(Times=windowsFont("Times New Roman"))

#--------------------------------- Package
library(dplyr)
library(tidyverse)
library(ggjoy)
library(ggmosaic)
library(usmap)
library(ggplot2)

setwd("C://Users//nomin//OneDrive//Desktop//Sugarbayar//Project_2023Y1//AdvVizR")

#--------------------------------- Data
df=read.csv("happiness.csv")
df=df %>% select(-c(X,gwbush04,happy,gwbush00,occattend,regattend,y94,y96,y98,y00,y02,y04,y06, reg16,attend)) # removing unneces columns
df$vhappy[df$vhappy==1]='happy'
df$vhappy[df$vhappy==0]='not happy'

# num - prestige, educ, babies, preteen, teens, tvhours
# cate - others

#--------------------------------- Part 1: Data explaining
df1=df %>% group_by(year,vhappy) %>% summarise(cnt=n())

df1 <- df1 %>% 
  arrange(year, desc(vhappy)) %>% 
  group_by(year) %>% 
  mutate(label_sum = cumsum(cnt)) 

df1 <- df1 %>% 
  mutate(label_sum2 = label_sum*.5)

ggplot(data=df1, aes(x=factor(year), y=cnt,fill=factor(vhappy)))+
  geom_bar(na.rm = TRUE, position = "stack", width = 0.7, stat = "identity") +
  ylab('Number of data') +
  xlab('Year') +
  labs(fill="vhappy")+
  geom_label(aes(y = label_sum2, label = cnt), fill = 'white', position = position_stack(),
             vjust = 0.5, color = "black", size = 3) +
  scale_fill_manual("legend", values = c("happy" = "#395a7f", "not happy" = "#6e9fc1")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ theme(legend.position="bottom")
  
  
statepop$division[statepop$full %in% c('Maine',
                                       'New Hampshire',
                                       'Vermont',
                                       'Massachusetts',
                                       'Rhode Island',
                                       'Connecticut')]='New England'
statepop$division[statepop$full %in% c('New York',
                                       'Pennsylvania',
                                       'New Jersey')]='Middle Atlantic'
statepop$division[statepop$full %in% c('Wisconsin',
                                       'Michigan',
                                       'Illinois',
                                       'Indiana',
                                       'Ohio')]='East North Central'
statepop$division[statepop$full %in% c('North Dakota',
                                       'South Dakota',
                                       'Nebraska',
                                       "Kansas",
                                       'Minnesota',
                                       'Iowa',
                                       'Missouri')]='West North Central'
statepop$division[statepop$full %in% c('Delaware',
                                       'Maryland',
                                       'District of Columbia',
                                       'Virginia',
                                       'West Virginia',
                                       'North Carolina',
                                       'South Carolina',
                                       'Georgia',
                                       'Florida')]='South Atlantic'
statepop$division[statepop$full %in% c('Kentucky',
                                       'Tennessee',
                                       'Mississippi',
                                       'Alabama')]='East South Central'
statepop$division[statepop$full %in% c('Oklahoma',
                                       'Texas',
                                       "Arkansas",
                                       'Louisiana')]='West South Central'
statepop$division[statepop$full %in% c('Idaho',
                                       'Montana',
                                       'Wyoming',
                                       'Nevada',
                                       'Utah',
                                       'Colorado',
                                       'Arizona',
                                       'New Mexico')]='Mountain'
statepop$division[statepop$full %in% c('Alaska',
                                       'Washington',
                                       'Oregon',
                                       'California',
                                       'Hawaii')]='Pacific'
statepop$pop_2015[statepop$division=='East North Central']=2881
statepop$pop_2015[statepop$division=='East South Central']=1117
statepop$pop_2015[statepop$division=='Middle Atlantic']=2414
statepop$pop_2015[statepop$division=='Mountain']=1195
statepop$pop_2015[statepop$division=='New England']=808
statepop$pop_2015[statepop$division=='Pacific']=2353
statepop$pop_2015[statepop$division=='South Atlantic']=3340
statepop$pop_2015[statepop$division=='West North Central']=1247
statepop$pop_2015[statepop$division=='West South Central']=1782

plot_usmap(data = statepop, values = "pop_2015", color = "#395a7f",labels = TRUE) + 
  scale_fill_continuous(
    low = "white", high = "#395a7f", name = "Number of data", label = scales::comma
  ) + theme(legend.position = "right")


#--------------------------------- Part 2: Factors

### Character - black, female, tvhours, owngun
# black & female
df1=df %>% filter(vhappy=='happy') %>% group_by(black, female) %>% summarise(cnt=n())
df1$black[df1$black==1]='black'
df1$black[df1$black==0]='not black'
df1$female[df1$female==1]='female'
df1$female[df1$female==0]='not female'
ggplot(df1, aes(x = black, y = female)) + # nowa baza danych
  geom_tile(aes(fill = cnt), color = 'white', show.legend = F) +
  theme_minimal() +labs(y= "Gender", x = "Skin")  + 
  geom_text(aes(label = cnt), size = 5, fontface = 'bold', color = 'white') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
# tvhours
ggplot(df, aes(x=factor(year), y=tvhours, fill=factor(vhappy))) + 
  geom_boxplot() + scale_fill_manual(values = c("#395a7f", "#6e9fc1"),name='Category')+labs(y= "Tvhours", x = "Year")  + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ theme(legend.position="bottom")
# owngun
library(ggplot2)
library(webr)
library(dplyr)
x=df %>% filter(!is.na(owngun),!is.na(vhappy)) %>% group_by(owngun,vhappy) %>% summarise(cnt=n())
PieDonut(x, aes(owngun, vhappy, count=cnt),color = c('white')) 

### Family - divorce, babies, preteen(6-12), teens(13-17), mothfat16
# divorce
df %>% filter(!is.na(divorce)) %>% ggplot() +
  geom_mosaic(aes(x = product(vhappy), fill=divorce), show.legend = FALSE) + 
  geom_mosaic_text(aes(x = product(vhappy), fill=divorce), na.rm = TRUE, repel = TRUE, as.label=TRUE,color='black',fill='white') +
  scale_fill_manual(values = c('#395a7f','#6e9fc1'))+labs(y= "Divorce", x = "Category") + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
# babies
df %>% filter(!is.na(babies),babies!=0,babies<=5) %>% ggplot(aes(x = babies, y = factor(year), fill = factor(vhappy))) +
  geom_joy(scale = 1,alpha = .4) + theme_joy() +
  scale_fill_manual(values = c('#395a7f','red3'),name='Category')+ theme(legend.position="bottom")+labs(y= "Year", x = "Babies") + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ theme(legend.position="bottom")

### Work - workstat, income, unemp10
# workstat
library(ggpubr)
x=df %>% filter(!is.na(workstat)) %>% group_by(workstat,vhappy) %>% summarise(cnt=n())
x=x %>% spread(workstat,cnt) %>% as.data.frame()
rownames(x) <- x$vhappy
ggballoonplot(x[,-1], fill = '#395a7f',show.label = TRUE) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ theme(legend.position="bottom")+scale_size_continuous(range = c(10,35))+
  theme(legend.position = "none") +labs(y= "Category", x = "Workstat")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
library(ggparallel)
library(RColorBrewer)
palette1 <- brewer.pal(3, "Set1")
palette2 <- brewer.pal(8, "Set2") 
a=df %>% filter(!is.na(vhappy),!is.na(workstat))
#a=a %>% filter(workstat=='working fulltime'|workstat=='working parttime')
cols <- c(brewer.pal(5, "Blues"),brewer.pal(5, "Blues"))
ggparallel(list("vhappy", "workstat"),order=0,alpha = 0.3,label.size = 5, text.angle = 0,data=a %>% filter(!is.na(vhappy),!is.na(workstat))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom')+ 
  scale_fill_manual(values=cols) + scale_colour_manual(values=cols)
ggparallel(list("vhappy", "workstat"), order=0, alpha = 0.3, label.size = 5, text.angle = 0, data=a %>% filter(!is.na(vhappy), !is.na(workstat))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(color = "black"), # Set legend text to black
        axis.text.x = element_text(color = "black"), # Set x-axis text to black
        axis.text.y = element_text(color = "black")) + # Set y-axis text to black
  scale_fill_manual(values=rep("black", length(cols))) + # Set fills to black
  scale_colour_manual(values=rep("black", length(cols))) # Set colours to black 20240113

### Plots - educ
# prestige, educ

############################### added Part ##############################################
data=df
str(data)

### preteen

# Filter preteen values to be between 0 and 4 (inclusive)
happiness_data <- subset(data, preteen >= 0 & preteen < 4)

# Convert preteen to a factor with levels 0 to 4
happiness_data$preteen <- factor(happiness_data$preteen, levels = 0:4)

# Define the colors extracted from the image
color_palette <- c("#395a7f", "#6e9fc1", "#B3BDCD", "#E2E6ED")
# Count Plot
ggplot(happiness_data, aes(x = as.factor(vhappy), fill = preteen)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values=color_palette) +
  labs(title = "Count Plot of Happy and unHappy by Number of Preteens",
       x = "Happy and unHappy",
       y = "Count",
       fill = "Number of Preteens") +
  theme_minimal()+ theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ theme(legend.position="bottom")

### Teens
    
# Filter preteen values to be between 0 and 4 (inclusive)
happiness_data <- subset(data, teens >= 0 & teens < 4)

# Convert preteen to a factor with levels 0 to 4
happiness_data$teens <- factor(happiness_data$teens, levels = 0:4)

# Count Plot
ggplot(happiness_data, aes(x = as.factor(vhappy), fill = teens)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values=color_palette) +
  labs(title = "Count Plot of Happy and unHappy by Number of teens",
       x = "Happy and unHappy",
       y = "Count",
       fill = "Number of teens") +
  theme_minimal()+ theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ theme(legend.position="bottom")

## income 
happiness_data = df
happiness_data$income <- factor(happiness_data$income, levels = unique(happiness_data$income[!is.na(happiness_data$income)]))
color_palette <- c("#395a7f", "#6e9fc1")

# Now create a bar plot with income ranges on the x-axis and counts on the y-axis, 
# faceted by the vhappy variable to show different happiness levels.
ggplot(happiness_data, aes(x = income)) +
  geom_bar(aes(fill = as.factor(vhappy)), position = "dodge") +
  scale_fill_manual(values=color_palette) +
  theme_minimal() +
  labs(title = "Count of Income Ranges by VHappy Levels",
       x = "Income Range",
       y = "Count",
       fill = "VHappy Level") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ theme(legend.position="bottom")

library(ggplot2)
library(dplyr)

# Convert 'income' to a factor and 'vhappy' to a factor
happiness_data$income <- factor(happiness_data$income, levels = unique(happiness_data$income[!is.na(happiness_data$income)]))
happiness_data$vhappy <- as.factor(happiness_data$vhappy)

# Calculate the count for each income range and vhappy level
count_data <- happiness_data %>%
  group_by(income, vhappy) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup()

# Reorder the income levels based on the total count for each level
count_data <- count_data %>%
  mutate(income = reorder(income, count))

# Define the color palette
color_palette <- c("#395a7f", "#6e9fc1")

# Create a horizontal bar plot with sorted bars
ggplot(count_data, aes(y = income, x = count, fill = vhappy)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  coord_flip() +  # Flip the coordinates to make the bars horizontal
  theme_minimal() +
  labs(title = "Count of Income Ranges by VHappy Levels",
       y = "Income Range",
       x = "Count",
       fill = "VHappy Level") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 45))

# Note: Replace 'path_to_your_data.csv' with the actual path to your dataset.

# Now create the horizontal bar plot with sorted bars
ggplot(count_data, aes(x = income, y = count, fill = vhappy)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Make bars horizontal
  scale_fill_manual(values = c("not happy" = "#395a7f", "happy" = "#6e9fc1")) +
  theme_minimal() +
  labs(title = "Count of Income Ranges by VHappy Levels",
       x = "Income Range",
       y = "Count",
       fill = "VHappy Level") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())  # Horizontal text for x-axis labels

# Note: This code assumes that 'vhappy' has already been c

library(ggplot2)
library(dplyr)

# Assuming 'happiness_data' has a 'teens' column, and you've already preprocessed 'income' and 'vhappy'

# Convert 'income' to a factor ordered by the count of each level
happiness_data$income <- factor(happiness_data$income, levels = unique(happiness_data$income[!is.na(happiness_data$income)]))

# Calculate the count for each income range and vhappy level
count_data <- happiness_data %>%
  group_by(income, vhappy) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup()

# Calculate the count of teens for each income range and vhappy level
# Replace 'teens_column_name' with the actual column name for teens in your dataset
teens_count_data <- happiness_data %>%
  group_by(income, vhappy) %>%
  summarise(teens_count = sum(teens, na.rm = TRUE), .groups = 'drop') %>%
  ungroup()

# Merge the overall counts with the teens counts
merged_data <- merge(count_data, teens_count_data, by = c("income", "vhappy"))

# Reorder the income factor levels based on the count
merged_data$income <- reorder(merged_data$income, merged_data$count)

# Define the color palette
color_palette <- c("not happy" = "#395a7f", "happy" = "#6e9fc1")

# Create a horizontal bar plot with sorted bars and add teens count inside the bars
ggplot(merged_data, aes(x = income, y = count, fill = vhappy)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = teens_count), position = position_dodge(width = 0.9), hjust = 0.5, vjust = -0.5) +
  coord_flip() +  # Make bars horizontal
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "Count of Income Ranges by VHappy Levels",
       x = "Income Range",
       y = "Count",
       fill = "VHappy Level") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0))  # Horizontal text for x-axis labels

# Note: Adjust the position and justification of `geom_text` as needed for your data and preference.
# Preprocess the income data
# Replace the income ranges with their midpoints
calculate_average <- function(income_range) {
  if (is.na(income_range)) {
    return(NA)
  } else {
    # Extract the numbers from the range string
    nums <- as.numeric(str_extract_all(income_range, "\\d+")[[1]])
    # Return the average of the first and last number
    return(mean(nums))
  }
}

happiness_data = df

# Apply the function to create a new column
happiness_data$income_numeric <- sapply(happiness_data$income, calculate_average)

# Remove rows with NA in the new income_numeric column
happiness_data <- happiness_data[!is.na(happiness_data$income_numeric), ]

# Define the color palette
color_palette <- c("#395a7f", "#6e9fc1")
# Plot the density if the income_numeric column is correct
if(any(!is.na(happiness_data$income_numeric))) {
  ggplot(happiness_data, aes(x = income_numeric, fill = as.factor(vhappy))) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = color_palette) +
    theme_minimal() +
    labs(title = "Density of Income Ranges by VHappy Levels",
         x = "Income",
         y = "Density",
         fill = "VHappy Level") +
    theme(legend.position = "bottom")
} else {
  print("The income_numeric column contains non-numeric values or is not properly computed.")
}



ggplot(happiness_data, aes(x = income_numeric, fill = as.factor(vhappy))) +
  geom_density(aes(y = ..scaled..), alpha = 0.5) +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "Normalized Density of Income Ranges by VHappy Levels",
       x = "Income",
       y = "Scaled Density",
       fill = "VHappy Level") +
  theme(legend.position = "bottom")



str(df)

ggplot(happiness_data, aes(x = log1p(income_numeric), fill = as.factor(vhappy))) +  # log1p adds 1 before log transformation to handle zero income
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "Log-Normalized Density of Income Ranges by VHappy Levels",
       x = "Log-transformed Income",
       y = "Density",
       fill = "VHappy Level") +
  theme(legend.position = "bottom")



#### Education level vs vhappy 

# Convert 'vhappy' to a factor if it's not already
happiness_data$vhappy <- factor(happiness_data$vhappy, levels = c("not happy", "happy"))

# Filter out NA values in 'unem10'
happiness_data <- happiness_data[!is.na(happiness_data$unem10), ]

# Group by education level and happiness status, then count the number of people
happiness_education_counts <- happiness_data %>%
  group_by(educ, vhappy) %>%
  summarise(count = n(), .groups = 'drop')

# Define the color palette
color_palette <- c("not happy" = "#395a7f", "happy" = "#6e9fc1")

# Create the line plot
ggplot(happiness_education_counts, aes(x = educ, y = count, group = vhappy, color = vhappy)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "Count of Happiness Status by Education Level",
       x = "Education Level (Years)",
       y = "Count of People",
       color = "Happiness Status") +
  coord_flip() + 
  theme(legend.position = "bottom")






# Convert 'vhappy' to a factor if it's not already
happiness_data$vhappy <- factor(happiness_data$vhappy, levels = c("not happy", "happy"))

# Calculate counts of people at each education level for each happiness category
education_counts <- happiness_data %>%
  group_by(educ, vhappy) %>%
  summarise(count = n(), .groups = 'drop')

# Create the plot
ggplot(education_counts, aes(y = educ, x = count)) +
  geom_segment(aes(yend = educ, xend = 0, color = vhappy), size = 1) +
  geom_point(aes(color = vhappy), size = 2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("not happy" = "#395a7f", "happy" = "#6e9fc1")) +
  labs(title = "Count of People by Education Level and Happiness",
       y = "Education Level",
       x = "Count of People",
       color = "Happiness Status") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


#### Employment vs vHappy

# Convert 'vhappy' to a factor if it's not already
happiness_data$vhappy <- factor(happiness_data$vhappy)

# Filter out NA values in 'unem10'
happiness_data <- filter(happiness_data, !is.na(unem10))

# Define the color palette
color_palette <- c("#395a7f", "#6e9fc1")

# Create a grouped bar plot to show the count of each happiness level by employment status
ggplot(happiness_data, aes(x = as.factor(unem10), fill = vhappy)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = color_palette) +
  labs(title = "Happiness Level by Employment Status",
       x = "Employment Status",
       y = "Count",
       fill = "Happiness Level") +
  scale_x_discrete(labels = c("0" = "Employed", "1" = "Unemployed")) +
  theme_minimal() +
  theme(legend.position = "bottom")+ theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ theme(legend.position="bottom")


### prestige



ggplot(happiness_data, aes(x = prestige, fill = as.factor(vhappy))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "Density of prestige Ranges by VHappy Levels",
       x = "prestige",
       y = "Density",
       fill = "VHappy Level") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


### unemploymenbt

library(ggplot2)
library(dplyr)

# Assuming your dataset is already loaded into 'happiness_data'
# and you have columns 'year', 'unem10', and 'vhappy' where 'unem10' represents employment status
# and 'year' represents the year.

# We'll need to calculate the count of each employment status for each year
employment_data <- happiness_data %>%
  group_by(year, unem10) %>%
  summarise(count = n(), .groups = 'drop') %>%
  # Convert the year to a date object for proper plotting
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  ungroup()

# Pivot the data to a wide format suitable for area plots
employment_wide <- employment_data %>%
  pivot_wider(names_from = unem10, values_from = count)

# Now create the area plot with employment status counts over the years
ggplot(employment_wide, aes(x = year)) +
  geom_area(aes(y = 'Unemployed', fill = 'Unemployed')) +  # Replace `1` with the actual factor level or column name for unemployed
  geom_area(aes(y = 'Employed', fill = 'Employed')) +  # Replace `0` with the actual factor level or column name for employed
  scale_fill_manual(values = c('Unemployed' = 'red', 'Employed' = 'blue')) +
  labs(title = "Employment Status Over Years",
       x = "Year",
       y = "Count of Employment Status") +
  theme_minimal()

# Replace `1` and `0` with the actual factor levels or column names for unemployed and employed in your dataset.


############################### added Part ##############################################
### Plots - Result happiness factor
library(fmsb)
library(tidyverse)
# prestige, divorce, educ, babies,tvhours,black, female,
# 43/46 , yes/yes, 13//14, 1/1, 2.8/3.1black/ black ,female/female
x=df %>% filter(!is.na(vhappy),!is.na(prestige),!is.na(educ),!is.na(babies),!is.na(tvhours),babies!=0) %>% group_by(vhappy) %>% summarise(score=mean(prestige),
                                                                                  educ=mean(educ),
                                                                                  babies=mean(babies),
                                                                                  tvhours=mean(tvhours))
df0=x %>% filter(vhappy=='happy')
df1=x %>% filter(vhappy=='not happy')
df0=df0[,-1]
df1=df1[,-1]
df_max=c(50,20,3,5)
df_min=c(0,0,0,0)
df_0=rbind(df_max,df_min,df0,df1)
radarchart(df_0,cglcol=c("grey"),pcol=c("darkblue",'red'))
