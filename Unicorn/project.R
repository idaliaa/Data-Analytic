library(readr)
library(janitor)
library(dplyr)
library(Hmisc)
library(tidyverse)
library(plotly)

##### CLEANING #####

data <- read_csv("Downloads/List of Unicorns in the World.csv")
data <- data %>% clean_names()
data$x1 <- seq_len(nrow(data))  # start the x1 from 1
sum(is.na(data)) # no missing value
data$date_joined <- as.Date(data$date_joined, format = "%m/%d/%Y")
data$month_year <- format(as.Date(data$date_joined, format = "%m/%d/%Y"), "%b %Y")
data$valuation <- round(as.numeric(sub("\\$", "",data$valuation_b)), 2) # remove Dolar sign


summary(data)
glimpse(data)

##### COMPANY vs. COUNTRY ######
countries <- data %>%
            group_by(country) %>%
            count(country) %>%
            arrange(desc(n)) %>%
            rename(total = n)

# Visualization
ggplot(countries, aes(x=total, y=reorder(country, total))) +
    geom_bar(stat="identity", color="black", fill="lightgreen") +
    labs(x="Total Countries", y="Country", title="Total Company in Each Country") +
    geom_text(aes(label=total), hjust=-0.5, color="black", size=3) +
    theme(panel.grid = element_blank())
 


# City with most company 
# Various = all cities with total company < 6
city_company <- data %>%
                group_by(city) %>%
                count(city) %>%
                mutate(city = if_else(n<6, "various", as.character(city))) %>%
                group_by(city) %>%
                summarise(total=sum(n)) %>%
                arrange(desc(total))
              
# Visualization
ggplot(city_company, aes(x=total, y=reorder(city, total))) +
  geom_bar(stat="identity", fill="pink", color="black") +
  labs(x="Total Company", y="City", title="Total Company in Each Cities") +
  geom_text(aes(label=total), hjust=-0.5, color="black", size=3) +
  theme(panel.grid = element_blank())



###### COMPANY & INDUSTRY #######
media_company <- data %>%
                group_by(industry) %>%
                count(industry) %>%
                arrange(desc(n)) %>%
                rename(total = n)

# Visualization
ggplot(media_company, aes(x=reorder(industry,total), y=total, fill=industry)) +
    geom_bar(stat="identity", width = 0.5) +
    theme(panel.grid = element_blank()) +
    geom_text(aes(label=total), vjust=-0.5, size=3, color="black") +
    labs(x="Industry", y="Total", title="Total Industry Type")

media_company_country <- data %>%
                group_by(country, industry) %>%
                count(industry) %>%
                arrange(desc(n)) %>%
                rename(total = n) %>%
                head(20)

# Visualization
ggplot(media_company_country, aes(x=industry, y=total, group=country)) +
    geom_line() +
    geom_point() +
    theme(panel.grid = element_blank()) +
    geom_text(aes(label=total), hjust=-0.5, vjust=-0.5, size=3, color="black") +
    labs(x="Industry", y="Total", title="Total Industry Type by Company") 


###### COMPANY & VALUATION #######
company_valuation <- data %>%
                    group_by(company, valuation) %>%
                    arrange(desc(valuation)) %>%
                    select(company, valuation) %>%
                    head(15)


# Visualize 
ggplot(company_valuation, aes(y=reorder(company,valuation), x=valuation)) +
    geom_bar(stat="identity", color="black", fill="darkred") +
    labs(x="Total Valuation in Billion Dolllar", y="Company Name", title="Top 15 Valuation of Company") +
    geom_text(aes(label=valuation), hjust=-0.5, color="black", size=3) +
    geom_vline(xintercept = mean(company_valuation$valuation), linetype="dashed", color="darkblue") +
    theme(panel.grid = element_blank())


##### DATE JOINED & COMPANY & VALUATION ########
company_valuation_time <- data %>%
                          group_by(company, valuation, month_year) %>%
                          select(company, valuation, month_year) %>%
                          head(15)

  

all_valuation <- data %>%
                filter(valuation > mean(valuation)) %>%
                group_by(company, valuation) %>%
                arrange(desc(valuation)) %>%
                select(company, valuation)


p <-  ggplot(all_valuation, aes(x=company, y=valuation, text = company)) +
      geom_bar(stat="identity", color="black", fill="darkred") +
      labs(x="Total Valuation in Billion Dolllar", y="Company Name", title="Valuation of Company") +
      geom_text(aes(label=valuation), hjust=-0.5, color="black", size=3) +
      geom_hline(yintercept = mean(all_valuation$valuation), linetype="dashed", color="darkblue") +
      theme(panel.grid = element_blank(), axis.text.x = element_blank()) 

ggplotly(p, tooltip = "text")



###### VALUATION COUNTRY #######
valuation_country <- data %>%
                      group_by(country) %>%
                      summarise(total_valuation = sum(valuation)) %>%
                      arrange(desc(total_valuation))
  
# Visualization
p <-  ggplot(valuation_country, aes(x=reorder(country, -total_valuation), y=total_valuation, text=paste(country, "<br>",total_valuation))) +
    geom_bar(stat="identity", color="black", fill="darkred", width = 0.8) +
    labs(x="Total Valuation in Billion Dolllar", y="Country", title="Valuation of Country") +
    geom_hline(yintercept = mean(valuation_country$total_valuation), linetype="dashed") +
    theme(panel.grid = element_blank(), axis.text.x = element_blank()) 

ggplotly(p, tooltip = "text")


write.csv(data, "new_data.csv", row.names = FALSE)
  


