library(tidyverse)
library(ggplot2)
library(scales)
trend<- read_csv("report_trend_2012-2023.csv")

head(trend)

Trend1<-trend%>%
  select(Year, quarter, "No of Reports", "No of Counties")%>%
  mutate(quarter=recode(quarter, "Q1"="03", "Q2"="06", "Q3"="09", "Q4"="12"))%>%
  rename("reports" = "No of Reports", "counties" = "No of Counties" )%>%
  mutate(date=as.Date(paste0(as.character(Year)," ", quarter," ", "01"),"%Y %m %d"))
  
head(Trend1)


###############################################################################

colors<-c("Number of reports"="red", "Number of counties"="blue")

ggplot()+
  geom_smooth(data=Trend1, aes(x=date, y=reports, color="Number of reports"))+
  geom_point(data=Trend1, aes(x=date, y=reports, color="Number of reports"), size=0.75 )+
  theme_bw()+
  geom_smooth(data=Trend1, aes(x=date, y=counties*70, color="Number of counties"))+
  geom_point(data=Trend1, aes(x=date, y=counties*70, color="Number of counties"),size=0.75)+
  scale_y_continuous(breaks=c(seq(0,3600, by=300)),
                     name = "Number of reports",
                     sec.axis = sec_axis(~./40,breaks = c(seq(0,90,10)), 
                                         name = "Proportion of counties",
                    labels = function(b) { paste0(round(b, 0), "%")})) +
  scale_x_date(
    labels = scales::date_format("%Y"),
    breaks = scales::date_breaks("1 year")     
               ) +
  labs(color="", x="Year") +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    #text = element_text(face="bold"), 
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "top")+
  scale_color_manual(values=colors)

dev.off()
ggsave("Trend_of_reports2.png", width=14, height=8, units = c("cm"), dpi = 1000)

####################################################################
# Load the ggplot2 library
library(ggplot2)

# Define custom colors
colors <- c("Number of reports" = "blue", "Number of counties" = "red")

# Create a ggplot
ggplot(Trend1, aes(x = date)) +
  # Geom for smoothed lines and points for "Number of reports"
  geom_smooth(aes(y = reports, color = "Number of reports"), size = 0.75) +
  geom_point(aes(y = reports, color = "Number of reports"), size = 0.75) +
  
  # Geom for smoothed lines and points for "Number of counties"
  geom_smooth(aes(y = counties * 70, color = "Number of counties"), size = 0.75) +
  geom_point(aes(y = counties * 70, color = "Number of counties"), size = 0.75) +
  
  # Define primary y-axis (left)
  scale_y_continuous(breaks = seq(0, 3600, by = 300), name = "Number of reports") +
  
  # Define secondary y-axis (right)
  scale_y_continuous(
    sec.axis = sec_axis(~./40, breaks = seq(0, 90, 10), name = "Proportion of counties",
                        labels = function(b) { paste0(round(b, 0), "%") })
  ) +
  
  # Define x-axis breaks
  scale_x_continuous(breaks = seq(min(Trend1$Year), max(Trend1$Year), by = 1)) +
  
  # Other theme adjustments
  theme_bw() +
  labs(color = "", x = "Year") +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    text = element_text(face = "bold", size = 20),
    legend.position = "top"
  ) +
  scale_color_manual(values = colors)
##############################################################################                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                       