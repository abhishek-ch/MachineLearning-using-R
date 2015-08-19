library(ggplot2)
colnames(country) <- c("Country", "Frequency")
country_noUSA <- country[which(country$Country!="United States"),]


# With USA
country$Country <- factor(country$Country, 
                          levels=country$Country[order(country$Frequency)])

p <- ggplot(country, aes(x=Country, y=Frequency)) + 
      geom_bar(stat="identity", fill="#53cfff") +
      coord_flip() +
      ggtitle("Kaggle Users by Location (with USA)")

ggsave(p, file="Kaggle_Users_by_Location.png", width=16, height=12)


# Without USA
country_noUSA$Country <- factor(country_noUSA$Country, 
                          levels=country_noUSA$Country[order(country_noUSA$Frequency)])

g <- ggplot(country_noUSA, aes(x=Country, y=Frequency)) +
      geom_bar(stat="identity", fill="#53cfff") +
      coord_flip() +
      ggtitle("Kaggle Users by Location (without USA)")

ggsave(g, file="Kaggle_Users_by_Location-wo_USA.png", width=16, height=12)
