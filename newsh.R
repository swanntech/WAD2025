library(tidyverse)
library(moments)
library(dplyr)
library(psych)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
data_base <- read.csv("C:/Users/MSI/Desktop/shopping_trends.csv")


#eksploracja i modyfikacja danych
summary(data_base)
str(data_base)

data <- data_base
data %>% 
  group_by(Frequency.of.Purchases) %>% 
  summarise(count=n()) %>% 
  ungroup()
data <- data %>% 
  mutate(across("Frequency.of.Purchases", ~ case_match(
    .,
    "Annually" ~ "Annually",
    "Every 3 Months" ~ "Every 3 Months",
    "Monthly" ~ "Monthly",
    "Weekly" ~ "Weekly",
    "Quarterly" ~ "Every 3 Months",
    "Fortnightly" ~ "Every 2 Weeks",
    "Bi-Weekly" ~ "Every 2 Weeks"
  )))
purchasef_summary <- data %>%
  group_by(Frequency.of.Purchases) %>%
  summarise(count = n(), .groups = "drop") %>% 
  ungroup()
purchasef_summary$Częstotliwość <- purchasef_summary$Frequency.of.Purchases
ggplot(purchasef_summary, aes(x = reorder(Częstotliwość, Frequency.of.Purchases), y = count, fill=Częstotliwość)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Częstość zakupów",
    x = "Częstotliwość",
    y = "Liczba"
  )
#wykres częstości zakupów ^^



data$Frequency.of.Purchases.N <- data$Frequency.of.Purchases
data<- data %>% 
  mutate(across("Frequency.of.Purchases.N", ~ case_match(
    .,
    "Annually" ~ "Rare",
    "Every 3 Months" ~ "Rare",
    "Monthly" ~ "Medium",
    "Weekly" ~ "Often",
    "Every 2 Weeks" ~ "Often"
  )))

data %>% 
  group_by(Frequency.of.Purchases.N) %>% 
  summarise(count=n()) %>% 
  ungroup()


data %>% 
  group_by(Payment.Method) %>% 
  summarise(count=n()) %>% 
  ungroup()
data <- data %>% 
  mutate(across("Payment.Method", ~ case_match(
    .,
    "Bank Transfer" ~ "Bank Transfer",
    "Credit Card" ~ "Card",
    "Debit Card" ~ "Card",
    "Cash" ~ "Cash",
    "PayPal" ~ "OnlineService",
    "Venmo" ~ "OnlineService"
  )))
data %>% 
  group_by(Payment.Method) %>% 
  summarise(count=n()) %>% 
  ungroup()


summary(data$Age)
data$AgeGroup <- cut(data$Age, breaks=c(0, 30, 45, 55, 70), labels=c("18-30", "31-45", "46-55", "55+"))

summary(data$Purchase.Amount..USD.)
data$Spending <- cut(data$Purchase.Amount..USD., breaks=c(0, 39, 60, 100), labels=c("Low", "Medium", "High"))

#wykres ilości zakupów w różnych grupach wiekowych
purchaseg_summary <- data %>%
  group_by(AgeGroup) %>%
  summarise(count = n(), .groups = "drop") %>% 
  ungroup()
ggplot(purchaseg_summary, aes(x = reorder(AgeGroup, AgeGroup), y = count, fill=AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Zakupy w różnych grupach wiekowych",
    x = "Wiek",
    y = "Liczba")

data <- data[ -c(1,7:8,11:12,14,16,18) ]


data %>% 
  group_by(Previous.Purchases) %>% 
  summarise(count=n()) %>% 
  ungroup()
summary(data$Previous.Purchases)
qplot(x=Previous.Purchases, data= data)
data$Previous.Purchases.N <- cut(data$Previous.Purchases, breaks=c(0, 15, 25, 35, 50), labels=c("1-15", "16-25", "26-35", "36-50"))
data %>% 
  group_by(Previous.Purchases.N) %>% 
  summarise(count=n()) %>% 
  ungroup()

color_mapping <- c(
  "Indigo" = "niebieski",
  "Blue" = "niebieski",
  "Cyan" = "niebieski",
  "Teal" = "niebieski",
  "Turquoise" = "niebieski",
  "Purple" = "fioletowy",
  "Violet" = "fioletowy",
  "Lavender" = "fioletowy",
  "Brown"="brązowy",
  "Beige"="brązowy",
  "White"="biały",
  "Red" = "czerwony",       
  "Maroon" = "czerwony",
  "Yellow" = "żółty",
  "Orange" = "żółty",
  "Green" = "zielony",
  "Olive" = "zielony",
  "Pink" = "różowy",
  "Magenta" = "różowy",
  "Peach" = "różowy",
  "Gray" = "szary",
  "Black" = "czarny",
  "Charcoal" = "szary",
  "Silver"= "srebrny",
  "Gold"="złoty"
)

data <- data %>%
  mutate(Color = recode(Color, !!!color_mapping))


#testy
table(data$Spending)
data %>% 
  select(Spending) %>% 
  table() %>% 
  chisq.test() #p=2.2e-16

table(data$AgeGroup)
data %>% 
  select(AgeGroup) %>% 
  table() %>% 
  chisq.test() #p=2.2e-16

table(data$Frequency.of.Purchases)
data %>% 
  select(Frequency.of.Purchases) %>% 
  table() %>% 
  chisq.test() #p=2.2e-16

table(data$Frequency.of.Purchases.N)
data %>% 
  select(Frequency.of.Purchases) %>% 
  table() %>% 
  chisq.test() #p=2.2e-16

table(data$Category)
data %>% 
  select(Category) %>% 
  table() %>% 
  chisq.test() #p=2.2e-16

table(data$Item.Purchased)
data %>% 
  select(Item.Purchased) %>% 
  table() %>% 
  chisq.test() #p=0.72  (>0,05)

table(data$Season)
data %>% 
  select(Season) %>% 
  table() %>% 
  chisq.test()#p=0.79  (>0.05)

table(data$Color)
data %>% 
  select(Color) %>% 
  table() %>% 
  chisq.test()#p=2.2e-16

table(data$Payment.Method)
data %>% 
  select(Payment.Method) %>% 
  table() %>% 
  chisq.test()#p=2.2e-16

table(data$Previous.Purchases)
data %>% 
  select(Previous.Purchases) %>% 
  table() %>% 
  chisq.test()#p=0.3 (>0.05)
table(data$Previous.Purchases.N)
data %>% 
  select(Previous.Purchases.N) %>% 
  table() %>% 
  chisq.test()#p=2.2e-16 

chisq.test(table(data$AgeGroup, data$Item.Purchased)) #p-value = 0.14587
chisq.test(table(data$Gender, data$Discount.Applied)) #p-value = 2.2e-16
chisq.test(table(data$Gender, data$Previous.Purchases)) #p-value = 0.02867
chisq.test(table(data$Gender, data$Previous.Purchases.N)) #p-value = 0.037
chisq.test(table(data$AgeGroup, data$Previous.Purchases.N)) #p-value = 0.23
chisq.test(table(data$Frequency.of.Purchases.N, data$Spending)) #p-value = 0.45

#doMCA
doMCA1 <- data %>%
  select(Discount.Applied,Previous.Purchases, Gender, AgeGroup)

doMCA1 <- doMCA1 %>%
  mutate(Discount.Applied = as.factor(Discount.Applied),
         AgeGroup = as.factor(AgeGroup),
         Gender = as.factor(Gender),
         Previous.Purchases = as.numeric(Previous.Purchases))

wynikMCA1 <- MCA(doMCA1, quanti.sup = 2)

summary(wynikMCA1)
plot.MCA(wynikMCA1)
fviz_mca_ind(wynikMCA1, repel = T)

fviz_contrib(wynikMCA1, choice = "var", axes = 1:2, top = 10)
fviz_contrib(wynikMCA1, choice = "var", axes = 1, top = 10)
fviz_contrib(wynikMCA1, choice = "var", axes = 2, top = 10)
fviz_contrib(wynikMCA1, choice = "var", axes = 1:3, top = 10)
fviz_screeplot(wynikMCA1, addlabels = TRUE)
fviz_mca_var(wynikMCA1, axes = c(1, 2), col.var = "contrib", gradient.cols = c("blue", "green", "red"))


doMCA2 <- data %>%
  select(Gender, Discount.Applied, Spending, AgeGroup, Payment.Method,Frequency.of.Purchases, Previous.Purchases.N)

wynikMCA2 <- MCA(doMCA2)
fviz_screeplot(wynikMCA2, addlabels = TRUE)
summary(wynikMCA2)
plot.MCA(wynikMCA2)
fviz_mca_ind(wynikMCA2, repel = T)

fviz_contrib(wynikMCA2, choice = "var", axes = 1:2, top = 10)
fviz_contrib(wynikMCA2, choice = "var", axes = 1, top = 10)
fviz_contrib(wynikMCA2, choice = "var", axes = 2, top = 10)
fviz_contrib(wynikMCA2, choice = "var", axes = 1:3, top = 10)

fviz_mca_var(wynikMCA2, axes = c(1, 2), col.var = "contrib", gradient.cols = c("lightblue", "purple", "red"))


doMCA3 <- data %>%
  select(Discount.Applied,Previous.Purchases.N, Gender, AgeGroup)
wynikMCA3 <- MCA(doMCA3)
fviz_screeplot(wynikMCA3, addlabels = TRUE)
fviz_mca_var(wynikMCA3, axes = c(1, 2), col.var = "contrib", gradient.cols = c("lightblue", "purple", "red"))

doMCA4 <- data %>%
  select(Discount.Applied,Previous.Purchases.N, Gender, AgeGroup, Frequency.of.Purchases.N)
wynikMCA4 <- MCA(doMCA4)
fviz_screeplot(wynikMCA4, addlabels = TRUE)
fviz_mca_var(wynikMCA4, axes = c(1, 2), col.var = "contrib", gradient.cols = c("lightblue", "purple", "red"))
