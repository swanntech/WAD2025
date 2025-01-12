library(tidyverse)
library(moments)
library(dplyr)
library(psych)
library(ggcorrplot)
library(ggplot2)
library(FactoMineR)
library(factoextra)
data_base <- read.csv("C:/Users/MSI/Desktop/shopping_trends.csv")


#🟥🟥🟥eksploracja i modyfikacja danych
summary(data_base)
str(data_base)

#Baza zawiera dane o 3900 klientach
#Wszyscy znajdują się w wieku 18-70
#Znacząco więcej mężczyzn niż kobiet, 2652 do 1248
#Średnia cena produktów: $60
#Ceny kupowanych produktów w zakresie $20-$100, jednak brak możliwości sprawdzenia dokładnie zachowań klientów w tym zakresie ze względu na brak danych o ich poprzednich transakcjach  
#Oceny produktów w skali 1-5, z zebranych danych oceniane od 2.5 do 5.0 - niejasne jak zliczane są oceny, najprawdopodobniej jest to średnia ocen zostawianych przez klienta, jednak w bazie nie mamy wglądu do poprzednich jego zakupów

data <- data_base

#🔵podział na płeć
data_w <- data %>%
  filter(Gender=="Female")
data_m <- data %>%
  filter(Gender=="Male")
  #⚫ zdecydowana przewaga mężczyzn wśród klientów; 2652 mężczyzn i 1248 kobiet

  #🔵analiza częstotliwości zakupów
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


  #📈wykres czestotliwości zakupów

ggplot(purchasef_summary, aes(x = reorder(Częstotliwość, Frequency.of.Purchases), y = count, fill=Częstotliwość)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Częstość zakupów",
    x = "Częstotliwość",
    y = "Liczba"
  )
    #⚫Najwięcej klientów robi zakupy co 3 miesiące (~ około 4 razy do roku)
    #⚫Najmniej klientów robi zakupy co tydzień, co miesiąc oraz raz do roku
    #⚫najmniej klientów robiących zakupy w skrajnych częstotliwościach ?

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

#====================================================================================================================

#🔵metoda płatności

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

  #⚫ najczęściej wybieraną formą płatności jest: karta

#====================================================================================================================

#🔵wiek a ilość zakupów
summary(data$Age)
data$AgeGroup <- cut(data$Age, breaks=c(0, 30, 45, 55, 70), labels=c("18-30", "31-45", "46-55", "55+"))

summary(data$Purchase.Amount..USD.)
data$Spending <- cut(data$Purchase.Amount..USD., breaks=c(0, 39, 60, 100), labels=c("Low", "Medium", "High"))

purchaseg_summary <- data %>%
  group_by(AgeGroup) %>%
  summarise(count = n(), .groups = "drop") %>% 
  ungroup()


#📈wykres ilości zakupów w różnych grupach wiekowych

ggplot(purchaseg_summary, aes(x = reorder(AgeGroup, AgeGroup), y = count, fill=AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Zakupy w różnych grupach wiekowych",
    x = "Wiek",
    y = "Liczba")

    #⚫najmniej zakupów robią osoby w przedziale wiekowym 46-55

#📈wykres ilości zakupów w różnych grupach wiekowych (przeskok co około 10 lat) 
# różnice wynikające z braku podzielności przedziału na 10 (min przeskok 9 lat, max przeskok 11 lat)

 bins <- c(18, 30, 40, 50, 60, 70)
      labels <- c("18-29", "30-39", "40-49", "50-59", "60-70")
      data$Age.Group <- cut(data$Age, breaks = bins, labels = labels, right = FALSE)
      
      age_group_counts <- as.data.frame(table(data$Age.Group))
      colnames(age_group_counts) <- c("Age.Group", "Count")
      
      library(ggplot2)
      
      ggplot(age_group_counts, aes(x = Age.Group, y = Count, fill = Age.Group)) +
        geom_bar(stat = "identity", color = "black") +
        scale_fill_manual(values = colorRampPalette(c("lightblue", "navy"))(length(labels))) +
        labs(title = "Liczba klientów w przedziałach wiekowych", 
             x = "Wiek", 
             y = "Liczba klientów") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

#====================================================================================================================

#🔵analiza ilości poprzednich zakupów

data <- data[ -c(1,7:8,11:12,14,16,18) ]

data %>% 
  group_by(Previous.Purchases) %>% 
  summarise(count=n()) %>% 
  ungroup()
summary(data$Previous.Purchases)

#📈wykres ilości poprzednich zakupów
qplot(x=Previous.Purchases, data= data)
data$Previous.Purchases.N <- cut(data$Previous.Purchases, breaks=c(0, 15, 25, 35, 50), labels=c("1-15", "16-25", "26-35", "36-50"))
data %>% 
  group_by(Previous.Purchases.N) %>% 
  summarise(count=n()) %>% 
  ungroup()
#⚫ najwięcej poprzednich zakupów mieściło się między 1-15 (1179 klientów) oraz 36-50 (1147 klientów)

#====================================================================================================================

#🔵Pora roku a ilość dokonanych zakupów
data %>% 
  group_by(Season) %>%  
  summarise(total_count = n()) %>%  
  ungroup() 

  #⚫najwięcej zakupów klienci robią: wiosną


#🔵Pora roku a ilość wydanych przez klientów pieniędzy
data %>% 
  group_by(Season) %>%  
  summarise(total_spent = sum(Purchase.Amount..USD.)) %>%  
  ungroup() 
  
  #⚫najwięcej klienci wydają jesienią



#====================================================================================================================

#🔵zgrupowanie kolorów dla przejrzystości wykresów i łatwiejszej analizy

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

#--------------------------------------------------

#🔵trendy w kolorach u mężczyzn
color_summary_m <- data_m %>%
  group_by(Gender, Color) %>%
  summarise(count = n(), .groups = "drop")


#📈 Wykres trendów w kolorach u mężczyzn
ggplot(color_summary_m, aes(x = reorder(Color, count), y = count, fill=Color )) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Kolory najczęściej wybierane przez mężczyzn",
    x = "Kolor",
    y = "Liczba"
  ) +
  scale_fill_manual(values=c("white", 
                             "brown", 
                             "black", 
                             "red",
                             "purple",
                             "blue",
                             "pink",
                             "gray",
                             "darkgray",
                             "green",
                             "gold",
                             "yellow"))

#--------------------------------------------------

#🔵trendy w kolorach u kobiet
color_summary_w <- data_w %>%
  group_by(Gender, Color) %>%
  summarise(count = n(), .groups = "drop")\


#📈Wykres w trendach kolorów u kobiet
ggplot(color_summary_w, aes(x = reorder(Color, count), y = count, fill=Color )) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Kolory najczęściej wybierane przez kobiety",
    x = "Kolor",
    y = "Liczba"
  ) +
  scale_fill_manual(values=c("white", 
                             "brown", 
                             "black", 
                             "red",
                             "purple",
                             "blue",
                             "pink",
                             "gray",
                             "darkgray",
                             "green",
                             "gold",
                             "yellow"))



#====================================================================================================================

#🟦📈🟦 WYKRESY


#📈rozkład wieku
ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 53, fill = "#009999", color = "black") +
  geom_density(aes(y = ..count..), color = "#660099", size = 1) +
  labs(
    title = "Rozkład wieku klientów",
    x = "Wiek",
    y = "Liczba klientów"
  ) 

#--------------------------------------------------

#📈rozkład płci
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Liczba klientów według płci",
    x = "Płeć",
    y = "Liczba klientów"
  )

#--------------------------------------------------

#📈rozkład kwoty zakupu dla grup wiekowych
ggplot(data, aes(x = AgeGroup, y = Purchase.Amount..USD., fill = AgeGroup)) +
  geom_boxplot() +
  labs(
    title = "Rozkład kwoty zakupu dla grup wiekowych",
    x = "Grupy wiekowe",
    y = "Kwota zakupu"
  )

#--------------------------------------------------

#📈rozkład ilości poprzednich zakupów dla grup wiekowych
ggplot(data, aes(x = AgeGroup, y = Previous.Purchases, fill = AgeGroup)) +
  geom_boxplot() +
  labs(
    title = "Rozkład ilości poprzednich zakupów dla grup wiekowych",
    x = "Grupy wiekowe",
    y = "Kwota zakupu"
  )

#====================================================================================================================

#🟨🟨🟨TESTY 

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


#====================================================================================================================

#🟪🟪🟪 MODELE MCA 🟪🟪🟪


#🟪MCA1
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
fviz_mca_biplot(wynikMCA1, label = "all", 
                col.var = "blue", col.ind = "red", 
                title = "MCA Biplot")
fviz_mca_var(wynikMCA1, 
             col.var = "red", 
             title = "MCA - Variables")

#--------------------------------------------------

#🟪MCA2
doMCA2 <- data %>%
  select(Gender, Discount.Applied, Spending, AgeGroup, Payment.Method,Frequency.of.Purchases, Previous.Purchases.N)

wynikMCA2 <- MCA(doMCA2)
fviz_screeplot(wynikMCA2, addlabels = TRUE)
summary(wynikMCA2)
plot.MCA(wynikMCA2)
fviz_mca_ind(wynikMCA2, repel = T)

fviz_contrib(wynikMCA2, choice = "var", axes = 1:3, top = 10)

fviz_mca_var(wynikMCA2, axes = c(1, 2), col.var = "contrib", gradient.cols = c("lightblue", "purple", "red"))
fviz_mca_biplot(wynikMCA2, label = "all", 
                col.var = "blue", col.ind = "red", 
                title = "MCA Biplot") #wygląda sensownie
fviz_mca_var(wynikMCA2, 
             col.var = "red", 
             title = "MCA - Variables")

#--------------------------------------------------

#🟪MCA3

doMCA3 <- data %>%
  select(Discount.Applied,Previous.Purchases.N, Gender, AgeGroup)
wynikMCA3 <- MCA(doMCA3)
summary(wynikMCA3)
plot.MCA(wynikMCA3)
fviz_screeplot(wynikMCA3, addlabels = TRUE)
fviz_mca_var(wynikMCA3, axes = c(1, 2), col.var = "contrib", gradient.cols = c("lightblue", "purple", "red"))
fviz_mca_ind(wynikMCA3, repel = T)
fviz_mca_biplot(wynikMCA3, label = "all", 
                col.var = "blue", col.ind = "red", 
                title = "MCA Biplot")
fviz_mca_var(wynikMCA3, 
             col.var = "red", 
             title = "MCA - Variables")

#--------------------------------------------------

#🟪MCA4

doMCA4 <- data %>%
  select(Discount.Applied,Previous.Purchases.N, Gender, AgeGroup, Frequency.of.Purchases.N)
wynikMCA4 <- MCA(doMCA4)
summary(wynikMCA4)
plot.MCA(wynikMCA4)
fviz_screeplot(wynikMCA4, addlabels = TRUE)
fviz_mca_var(wynikMCA4, axes = c(1, 2), col.var = "contrib", gradient.cols = c("lightblue", "purple", "red"))
fviz_mca_ind(wynikMCA4, repel = T)

fviz_mca_var(wynikMCA4, 
             col.var = "red", 
             title = "MCA - Variables")
fviz_mca_biplot(wynikMCA4, label = "all", 
                col.var = "blue", col.ind = "red", 
                title = "MCA Biplot")

#--------------------------------------------------

#🟪MCA5

doMCA5 <- data %>%
  select(Gender, Discount.Applied, Spending, AgeGroup, Payment.Method,Frequency.of.Purchases.N, Previous.Purchases.N)

wynikMCA5 <- MCA(doMCA5)
fviz_screeplot(wynikMCA5, addlabels = TRUE)
summary(wynikMCA5)
plot.MCA(wynikMCA5)
fviz_mca_ind(wynikMCA5, repel = T)

fviz_contrib(wynikMCA5, choice = "var", axes = 1:2, top = 10)

fviz_mca_var(wynikMCA5, axes = c(1, 2), col.var = "contrib", gradient.cols = c("lightblue", "purple", "red"))
fviz_mca_biplot(wynikMCA5, label = "all", 
                col.var = "blue", col.ind = "red", 
                title = "MCA Biplot") #wygląda sensownie
fviz_mca_var(wynikMCA5, 
             col.var = "red", 
             title = "MCA - Variables")
#ostatecznie: wybór analizy MCA ze zmiennymi:
#Płeć(Gender), Zastosowanie zniżki(Discount.Applied),
#Wydatki(Spending) - zgrupowanie danych w trzy kategorie na podstawie rozkładu ćwiartkowego,
#Grupa wiekowa(AgeGroup) zgrupowanie danych w cztery kategorie na podstawie rozkładu ćwiartkowego,
#Metoda Płatności(Payment.Method) - zgrupowanie danych w cztery kategorie na podstawie działania formy płatności,
#Częstotliwość Zakupów(Frequency.of.Purchases.N) - zmienna jakościowa, dane zgrupowane w celu uniknięcia redundancji a dodatkowo zredukowane do trzech najważniejszych kategorii,
#Ilość poprzednich zakupów(Previous.Purchases.N) -  zgrupowanie danych w cztery kategorie na podstawie rozkładu ćwiartkowego

#--------------------------------------------------

#🟪Finalny model MCA

MCA_final <- doMCA5
wynikMCAfinal <- MCA(MCA_final)



