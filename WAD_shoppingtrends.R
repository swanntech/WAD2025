#Podział pracy: 
#Anna: Eksploracja, wstępna analiza i opis zbioru, grupowanie i redukcja zmiennych, wykresy,  koordynacja projektu   
#Emilia: Porządkowanie kodu, wizualne opracowanie prezentacji, analiza poszczególnych zmiennych i wykresy, prezentacja
#Marta: Analiza zmiennych, model MCA oraz jego interpretacja, prezentacja
#Zuzanna: Analiza zmiennych, wykresy, , prezentacja 
#Ze względu na wiele podejść do bazy danych i związanych z nią problemów, każda z nas w pewnym stopniu brała udział w analizie zbioru oraz wykresów,
#aby zapewnić możliwie jak najwięcej perspektyw na prowadzoną analizę. Nie wszystkie efekty pracy każdej z nas są więc widoczne w poniższym kodzie.



library(tidyverse)
library(moments)
library(dplyr)
library(psych)
library(ggcorrplot)
library(ggplot2)
library(FactoMineR)
library(factoextra)
data_base <- read.csv("C:/Users/MSI/Desktop/shopping_trends.csv")


#eksploracja i modyfikacja danych
summary(data_base)
str(data_base)

numeric_data <- data_base[sapply(data_base, is.numeric)]
numeric_data <- numeric_data[, !colnames(numeric_data) %in% "Customer.ID"]

#cor_matrix <- cor(numeric_data)
cor_matrix <- cor(numeric_data, use = "complete.obs", method = "kendall")

cor_df <- as.data.frame(as.table(cor_matrix)) 

ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

table_summary <- table(data_base$Subscription.Status, data_base$Discount.Applied)
print(table_summary)

#Baza zawiera dane o 3900 klientach
#Wszyscy znajdują się w wieku 18-70
#Znacząco więcej mężczyzn niż kobiet, 2652 do 1248
#Średnia cena produktów: $60
#Ceny kupowanych produktów w zakresie $20-$100, jednak brak możliwości sprawdzenia dokładnie zachowań klientów w tym zakresie ze względu na brak danych o ich poprzednich transakcjach  
#Oceny produktów w skali 1-5, z zebranych danych oceniane od 2.5 do 5.0 - niejasne jak zliczane są oceny, najprawdopodobniej jest to średnia ocen zostawianych przez klienta, jednak w bazie nie mamy wglądu do poprzednich jego zakupów
#Brak korelacji między danymi ilościowymi

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

#Pora roku a ilość dokonanych zakupów
data %>% 
  group_by(Season) %>%  
  summarise(total_count = n()) %>%  
  ungroup() 
#najwięcej zakupów klienci robią: wiosną

#Pora roku a ilość wydanych przez klientów pieniędzy
data %>% 
  group_by(Season) %>%  
  summarise(total_spent = sum(Purchase.Amount..USD.)) %>%  
  ungroup() #najwięcej klienci wydają jesienią

# wykresy

# rozkład wieku
ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 53, fill = "#009999", color = "black") +
  geom_density(aes(y = ..count..), color = "#660099", size = 1) +
  labs(
    title = "Rozkład wieku klientów",
    x = "Wiek",
    y = "Liczba klientów"
  ) 

# rozkład płci
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Liczba klientów według płci",
    x = "Płeć",
    y = "Liczba klientów"
  )
# rozkład wydanych kwot

ggplot(data, aes(x = Purchase.Amount..USD.)) +
  geom_histogram(bins = 81, fill = "#666600", color = "black") +
  geom_density(aes(y = ..count..), color = "#990033", size = 1) +
  labs(
    title = "Rozkład kwot",
    x = "Kwota",
    y = "Liczba wystąpień"
  ) +
  theme_minimal()

# rozkład kwoty zakupu dla grup wiekowych
ggplot(data, aes(x = AgeGroup, y = Purchase.Amount..USD., fill = AgeGroup)) +
  geom_boxplot() +
  labs(
    title = "Rozkład kwoty zakupu dla grup wiekowych",
    x = "Grupy wiekowe",
    y = "Kwota zakupu"
  )

# rozkład ilości poprzednich zakupów dla grup wiekowych
ggplot(data, aes(x = AgeGroup, y = Previous.Purchases, fill = AgeGroup)) +
  geom_boxplot() +
  labs(
    title = "Rozkład ilości poprzednich zakupów dla grup wiekowych",
    x = "Grupy wiekowe",
    y = "Kwota zakupu"
  )
# kategorie
ggplot(data, aes(x = Category, fill = Category)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Liczba zakupów w danej kategorii",
    x = "Kategoria",
    y = "Liczba zakupów"
  ) +
  theme_minimal()


#podział na płeć
data_w <- data %>%
  filter(Gender=="Female")
data_m <- data %>%
  filter(Gender=="Male")
#najczęściej wybierane kolory przez mężczyzn - bez grupowania
color_summary_m1 <- data_base %>%
  group_by(Gender, Color) %>%
  filter(Gender=="Male") %>% 
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

ggplot(color_summary_m1, aes(x = reorder(Color, count), y = count, )) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Kolory najczęściej wybierane przez mężczyzn",
    x = "Kolor",
    y = "Liczba"
  ) +
  theme_minimal()

#najczęściej wybierane kolory przez kobiety - bez grupowania
color_summary_w1 <- data_base %>%
  group_by(Gender, Color) %>%
  filter(Gender=="Female") %>% 
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

ggplot(color_summary_w1, aes(x = reorder(Color, count), y = count, )) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Kolory najczęściej wybierane przez kobiety",
    x = "Kolor",
    y = "Liczba"
  ) +
  theme_minimal()

#trendy w kolorach u mężczyzn
color_summary_m <- data_m %>%
  group_by(Gender, Color) %>%
  summarise(count = n(), .groups = "drop")

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

#trendy w kolorach u kobiet
color_summary_w <- data_w %>%
  group_by(Gender, Color) %>%
  summarise(count = n(), .groups = "drop")

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

# rozkład zakupów w porach roku
ggplot(data, aes(x = Season, fill = Season)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Liczba zakupów w różnych porach roku",
    x = "Pora roku",
    y = "Liczba zakupów"
  ) +
  theme_minimal()+
  scale_fill_manual(values = c("#FF6666", "#66CC66", "#FFCC00", "#0099CC" ))

popular_items <- data_base %>%
  group_by(Season, Item.Purchased) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))%>%
  head(15)

# Wizualizacja: Popularność przedmiotów w sezonach
ggplot(popular_items, aes(x = Season, y = Count, fill = Item.Purchased)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Popularność przedmiotów w poszczególnych sezonach",
       x = "Sezon", y = "Częstość sprzedaży") +
  theme_minimal()

#rozkład ocen
ggplot(data_base, aes(x = Review.Rating)) +
  geom_histogram(bins = 26, fill = "#009919", color = "black") +
  labs(
    title = "Rozkład ocen",
    x = "Ocena",
    y = "Liczba ocen"
  ) +
  theme_minimal()

#Średnia kwota zakupów z rabatem
srednia_kwota <- data %>%
  group_by(Discount.Applied) %>%
  summarise(Mean_Purchase_Amount = mean(Purchase.Amount..USD., na.rm = TRUE))

ggplot(srednia_kwota, aes(x = Discount.Applied, y = Mean_Purchase_Amount, fill = Discount.Applied)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Mean_Purchase_Amount, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Średnia kwota zakupów z rabatem i bez",
    x = "Rabat",
    y = "Średnia kwota zakupu"
  ) +
  theme_minimal()

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
fviz_mca_biplot(wynikMCA1, label = "all", 
                col.var = "blue", col.ind = "red", 
                title = "MCA Biplot")
fviz_mca_var(wynikMCA1, 
             col.var = "red", 
             title = "MCA - Variables")

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
                title = "MCA Biplot") 
fviz_mca_var(wynikMCA2, 
             col.var = "red", 
             title = "MCA - Variables")

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
                title = "MCA Biplot") 
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


MCA_final <- data %>%
  select(Gender, Discount.Applied, Spending, AgeGroup, Payment.Method,Frequency.of.Purchases.N, Previous.Purchases.N)

wynikMCAfinal <- MCA(MCA_final)
get_eigenvalue(wynikMCAfinal)
fviz_screeplot(wynikMCAfinal, addlabels = TRUE, ylim = c(0, 45))
fviz_mca_var(wynikMCAfinal, choice = "mca.cor", 
             repel = TRUE, 
             ggtheme = theme_minimal())
fviz_mca_biplot(wynikMCAfinal, label = "var", repel = TRUE,
                col.var = "blue", col.ind = "red", 
                title = "MCA Biplot")
fviz_mca_var(wynikMCAfinal, 
             repel = TRUE, 
             ggtheme = theme_minimal())

fviz_ellipses(wynikMCAfinal, c("Discount.Applied", "Gender"), repel = TRUE,
              geom = "point")
res.desc <- dimdesc(wynikMCAfinal, axes = c(1,2))
res.desc[[1]]
res.desc[[2]]

fviz_mca_ind(wynikMCAfinal, 
             label = "none",
             habillage = "Spending", 
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 
fviz_mca_ind(wynikMCAfinal, 
             label = "none", 
             habillage = "Payment.Method", 
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 