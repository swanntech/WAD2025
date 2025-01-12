#📅📅📅wstępna analiza
library(tidyverse)
library(moments)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)

data <- read.csv("C:/Users/MSI/Desktop/shopping_trends.csv")
data_w <- data %>%
  filter(Gender=="Female")
data_m <- data %>%
  filter(Gender=="Male")

summary(data)
summary(data_m)
summary(data_w)
stats <- describe(data)

#---------------------------------------------------------------------------------------------------------------------------------------
# UWAGI ❗
#Baza zawiera dane o 3900 klientach
#Wszyscy znajdują się w wieku 18-70
#Znacząco więcej mężczyzn niż kobiet, 2652 do 1248
#Średni wydatek na zakup: $60
#Ceny kupowanych produktów w zakresie $20-$100
#Oceny produktów w skali 1-5, z zebranych danych oceniane od 2.5 do 5.0
#Osób z subskrypcją: 1053

#---------------------------------------------------------------------------------------------------------------------------------------

# ANALIZA POJEDYNCZYCH ZMIENNYCH
#==============================================
#🔴🟠🟡🟢🔵🟣🟤⚫⚪🟥🟧🟨🟩🟦🟪🟫⬛⬜🔶🔷 to potem usune xd

#🔵Status subskrypcji
data_s <- data %>%
  filter(Subscription.Status=="Yes")
data_ns <- data %>%
  filter(Subscription.Status=="No")

#🔵Metody płatności:
    #wybrana metoda płatności
data_p <- data %>% 
  group_by(Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()
    #preferowana metoda płatności
data_p1 <- data %>% 
  group_by(Preferred.Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()
    #wybrana vs preferowana metoda płatności
data_p2 <- data %>% 
  group_by(Payment.Method,Preferred.Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#🔵Metody dostawy: 
data_d <- data %>% 
  group_by(Shipping.Type) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#🔵Częstotliwość zakupów:
data_f <- data %>% 
  group_by(Frequency.of.Purchases) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#🔵Wcześniejszych zakupów:

data_prev <- data %>% 
  group_by(Previous.Purchases) %>% 
  summarise(total_count=n()) %>%
  ungroup() 
    #⚫Od 1 do 50

#🔵Kategorie produktu:
data_c <- data %>% 
  group_by(Category,Gender) %>% 
  summarise(total_count=n()) %>%
  ungroup()


data1 <- data %>% 
  group_by(Age,Gender) %>% 
  summarise(suma_USD=sum(Purchase.Amount..USD.)) %>%
  ungroup()
#co ciekawe najwięcej produktów we wszystkich kategoriach kupują mężczyźni
data1 %>% 
  ggplot(aes(x=Age, y=suma_USD, colour=Gender))+
  geom_point()+
  geom_smooth(method="lm")

# to samo wychodzi tu: w pierwszej 50-tce klientów, którzy wydali najwięcej nie ma ani jednej kobiety
# pierwsza kobieta pojawia się dopiero na 52 miejscu

data2 <- data %>% 
  group_by(Age,Gender) %>% 
  summarise(suma_poprzednie=sum(Previous.Purchases)) %>%
  ungroup()

data2 %>% 
  ggplot(aes(x=Age, y=suma_poprzednie, colour=Gender))+
  geom_point()+
  geom_smooth(method="lm")


#Przykładowe pytania które mogą nas interesować
#1. Jak można porównać to, ile wydają klienci w stosunku do częstotliwości ich zakupów?
#2. od czego może zależeć metoda płatności? cena? częstotliwość zakupu?
#3. Kto/dlaczego decyduje się na subskrybcję?
#4. Jak wygląda rynek dla poszczególnych kategorii?
#5. Czy osoby w różnym wieku różnie dokonują zakupów? A może metod płatności?
#6 Pominęłabym w tym wszystkim Lokalizację, bo raczej średnio nam to wpłynie na cokolwiek 
#+ nie bardzo raczej znamy się na różnicach gospodarczych, ekonomicznych i społecznych w regionach Ameryki xd 
#7. Raczej też mało istotny wydaje mi się rozmiar tutaj
#8. Ciekawe może być porównanie dominujących kolorów dla danych grup, albo/i w poszczególnych porach roku
#9. Plus to w jakim okresie kto więcej wydaje? może z okazji jakichś świąt? Może na walentynki?

 #Pora roku a ilość dokonanych zakupów
    data_season1 <- data %>% 
      group_by(Season) %>%  
      summarise(total_count = n()) %>%  
      ungroup() 
                                  #najwięcej zakupów klienci robią: wiosną

#Pora roku a ilość wydanych przez klientów pieniędzy
    data_season2 <- data %>% 
      group_by(Season) %>%  
      summarise(total_spent = sum(Purchase.Amount..USD.)) %>%  
      ungroup() 
                                  #najwięcej klienci wydają jesienią


#wykresy 'wiek klientów wg metod płatności'

    #wykres boxplot
ggplot(data, aes(x = Payment.Method, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Wiek klientów wg metody płatności",
       x = "Metoda płatności",
       y = "Wiek") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

    #Wykres słupkowy
ggplot(data, aes(x = Payment.Method, fill = cut(Age, breaks = c(0, 20, 30, 40, 50, 60, 70)))) +
  geom_bar(width = 0.6) +
  labs(title = "Liczba klientów wg metody płatności i przedziału wiekowego",
       x = "Metoda płatności",
       y = "Liczba klientów",
       fill = "Przedział wiekowy") +
  theme_void() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))


  #wykres 'oceny zakupów a status subskrypcji'

ggplot(data, aes(x = Subscription.Status, y = Review.Rating, fill = Subscription.Status)) +
  geom_boxplot() +
  labs(title = "Ocena zakupów a statusu subskrypcji",
       x = "Status subskrypcji",
       y = "Ocena zakupów (1-5)") +
  theme_minimal() +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62"))
    #średnio subskrybenci wystawiają wyższe oceny.


#najczęściej wybierane kolory przez mężczyzn
color_summary_m <- data_m %>%
  group_by(Gender, Color) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

ggplot(color_summary_m, aes(x = reorder(Color, count), y = count, )) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Kolory najczęściej wybierane przez mężczyzn",
    x = "Kolor",
    y = "Liczba"
  ) +
  theme_minimal()

#najczęściej wybierane kolory przez mężczyzn
color_summary_w <- data_w %>%
  group_by(Gender, Color) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

ggplot(color_summary_w, aes(x = reorder(Color, count), y = count, )) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Kolory najczęściej wybierane przez kobiety",
    x = "Kolor",
    y = "Liczba"
  ) +
  theme_minimal()

#Analiza skupień
numeric_data <- data %>% select(where(is.numeric))
scaled_data <- scale(numeric_data)
view(numeric_data)
numeric_data <- numeric_data[, !colnames(numeric_data) %in% "Customer.ID"]
view(scaled_data)

pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
pca_data <- pca_result$x[, 1:2]
kmeans_result <- kmeans(pca_data, centers = 3, nstart = 25)
fviz_cluster(kmeans_result, data = pca_data)

#inna naliza skupień (wyszedł znaczek windowsa)
fit <- kmeans(scaled_data, 5)
library(cluster)

clusplot(scaled_data, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
library(fpc)
plotcluster(scaled_data, fit$cluster)

#kolory
library(dplyr)

color_mapping <- c(
  "Indigo" = "blue",
  "Blue" = "blue",
  "Cyan" = "blue",
  "Teal" = "blue",
  "Turquoise" = "blue",
  "Purple" = "purple",
  "Violet" = "purple",
  "Lavender" = "purple",
  "Brown"="brown",
  "Beige"="brown",
  "White"="white",
  "Red" = "red",        
  "Maroon" = "red",
  "Yellow" = "yellow",
  "Orange" = "yellow",
  "Green" = "green",
  "Olive" = "green",
  "Pink" = "pink",
  "Magenta" = "pink",
  "Peach" = "pink",
  "Gray" = "gray",
  "Black" = "gray",
  "Charcoal" = "gray",
  "Silver"= "silver",
  "Gold"="gold"
)

data_col <- data %>%
  mutate(Grouped_Color = recode(Color, !!!color_mapping))

test_c <- data_col%>%
  group_by(Season, Grouped_Color) %>% 
  summarise(total_count=n()) %>%
  ungroup()
test_c %>% 
  ggplot(aes(Season, total_count, fill=Grouped_Color)) +
  stat_summary(geom="bar", position="dodge") +
  xlab("") + ylab("Amount")

# Analiza połączeń sezon-kolor-produkt

# Wybór interesujących kolumn
selected_data <- data %>%
  select(Season, Color, Item.Purchased)

# Zliczanie częstości sprzedaży dla kombinacji
freq_table <- selected_data %>%
  group_by(Season, Color, Item.Purchased) %>%
  summarise(Count = n(), .groups = "drop")
top_combinations <- freq_table %>% 
  arrange(desc(Count)) %>% 
  head(20) # Top 20 najczęstszych kombinacji
print(top_combinations)

# Analiza najpopularniejszych kolorów w sezonach
popular_colors <- selected_data %>%
  group_by(Season, Color) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  head(15)
print(popular_colors)

# Analiza najczęściej sprzedawanych przedmiotów w sezonach
popular_items <- selected_data %>%
  group_by(Season, Item.Purchased) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))%>%
  head(15)
print(popular_items)

# Wizualizacja: Najpopularniejsze kombinacje
ggplot(top_combinations, aes(x = reorder(Color, -Count), y = Count, fill = Season)) +
  geom_bar(stat = "identity", position = "nudge") +
  scale_fill_manual(values = color_mapping) +
  labs(title = "Najczęstsze kombinacje kolorów w sezonach",
       x = "Kolor", y = "Częstość sprzedaży") +
  theme_minimal()

# Wizualizacja: Popularność kolorów w sezonach
ggplot(popular_colors, aes(x = Season, y = Count, fill = Color)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_mapping) +
  labs(title = "Popularność kolorów w poszczególnych sezonach",
       x = "Sezon", y = "Częstość sprzedaży") +
  theme_minimal()

# Wizualizacja: Popularność przedmiotów w sezonach
ggplot(popular_items, aes(x = Season, y = Count, fill = Item.Purchased)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Popularność przedmiotów w poszczególnych sezonach",
       x = "Sezon", y = "Częstość sprzedaży") +
  theme_minimal()

color_mapping <- c(
  "Red" = "red", 
  "Blue" = "blue", 
  "Green" = "green", 
  "Yellow" = "yellow", 
  "Pink" = "pink", 
  "Black" = "black",
  "White" = "white",
  "Gray" = "gray",
  "Purple" = "purple",
  "Orange" = "orange",
  "Brown" = "brown",
  "Cyan" = "#00FFFF",
  "Charcoal" = "#36454F",
  "Peach" = "#FFD3AC",
  "Violet" = "#EE82EE",
  "Teal" = "#008080",
  "Silver" = "#E0E0E0",
  "Maroon" = "#800000",
  "Olive" = "#808000",
  "Turquoise" = "#40E0d0",
  "Gold" = "#d4af37",
  "Magenta" = "#ff00ff",
  "Lavender" = "#d3d3ff",
  "Indigo" = "#560591"
)
