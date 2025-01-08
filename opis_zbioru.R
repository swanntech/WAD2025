data = read.csv("C:/Users/48695/Desktop/sem_iii/wad/wad_zajecia/zaliczenie/shopping_trends.csv")

library(tidyverse)
library(ggplot2)
library(dplyr)

summary(data)

# rozkład wieku
ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 53, fill = "#009999", color = "black") +
  geom_density(aes(y = ..count..), color = "#660099", size = 1) +
  labs(
    title = "Rozkład wieku klientów",
    x = "Wiek",
    y = "Liczba klientów"
  ) +
  theme_minimal()

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


# rozkład płci
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Liczba klientów według płci",
    x = "Płeć",
    y = "Liczba klientów"
  ) +
  theme_minimal()

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


# rozkład kolorów - Ania

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

#rozkład ocen
ggplot(data, aes(x = Review.Rating)) +
  geom_histogram(bins = 26, fill = "#009919", color = "black") +
  labs(
    title = "Rozkład ocen",
    x = "Ocena",
    y = "Liczba ocen"
  ) +
  theme_minimal()

# częstośc zakupów
data$Frequency.of.Purchases <- factor(
  data$Frequency.of.Purchases,
  levels = c("Bi-Weekly","Weekly", "Fortnightly", "Monthly","Every 3 Months","Quarterly", "Annually")
)

ggplot(data, aes(x = Frequency.of.Purchases, fill = Frequency.of.Purchases)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Częstość zakupów",
    x = "Częstotliwość zakupów",
    y = "Liczba zakupów"
  ) +
  theme_minimal()

# age vs amount
ggplot(data, aes(x=Age, y=Purchase.Amount..USD.)) + 
  geom_point(
    fil=  "#666600",
    color = "#555500"
  )+
  labs(
    title = "Wydawana kwota a wiek",
    x = "Wiek",
    y = "Wydana kwota"
    
  )

#boxbox

data$Age.Group <- cut(
  data$Age,
  breaks = c(18, 29, 39, 49, 59, 70),
  labels = c("18-29", "30-39", "40-49", "50-59", "60-70"),
  include.lowest = TRUE
)

# Tworzenie boxplotu
library(ggplot2)

ggplot(data, aes(x = Age.Group, y = Purchase.Amount..USD., fill = Age.Group)) +
  geom_boxplot() +
  labs(
    title = "Rozkład kwoty zakupu dla grup wiekowych",
    x = "Grupy wiekowe",
    y = "Kwota zakupu"
  ) +
  theme_minimal()

# wpływ zniżki na średnią kwotę zakupów
# ile zniżek ile bez zniżek?
#Może to oznaczać, że zniżki nie są wystarczająco atrakcyjne, aby zwiększać wydatki.
#Może być sygnałem do przeanalizowania strategii rabatowej. Zniżki powinny motywować klientów do zwiększania wydatków, a nie jedynie zmniejszać marżę.
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

# Czy liczba transakcji wzrasta z rabatami? Nie wiadomo, nie możemy śledzić poszczególnych klientów
# Jak rozkładają się kwoty w obu grupach? głupie pytanie bo bez zniżek było po prostu więcej zakupów
# można zostawić wykres i zapytać grupe czm to jest bez sensu? pytanie interaktywne

discount_yes <- data$Purchase.Amount[data$Discount == "Yes"]
discount_no <- data$Purchase.Amount[data$Discount == "No"]

# Ustawienie przezroczystości kolorów za pomocą rgb
color_yes <- rgb(0.2, 0.4, 0.6, 0.5) # Niebieski, 50% przezroczystości
color_no <- rgb(0.8, 0.2, 0.2, 0.5)  # Czerwony, 50% przezroczystości

# Tworzenie histogramów
hist_yes <- hist(discount_yes, breaks = 20, plot = FALSE)  # Histogram dla zniżek
hist_no <- hist(discount_no, breaks = 20, plot = FALSE)    # Histogram bez zniżek

# Rysowanie histogramów na jednym wykresie
plot(hist_yes, col = color_yes, xlim = c(0, max(data$Purchase.Amount, na.rm = TRUE)), 
     main = "Porównanie rozkładu kwot zakupów", xlab = "Kwota zakupu", ylab = "Częstość", border = "black")
plot(hist_no, col = color_no, add = TRUE, border = "black")

# Dodanie legendy
legend("bottomleft", legend = c("Z rabatem", "Bez rabatu"), 
       fill = c(color_yes, color_no), border = "black")

#wplyw kodu promocyjnego na średnią kwotę zakupów
srednia_kwota2 <- data %>%
  group_by(Promo.Code.Used) %>%
  summarise(Mean_Purchase_Amount = mean(Purchase.Amount..USD., na.rm = TRUE))

ggplot(srednia_kwota2, aes(x = Promo.Code.Used, y = Mean_Purchase_Amount, fill = Promo.Code.Used)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Mean_Purchase_Amount, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Średnia kwota zakupów z kodem rabatowym i bez",
    x = "Kod rabatowy",
    y = "Średnia kwota zakupu"
  ) +
  theme_minimal()
