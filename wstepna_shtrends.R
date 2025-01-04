#wstępna analiza
library(tidyverse)
library(moments)
library(Hmisc)

data <- read.csv("C:/Users/MSI/Desktop/shopping_trends.csv")
data_w <- data %>%
  filter(Gender=="Female")
data_m <- data %>%
  filter(Gender=="Male")

summary(data)
summary(data_m)
summary(data_w)
stats <- describe(data)

#Baza zawiera dane o 3900 klientach
#Wszyscy znajdują się w wieku 18-70
#Znacząco więcej mężczyzn niż kobiet, 2652 do 1248
#Średni wydatek na zakup: $60
#Ceny kupowanych produktów w zakresie $20-$100
#Oceny produktów w skali 1-5, z zebranych danych oceniane od 2.5 do 5.0
#Osób z subskrypcją: 1053


data_s <- data %>%
  filter(Subscription.Status=="Yes")
data_ns <- data %>%
  filter(Subscription.Status=="No")

#Metody płatności:
data_p <- data %>% 
  group_by(Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()
data_p1 <- data %>% 
  group_by(Preferred.Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()
data_p2 <- data %>% 
  group_by(Payment.Method,Preferred.Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#Metody dostawy: 
data_d <- data %>% 
  group_by(Shipping.Type) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#Częstotliwość zakupów:
data_f <- data %>% 
  group_by(Frequency.of.Purchases) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#Wcześniejszych zakupów:
#Od 1 do 50
data_prev <- data %>% 
  group_by(Previous.Purchases) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#Kategorie produktu:
data_c <- data %>% 
  group_by(Category,Gender) %>% 
  summarise(total_count=n()) %>%
  ungroup()


data1 <- data %>% 
  group_by(Age,Gender) %>% 
  summarise(suma_USD=sum(Purchase.Amount..USD.)) %>%
  ungroup()
data1 %>% 
  ggplot(aes(x=Age, y=suma_USD, colour=Gender))+
  geom_point()+
  geom_smooth(method="lm")

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

