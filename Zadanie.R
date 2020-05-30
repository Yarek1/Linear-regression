## Regresja liniowa, Jaroslaw Zadak oraz Wojciech Wolski
## ZESTAW 11
#=========
# Literatura:  Spaeth H. (1991), Mathematical Algorithms for Linear Regression, Academic Press, 1991
#Kolumna 1 (X):  wskaznik spozycia win i wermutow na osobe (l/rocznie) wsrod doroslej populacji w danym stanie 
#Kolumna 2 (Y):  wskaznik smiertelnosci (na 100 tys. osob populacji) z powodu marskosci watroby w danym stanie.
#Dane z wybranych stanow USA. 
library(moments)
library(tseries)

x<- c(5,4,3,7,11,9,7.5,3,12,14,12,10,13,9.5,7,18,6.5,31,14,19)
y<- c(41.2,31.7,39.8,57.5,74.8,62.8,54.3,47.9,77.2,82.6,34.8,53.1,55.8,62.8,67.3,56.7,39.6,129.9,72.3,83.6)

# x- zmienna objasniajaca, y- zmienna objasniana
length(x)
length(y)
plot(x,y, main = "Wykres wizualizacji", xlab = "wskaznik spozycia win (l/rok)", ylab = "wskaznik smiertelnosci (na 100tys.osob)",pch = 19, frame = TRUE)

#Statystyki opisowe zmienna objasniajaca:
# Srednia
m1<-mean(x)
#Wariancja
var(x)=
# Odchylenie standardowe
sd(x)
# Skosnosc
skewness(x)
# Kurtoza
kurtosis(x)
#Podsumowanie
summary(x)

#Statystyki opisowe zmienna objasnianej:
# Srednia
m2<-mean(y)
#Wariancja
var(y)
# Odchylenie standardowe
sd(y)
# Skosnosc
skewness(y)
# Kurtoza
kurtosis(y)
#Podsumowanie
summary(y)

# Badamy korelacje pomiedzy zmiennymi
cor(x,y)
# Korelacja pomiedzy zmiennymi wynosi 0.82 stad wniosek ze zmienne sa silnie powiazane ze soba, powyzszy wykres przedstawia liniowa zaleznosc wraz z tendencja wzrostow¹.

#2. Rozwazamy model regresji liniowej w postaci:

y=b0+b1*x+e

# Obliczamy wspolczynniki za pomoca wzorow podanych na wykladzie.

b1=(mean(x*y)-m1*m2)/var(x)

b0=m2-b1*m1

yi=b0+b1*x

# Nastepnie obliczamy reszty e i przedstawiamy ich zachowanie na wykresie

e=y-yi

plot(c(1:20),e,xlab = "Index reszty", ylab = "Wartosc reszty")
abline(0,0,col="blue")

#Zachowanie reszt jest prowidlowe, choc maja one duze odchylenia

# Wykres regresji liniowej

plot(x,y, main = "Regresja Liniowa", xlab = "wskaznik spozycia win (l/rok)", ylab = "wskaznik smiertelnosci (na 100tys. osob)",col="red",pch = 19, frame = TRUE)
abline(b0,b1,col="blue")
# Niebieska linia przedstawia prosta regresji liniowej dla naszych danych

# 3. Szacowanie wariancji skladnika losowego-MSE
mse=sum(e^2)/(length(x)-2)
mse
# Wariancja ma duza wartosc co ma wplyw na oszacowanie wspolczynnikow modelu regresi liniowej
# Obliczamy wspolczynniki macierzy kowariancji.

wb0=mse*(1/20+m1^2/var(x))
cb0b1=-(mse)*mean(x)/var(x)
wb1=mse/var(x)

# Estymator macierzy kowariancji

mc=matrix(c(wb0,cb0b1,cb0b1,wb1),2,2)
mc

# Nastepnie obliczamy bledy standardowe

SEb0=sqrt(wb0)
SEb0
SEb1=sqrt(wb1)
SEb1

#4. Testowanie hipotezy o istotnosci parametrow regresji
# H0: Bi=0 wobec alternatywy dwustronnej
# H1: Bi=/=0. Obliczamy statystyki testowe

T0=b0/SEb0
T1=b1/SEb1
T0
T1
# wartosc krytyczna t (z rozkladu studenta) dla alfa rownego 0.05 i 18 stopniach swobody

t=qt(0.975, df=18)
t
# Nie mamy podstaw do odrzucenia hipotezy H0 na poziomie istotnosci 0.05 z powodu wartosci statystyk T0 oraz T1 w stosunku do wartosci krytycznej t (|T0|<t,|T1|<t)
# Wystepuje male zaszumienie estymatora.

#5. Statystyczna analiza reszt na poziomie istotnosci 0.05

qqnorm(e,pch=20);qqline(e,col="red")

# Jest to rozklad podobny do normalnego, ale ma odchylenia na wartosciach skrajnych

lillie.test(e)
# p-value wynosi okolo 0.51, zatem akceptujemy hipoteze ze proba pochodzi z rozkladu normalnego

jarque.bera.test(e)
# p-value wynosi okolo 0.33, zatem akceptujemy hipoteze ze proba pochodzi z rozkladu normalnego

shapiro.test(e)
# p-value wynosi okolo 0.13, zatem akceptujemy hipoteze ze proba pochodzi z rozkladu normalnego

rmarkdown::render("Zadanie.R", "pdf_document")
