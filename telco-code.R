# Librerie necessarie
library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)
library(psych)
library(PerformanceAnalytics)
library(corrplot)
library(cowplot)
library(vcd)      # Cramer V Test
library(qcc)      # Diagramma di Pareto
library(car)      # Freedman Diaconis
library(scales)
library(GGally)
library(caret)
library(ROSE)
library(blorr)
library(DescTools)
library(iml)
library(lmtest)

# Percorso dei file
path <- "C:/Users/crist/OneDrive/Desktop/UNI CRISTIAN/Tesi/"

# Importazione dei dataset
train <- read_csv(paste0(path, "train.csv"))
test <- read_csv(paste0(path, "test.csv"))
sample_submission <- read_csv(paste0(path, "sampleSubmission.csv"))

# Tipo delle colonne
sapply(train, class)

# Pulizia e trasformazione dei dati
train <- train %>%
  mutate(
    international_plan = as.factor(ifelse(international_plan == "yes", 1, 0)),
    voice_mail_plan = as.factor(ifelse(voice_mail_plan == "yes", 1, 0)),
    churn = as.factor(ifelse(churn == "yes", 1, 0)),
    n_weeks = account_length,
    total_calls = total_day_calls + total_eve_calls + total_night_calls,
    total_minutes = total_day_minutes + total_eve_minutes + total_night_minutes,
    total_charge = total_day_charge + total_eve_charge + total_night_charge
  ) %>%
  select(-area_code, -account_length)

#----------------------------------------------------------------------------------------------------------------------------------

# Analisi variabili categoriali
# Focalizzo l'attenzione sulla variabile di interesse CHURN
table(train$churn) / nrow(train)
ggplot(train, aes(x = churn)) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = churn)) +
  ggtitle("Tasso di abbandono [%]") +
  labs(x = "Churn", y = "Frequenza Percentuale") +
  scale_y_continuous(labels = label_percent(accuracy = .1)) +
  geom_text(aes(label = percent((..count..) / sum(..count..)), y = ..prop..), stat= "count", vjust = 40)
# Il 14.1% dei clienti ha effettuato l'abbandono (3652 vs 598) -> classi sbilanciate, problema di cui si terrà conto in fase di costruzione del modello

table(train$international_plan) / nrow(train)
ggplot(train, aes(x = international_plan)) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = international_plan)) +
  ggtitle("Piano tariffario internazionale") +
  labs(x = "International Plan", y = "Frequenza Percentuale") +
  scale_y_continuous(labels = label_percent(accuracy = .1)) +
  geom_text(aes(label = percent((..count..) / sum(..count..)), y = ..prop..), stat = "count", vjust = 40)
# Il 90.7% dei clienti NON possiede un piano tariffario internazionale (3854 vs 396).
# Sarebbe dunque interessante per l'azienda cercare di capire come mai questo piano tariffario non è propriamente in voga.

table(train$voice_mail_plan) / nrow(train)
ggplot(train, aes(x = fct_infreq(voice_mail_plan))) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = voice_mail_plan)) +
  ggtitle("Piano tariffario messaggi vocali") +
  labs(x = "Voice mail Plan", y = "Frequenza Percentuale") +
  scale_y_continuous(labels = label_percent(accuracy = .1)) +
  geom_text(aes(label = percent((..count..) / sum(..count..)), y = ..prop..), stat = "count", vjust = 40)
# Il 73.9% delle persone NON hanno un piano tariffario con messaggi vocali (3138 vs 1112).
# Piano tariffario che copre un maggior numero di persone, tuttavia anch'esso non molto diffuso

# Concentriamoci ora sull'area geografica che l'azienda copre
ggplot(train, aes(y = fct_rev(fct_infreq(state)))) +
  geom_bar(aes(x = (..count..) / sum(..count..), fill = state)) +
  ggtitle("Presenza [%] Stati") +
  labs(y = "State", x = "Frequenza Percentuale") +
  scale_x_continuous(labels = label_percent(accuracy = .1))
sort(table(train$state) / nrow(train) * 100, decreasing = TRUE)
# Sono presenti tutti e 51 gli stati americani. Nella top 5 dei paesi più presenti troviamo West-Virginia, Minnesota, Idaho, Alabama e Virginia.

# Sarebbe interessante capire, in modo del tutto generale e preliminare, le probabili cause di abbandono da parte del customer.

# ♦-----------------------------------------------------------------------------------------

# Interazione tra variabili categoriali (tabelle di contingenza)
t1 <- table(train$churn, train$international_plan, dnn = c("Churn", "Int-Plan"))
ggplot(train, aes(fill = international_plan, x = churn)) +
  geom_bar(position = "fill") +
  ggtitle("Abbandoni vs Piano Internazionale") +
  labs(x = "Churn", y = "Frequenza Percentuale")
prop.table(t1, margin = 1)
# Si nota che il 28% di coloro che hanno abbandonato avevano questo piano attivo.

# Effettuo il test chi quadro per rilevare se c'è grado di associazione tra le due variabili:
assocstats(t1)
# p-value inferiore ad alpha = 0.01 quindi rifiuto Ho, dunque churn e int-plan non sono indipendenti.
# in particolare il grado di associazione (phi) è pari a 0.26, il che indica una bassa correlazione.

ggplot(train, aes(fill = churn, x = international_plan)) +
  geom_bar(position = "fill") +
  ggtitle("Abbandoni vs Piano Internazionale") +
  labs(x = "International Plan", y = "Frequenza Percentuale")
prop.table(t1, margin = 2)
# Dato importante da evidenziare:
# - Il 42.4% di coloro che hanno il piano internazionale sono anche churned! Tasso molto elevato!
# - Da effettuare ulteriori approfondimenti

ggplot(train, aes(fill = voice_mail_plan, x = churn)) +
  geom_bar(position = "fill") +
  ggtitle("Abbandoni vs Piano Messaggi Vocali") +
  labs(x = "VM-Plan", y = "Frequenza Percentuale")
prop.table(table(train$churn, train$voice_mail_plan, dnn = c("Churn", "VM-Plan")), margin = 1)
# Tra quelli che hanno abbandonato: quasi il 14% aveva un piano di messaggi vocali attivo

t2 <- table(train$churn, train$voice_mail_plan, dnn = c("Churn", "VM-Plan"))
ggplot(train, aes(x = voice_mail_plan, fill = churn)) +
  geom_bar(position = "fill") +
  ggtitle("Abbandoni vs Piano tariffario messaggi vocali") +
  labs(x = "Voice mail Plan", y = "Frequenza Percentuale")
prop.table(t2, margin = 2)
# Il 7.3% di coloro che possiedono il VM_plan sono churners. Tasso di abbandono molto più rassicurante.
# Potrebbe esserci dunque qualche problema inerente al piano internazionale
assocstats(t2)
# Anche queste sembrano presentare un certo legame che, tuttavia risulta essere molto basso (0.11)

# CUSTOMER CHE HANNO ENTRAMBI I PIANI TARIFFARI: il 2.4% dei clienti ha entrambi i piani tariffari attivi.
prop.table(table(train$voice_mail_plan, train$international_plan, dnn = c("VM-Plan", "Int-Plan")))
t3 <- table(train$voice_mail_plan, train$international_plan)
assocstats(t3)
# Non rifiuto Ho, tra vm-plan e int plan non risulta esserci un'associazione.

# Vediamo in quale stato il tasso di abbandono risulta essere più elevato (ovviamente tendendo conto della numerosità)
sort(table(train$state, train$churn)[, 2] / table(train$state))
t4 <- table(train$state, train$churn)
assocstats(t4)
# Nella top 5 degli stati con churn rate più alto troviamo:
# - New Jersey, California, Washington, Maryland, Montana (con churn rate compreso tra il 20 e il 27%)
# Nelle posizioni più basse troviamo Virgina e le Hawaii con un churn rate al di sotto del 6%.
# Un obiettivo per l'azienda potrebbe essere ad esempio quello di portare tutti gli stati al di sotto del 20% di churn rate
# State e churn risultano avere un certo grado di associazione seppur debole (0.14)

#---------------------------------------------------------------

# Analisi Variabili numeriche 
# Panoramica generale di tutte le variabili numeriche 
describe(train[-c(1, 3, 4, 5, 20)])
summary(train[-c(1, 3, 4, 5, 20)])

# n_weeks
# In media un individuo resta cliente della stessa azienda per 100 mesi circa (~ 9 anni).
# Guardando i dati sembra che si possa suddivedere la clientela in 3 fasce differenti:
# Il 25% dei customer sono racchiusi nel range che va da 1 a 73; il 50% tra 73 e 127; e il restante 25% che arriva a 243 mesi
# Tuttavia in quest'ultima fascia il range è abbastanza elevato quindi potrebbe esserci dei valori anomali che non verranno rimossi
# La distribuzione di questa variabile presenta una leggera asimmetria positiva. Presenta una forma platicurtica quindi poco appuntita

w1 <- ggplot(train, aes(x = n_weeks)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef") +
  labs(x = "N_weeks", y = "Frequenza Assoluta") +
  geom_vline(xintercept = mean(train$n_weeks), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train$n_weeks, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione settimane di permanenza")

w2 <- ggplot(train, aes(y = n_weeks)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Settimane di permanenza del cliente", y = "N_weeks")

plot_grid(w1, w2)

ggplot(train, aes(x = n_weeks, fill = churn)) + 
  geom_histogram(binwidth = 10) + 
  labs(title = "Distribuzione N_weeks per Churn")

assocstats(table(train$churn, train$n_weeks))

# TOTAL_CALLS
# Il cliente effettua in media 300 chiamate giornaliere.
# La distribuzione è pressocchè simmetrica (asimmetria negativa quasi irrilevante) ed ha una forma platicurtica.
# Possiamo notare che la forma della distribuzione risulta quindi essere poco appuntita e maggiormente uniforme 

w3 <- ggplot(train, aes(x = total_calls)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train$total_calls), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train$total_calls, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione chiamate giornaliere")

boxplot(train$total_calls, main = "Chiamate giornaliere effettuate")

t096 <- table(train$total_calls, train$churn)
assocstats(t096)

# TOTAL_CALLS_QUOTIDIANE
train2 <- train %>%
  mutate(total_calls = total_calls / 30,
         total_minutes = total_minutes / 30)

summary(train2$total_calls)
ggplot(train2, aes(x = total_calls)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train2$total_calls), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train2$total_calls, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione chiamate giornaliere (Quotidiane)")

boxplot(train2$total_calls, main = "Chiamate giornaliere effettuate (Quotidiane)")

# TOTAL_MINUTES
# In media si trascorrono 581 minuti in telefonata mensilmente. Sembrano essere presenti potenziali outliers.
# Leggera asimmetria negativa con curtosi prossima alla 0, mesocurtica. Bisogna capire quanto gli outliers incidono su questo fattore.

ggplot(train, aes(x = total_minutes)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train$total_minutes), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train$total_minutes, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione totale minuti trascorsi in chiamata")

boxplot(train$total_minutes, main = "Minuti giornalieri trascorsi in chiamata")

ggplot(train, aes(x = total_minutes, fill = churn)) + 
  geom_histogram(binwidth = 10) + 
  labs(title = "Distribuzione totale minuti per Churn")

t099 <- table(train$total_minutes, train$churn)
assocstats(t099)

# Total_minutes_QUOTIDIANI
ggplot(train2, aes(x = total_minutes)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train2$total_minutes), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train2$total_minutes, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione totale minuti giornalieri trascorsi in chiamata")

boxplot(train2$total_minutes, main = "Minuti giornalieri trascorsi in chiamata")
summary(train2$total_minutes)

# TOTAL_CHARGE
# In media vengono effettuate circa 57$ di ricarica in un certo intervallo di tempo. La distribuzione risulta essere simmetrica
# Potrebbero esserci outliers che vanno ad influenzare.

ggplot(train, aes(x = total_charge)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train$total_charge), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train$total_charge, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione addebiti mensili")

boxplot(train$total_charge, main = "Carica totale giornaliera")

ggplot(train, aes(x = total_charge, fill = churn)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribuzione addebiti mensili per Churn")

ggplot(train, aes(x = total_intl_charge, fill = churn)) + 
  geom_histogram(binwidth = 2) + 
  labs(title = "Distribuzione addebiti internazionali per Churn")

t097 <- table(train$total_intl_charge, train$churn)
assocstats(t097)

t098 <- table(train$total_charge, train$churn)
assocstats(t098)

# NUMBER_COSTUMER_SERVICE_CALLS
table(original_train$number_customer_service_calls)
ggplot(original_train, aes(x = number_customer_service_calls)) + geom_histogram(bins = 72)
boxplot(original_train$number_customer_service_calls)

# Si divide dunque n_c_s_c in 3 classi:
# Da 0-1 chiamata -----> LOW
# Da 2-3 chiamate -----> MEDIUM
# Da 4-9 chiamate -----> HIGH
train$number_customer_service_calls <- train %>%
  mutate(number_customer_service_calls = case_when(
    number_customer_service_calls >= 4 ~ "High",
    number_customer_service_calls >= 2 ~ "Medium",
    TRUE ~ "Low"
  )) %>%
  pull(number_customer_service_calls) %>%
  as.factor()

estremi_classi <- c(0, 2, 4, 10)
classi <- cut(original_train$number_customer_service_calls, breaks = estremi_classi, right = FALSE)
levels(classi) <- c("Low", "Medium", "High")
fr.ass <- table(classi)
pareto.chart(data = fr.ass, main = "Diagramma di pareto")

# NUMBER_VMAIL_MESSAGES
table(original_train$number_vmail_messages)
t <- original_train %>%
  filter(number_vmail_messages != 0)
describe(t$number_vmail_messages)

table(original_train$number_vmail_messages, train$voice_mail_plan)
ggplot(t, aes(x = number_vmail_messages)) + geom_histogram()
boxplot(t$number_vmail_messages)

# Suddividere in due classi: 0 (nessun messaggio) e 1 (almeno un messaggio)
train$number_vmail_messages <- ifelse(train$number_vmail_messages == 0, 0, 1) %>%
  as.factor()
table(train$number_vmail_messages)

ggplot(train, aes(x = number_vmail_messages)) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = number_vmail_messages)) +
  labs(x = "Voice-mail", y = "Frequenza Percentuale") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = ..prop..), stat = "count", vjust = 40)

# TOTAL_INTL_CALLS
ggplot(train, aes(x = total_intl_calls)) + geom_bar(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train$total_intl_calls), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train$total_intl_calls, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione chiamate internazionali giornaliere")
table(train$total_intl_calls) / nrow(train) * 100
cumsum(table(train$total_intl_calls) / nrow(train) * 100)
boxplot(train$total_intl_calls)

# TOTAL_INTL_MINUTES
f2 <- original_train %>% filter(total_intl_minutes != 0)
gg1 <- ggplot(f2, aes(x = total_intl_minutes)) + geom_bar(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(f2$total_intl_minutes), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(f2$total_intl_minutes, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione chiamate internazionali giornaliere")

gg2 <- ggplot(train, aes(x = total_intl_minutes)) + geom_bar(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train$total_intl_minutes), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train$total_intl_minutes, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione chiamate internazionali giornaliere")
plot_grid(gg1, gg2)

f3 <- original_train %>% filter(total_intl_minutes == 0)
table(f3$total_intl_minutes, f3$international_plan)
boxplot(f2$total_intl_minutes)
boxplot(train$total_intl_calls)

# TOTAL_INTL_CHARGE
ggplot(train, aes(x = total_intl_charge)) + geom_bar()
boxplot(train$total_intl_charge)
ggplot(train, aes(x = total_intl_charge, fill = churn)) + geom_bar()

# COME CAMBIA LA QUANTITÀ DI CHIAMATE EFFETTUATE DI GIORNO, DI SERA E DI NOTTE
par(mfrow = c(1, 3))
b1 <- boxplot(train$total_day_calls, xlab = "CALLS", ylim = c(0, 175), main = "DAY")
b2 <- boxplot(train$total_eve_calls, xlab = "CALLS", ylim = c(0, 175), main = "EVENING")
b3 <- boxplot(train$total_night_calls, xlab = "CALLS", ylim = c(0, 175), main = "NIGHT")

# COME CAMBIA LA QUANTITÀ di MINUTI IN CHIAMATA DI GIORNO, DI SERA E DI NOTTE
b4 <- boxplot(train$total_day_minutes, xlab = "MINUTES", ylim = c(0, 395), main = "DAY")
b5 <- boxplot(train$total_eve_minutes, xlab = "MINUTES", ylim = c(0, 395), main = "EVENING")
b6 <- boxplot(train$total_night_minutes, xlab = "MINUTES", ylim = c(0, 395), main = "NIGHT")

# COME CAMBIA LA QUANTITÀ DI RICARICHE DURANTE L'ARCO DELLA GIORNATA
b7 <- boxplot(train$total_day_charge, xlab = "CHARGE", ylim = c(0, 60), main = "DAY")
b8 <- boxplot(train$total_eve_charge, xlab = "CHARGE", ylim = c(0, 60), main = "EVENING")
b9 <- boxplot(train$total_night_charge, xlab = "CHARGE", ylim = c(0, 60), main = "NIGHT")
dev.off()

# Frequenza delle chiamate all'assistenza clienti
table(original_train$number_customer_service_calls)
ggplot(original_train, aes(x = number_customer_service_calls)) + geom_histogram(bins = 72)
boxplot(original_train$number_customer_service_calls)

# Suddivisione in 3 classi: LOW, MEDIUM, HIGH
train["number_customer_service_calls"] <- original_train %>%
  mutate(number_customer_service_calls = case_when(
    number_customer_service_calls >= 4 ~ "High",
    number_customer_service_calls >= 2 ~ "Medium",
    TRUE ~ "Low"
  )) %>%
  as.factor()

estremi_classi <- c(0, 2, 4, 10)
classi <- cut(original_train$number_customer_service_calls, breaks = estremi_classi, right = FALSE)
levels(classi) <- c("Low", "Medium", "High")
fr.ass <- table(classi)
pareto.chart(data = fr.ass, main = "Diagramma di pareto")

# Suddivisione messaggi vocali in due classi: 0 e 1
train["number_vmail_messages"] <- ifelse(train$number_vmail_messages == 0, 0, 1) %>% as.factor()
ggplot(train, aes(x = number_vmail_messages)) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = number_vmail_messages)) +
  labs(x = "Voice-mail", y = "Frequenza Percentuale") +
  scale_y_continuous(labels = label_percent(accuracy = .1)) +
  geom_text(aes(label = percent((..count..) / sum(..count..)), y = ..prop..), stat = "count", vjust = 40)

# Distribuzione chiamate internazionali
ggplot(train, aes(x = total_intl_calls)) +
  geom_bar(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train$total_intl_calls), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train$total_intl_calls, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione chiamate internazionali giornaliere")
table(train$total_intl_calls) / nrow(train) * 100
cumsum(table(train$total_intl_calls) / nrow(train) * 100)
boxplot(train$total_intl_calls)

# Chiamate internazionali senza valori nulli
f2 <- original_train %>% filter(total_intl_minutes != 0)
gg1 <- ggplot(f2, aes(x = total_intl_minutes)) +
  geom_bar(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(f2$total_intl_minutes), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(f2$total_intl_minutes, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione chiamate internazionali giornaliere")

gg2 <- ggplot(train, aes(x = total_intl_minutes)) +
  geom_bar(fill = "#69b3a2", color = "#e9ecef") +
  geom_vline(xintercept = mean(train$total_intl_minutes), col = "red", lwd = 2) +
  geom_vline(xintercept = quantile(train$total_intl_minutes, c(0.25, 0.75)), col = "blue", lwd = 1, lty = 2) +
  ggtitle("Distribuzione chiamate internazionali giornaliere")
plot_grid(gg1, gg2)

# Distribuzione ricariche totali internazionali
ggplot(train, aes(x = total_intl_charge)) + geom_bar()
ggplot(train, aes(x = total_intl_charge, fill = churn)) + geom_bar()
boxplot(train$total_intl_charge)

# Distribuzione delle chiamate in diverse fasce orarie
par(mfrow = c(1, 3))
boxplot(train$total_day_calls, xlab = "CALLS", ylim = c(0, 175), main = "DAY")
boxplot(train$total_eve_calls, xlab = "CALLS", ylim = c(0, 175), main = "EVENING")
boxplot(train$total_night_calls, xlab = "CALLS", ylim = c(0, 175), main = "NIGHT")

# Quantità di minuti in chiamata in diverse fasce orarie
boxplot(train$total_day_minutes, xlab = "MINUTES", ylim = c(0, 395), main = "DAY")
boxplot(train$total_eve_minutes, xlab = "MINUTES", ylim = c(0, 395), main = "EVENING")
boxplot(train$total_night_minutes, xlab = "MINUTES", ylim = c(0, 395), main = "NIGHT")

# Quantità di ricariche durante l'arco della giornata
boxplot(train$total_day_charge, xlab = "CHARGE", ylim = c(0, 60), main = "DAY")
boxplot(train$total_eve_charge, xlab = "CHARGE", ylim = c(0, 60), main = "EVENING")
boxplot(train$total_night_charge, xlab = "CHARGE", ylim = c(0, 60), main = "NIGHT")
dev.off()

# Analisi piani tariffari e frequenza chiamate internazionali
ggplot(train, aes(fill = international_plan, x = total_intl_calls)) + geom_bar(position = "fill")
table(train$international_plan, train$total_intl_calls)[1, ]

# Minuti in chiamata per i clienti senza piano internazionale
ggplot(f2, aes(fill = international_plan, x = total_intl_minutes)) + geom_histogram(position = "fill")
ggplot(train, aes(x = total_intl_minutes)) +
  geom_bar(aes(y = (..count..) / sum(..count..), fill = international_plan)) +
  labs(x = "Minutes", y = "frequenze relative") +
  scale_y_continuous(labels = label_percent(accuracy = .1)) +
  geom_text(aes(label = percent((..count..) / sum(..count..)), y = ..prop..), stat = "count", vjust = 55)

# Churn rate in relazione al numero di chiamate all'assistenza
ggplot(train, aes(x = number_customer_service_calls, fill = churn)) + geom_bar(position = "fill")
tc <- addmargins(table(train$churn, train$number_customer_service_calls))
tc[2, ] / tc[3, ]
prop.table(table(train$churn, train$number_customer_service_calls), 1)
prop.table(table(train$churn, train$number_customer_service_calls), 2)

# Matrice di correlazione delle variabili numeriche
var.num <- train %>%
  select(-c(state, international_plan, voice_mail_plan, churn, number_customer_service_calls, total_calls,
            total_charge, total_minutes, number_vmail_messages))
corr.m <- cor(var.num)
corrplot(corr.m, type = "upper", title = "Matrice di correlazione", addCoef.col = 'grey')

# Rimozione delle variabili CHARGE
var.num <- train %>%
  select(-c(state, international_plan, voice_mail_plan, churn, number_customer_service_calls, total_calls,
            total_charge, total_minutes, total_day_charge, total_eve_charge, total_night_charge, total_intl_charge, number_vmail_messages))
corr.m <- cor(var.num)
corrplot(corr.m, type = "upper", title = "Matrice di correlazione", addCoef.col = 'grey')

# Matrice di varianza-covarianza
cov(var.num)
chart.Correlation(var.num)
ggpairs(var.num)

# Interazione tra variabili quantitative e categoriche
g2 <- ggplot(train, aes(y = n_weeks, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)
g3 <- ggplot(train, aes(y = total_day_minutes, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)
g4 <- ggplot(train, aes(y = total_day_calls, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)
g5 <- ggplot(train, aes(y = total_eve_minutes, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)
g6 <- ggplot(train, aes(y = total_eve_calls, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)
g7 <- ggplot(train, aes(y = total_night_minutes, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)
g8 <- ggplot(train, aes(y = total_night_calls, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)
g9 <- ggplot(train, aes(y = total_intl_minutes, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)
g10 <- ggplot(train, aes(y = total_intl_calls, x = churn, fill = churn)) + geom_boxplot(show.legend = FALSE)

cowplot::plot_grid(g2,g3,g4,g5,g6,g7,g8,g9,g10)

# Boxplots per variabili quantitative rispetto a international_plan
g12 <- ggplot(train, aes(y = n_weeks, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)
g13 <- ggplot(train, aes(y = total_day_minutes, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)
g14 <- ggplot(train, aes(y = total_day_calls, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)
g15 <- ggplot(train, aes(y = total_eve_minutes, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)
g16 <- ggplot(train, aes(y = total_eve_calls, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)
g17 <- ggplot(train, aes(y = total_night_minutes, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)
g18 <- ggplot(train, aes(y = total_night_calls, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)
g19 <- ggplot(train, aes(y = total_intl_minutes, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)
g20 <- ggplot(train, aes(y = total_intl_calls, x = international_plan, fill = international_plan)) + geom_boxplot(show.legend = FALSE)

cowplot::plot_grid(g12, g13, g14, g15, g16, g17, g18, g19, g20)

# Boxplots per variabili quantitative rispetto a voice_mail_plan
g22 <- ggplot(train, aes(y = n_weeks, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)
g23 <- ggplot(train, aes(y = total_day_minutes, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)
g24 <- ggplot(train, aes(y = total_day_calls, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)
g25 <- ggplot(train, aes(y = total_eve_minutes, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)
g26 <- ggplot(train, aes(y = total_eve_calls, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)
g27 <- ggplot(train, aes(y = total_night_minutes, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)
g28 <- ggplot(train, aes(y = total_night_calls, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)
g29 <- ggplot(train, aes(y = total_intl_minutes, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)
g30 <- ggplot(train, aes(y = total_intl_calls, x = voice_mail_plan, fill = voice_mail_plan)) + geom_boxplot(show.legend = FALSE)

cowplot::plot_grid(g22, g23, g24, g25, g26, g27, g28, g29, g30)

# Adattamento del test set per uniformarlo al train set
test["international_plan"] <- as.factor(ifelse(test$international_plan == "yes", 1, 0))
test["voice_mail_plan"] <- as.factor(ifelse(test$voice_mail_plan == "yes", 1, 0))
colnames(test)[3] <- "n_weeks"

test["number_customer_service_calls"] <- test %>%
  select(number_customer_service_calls) %>%
  mutate(number_customer_service_calls = case_when(
    number_customer_service_calls >= 4 ~ "High",
    number_customer_service_calls >= 2 ~ "Medium",
    TRUE ~ "Low"
  ))
test["number_customer_service_calls"] <- as.factor(test$number_customer_service_calls)

test["number_vmail_messages"] <- as.factor(ifelse(test$number_vmail_messages == 0, 0, 1))

test_model <- test %>%
  select(-c(id, area_code, total_day_minutes, total_eve_minutes, total_night_minutes, total_intl_minutes))

view(test_model)

# Adattamento di sample_submission
sub <- sample_submission %>%
  mutate(churn = ifelse(sample_submission$churn == "yes", 1, 0))

# Verifica collinearità tra variabili
assocstats(table(train_model$number_vmail_messages, train_model$voice_mail_plan))

# Creazione di train_model escludendo variabili ridondanti
train_model <- train %>%
  dplyr::select(-c(total_charge, total_calls, total_minutes, total_day_minutes,
                   total_eve_minutes, total_night_minutes, total_intl_minutes,
                   number_vmail_messages))
view(train_model)
str(train_model)

# Conteggio churn nel dataset di addestramento
table(train$churn)

# Suddivisione in train e test set
set.seed(5)
Sample <- createDataPartition(y = train_model$churn, p = 0.80, list = FALSE)
Sample_train <- train_model[Sample, ]
Sample_test <- train_model[-Sample, ]
dim(Sample_train)
table(Sample_train$churn) / nrow(Sample_train)  # Verifica proporzioni churn

# Modello di regressione logistica iniziale
md <- glm(churn ~ ., data = Sample_train, family = binomial(link = "logit"))
summary(md)

# Test di significatività per le variabili
anova(md, test = "Chisq")

# Pseudo R2 per il modello
PseudoR2(md, c("McFadden", "Nagel", "CoxSnell"))
lrtest(md)

# Odds ratio e probabilità
exp(coef(md))
exp(coef(md)) / (1 + exp(coef(md)))

# Valutazione dell'accuratezza del modello
pd <- predict(md, Sample_test, type = "response")
pd_class <- ifelse(pd > 0.5, 1, 0)
confusionMatrix(as.factor(pd_class), as.factor(Sample_test$churn), positive = "1", mode = "everything")

roc.curve(Sample_test$churn, pd)

pd2 <- predict(md, test, type = "response")
pd_class2 <- ifelse(pd2 > 0.5, 1, 0)
confusionMatrix(as.factor(pd_class2), as.factor(sub$churn), positive = "1", mode = "everything")

roc.curve(sub$churn, pd2)

# Test multicollinearità
blr_vif_tol(md)
blr_coll_diag(md)

# Secondo modello logit con selezione variabili
md3 <- glm(churn ~ state + n_weeks + international_plan + voice_mail_plan + 
             total_day_charge + total_eve_charge + total_night_charge +
             total_intl_calls + total_intl_charge + number_customer_service_calls,
           data = Sample_train, family = binomial(link = "logit"))
summary(md3)

anova(md3, test = "Chisq")

# Pseudo R2 per il modello
PseudoR2(md3, c("McFadden", "Nagel", "CoxSnell"))
lrtest(md3)

# Odds ratio e probabilità
exp(coef(md3))
exp(coef(md3)) / (1 + exp(coef(md3)))

# Valutazione dell'accuratezza del modello
pd3 <- predict(md3, Sample_test, type = "response")
pd3_class <- ifelse(pd3 > 0.5, 1, 0)
confusionMatrix(as.factor(pd3_class), as.factor(Sample_test$churn), positive = "1", mode = "everything")

roc.curve(Sample_test$churn, pd3)

pd4 <- predict(md3, test, type = "response")
pd_class4 <- ifelse(pd4 > 0.5, 1, 0)
confusionMatrix(as.factor(pd_class4), as.factor(sub$churn), positive = "1", mode = "everything")

roc.curve(sub$churn, pd4)

# Test multicollinearità
blr_vif_tol(md3)
blr_coll_diag(md3)

# Curva ROC per il modello
roc1 <- rocit(score = pd4, class = sub$churn)
plot(roc1)

# Modello con cross-validation SMOTE
set.seed(1)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, sampling = "smote")

logistic_model <- train(churn ~ state + international_plan + voice_mail_plan +
                          total_day_charge + total_eve_charge + total_night_charge +
                          total_intl_charge + number_customer_service_calls + total_intl_calls,  
                        data = train_model, 
                        method = "glm",
                        preProcess = c("center", "scale"),  
                        trControl = control)

logistic_predict_test <- predict(logistic_model, test_model)
confusionMatrix(as.factor(logistic_predict_test), as.factor(sub$churn), positive = "1", mode = "everything")
roc.curve(logistic_predict_test, as.factor(sub$churn))

# Modello Forward Selection
mod.null <- glm(churn ~ 1, data = train_model, family = binomial(link = "cloglog"))
variables <- names(train_model)[c(1:13)]
variables <- variables[-1]
model01 <- paste("churn", str_c(variables, collapse = "+"), sep = "~")

Step5.reg <- stepAIC(mod.null, direction = "forward", scope = model01, trace = TRUE)
summary(Step5.reg)

mod2.final <- glm(Step5.reg$formula, data = train_model, family = binomial(link = "cloglog"))
summary(mod2.final)

pre <- predict(mod2.final, newdata = test_model, type = "response")
pred_class <- ifelse(pre > 0.5, 1, 0)
confusionMatrix(as.factor(pred_class), as.factor(sub$churn), positive = "1", mode = "everything")

# Modello Backward Selection
variables <- names(train_model)[c(1:14)]
model02 <- paste("churn", str_c(variables, collapse = "+"), sep = "~")

mod.full <- glm(model02, data = train_model, family = binomial(link = "cloglog"))
Step5.reg <- stepAIC(mod.full, direction = "backward", scope = model02, trace = TRUE)
summary(Step5.reg)

mod2.final <- glm(Step5.reg$formula, data = train_model, family = binomial(link = "logit"))
summary(mod2.final)

pre <- predict(mod2.final, newdata = test_model, type = "response")
pred_class <- ifelse(pre > 0.5, 1, 0)
confusionMatrix(as.factor(pred_class), as.factor(sub$churn))

# Stepwise Regression
mod.null <- glm(churn ~ 1, data = train_model, family = binomial(link = "logit"))
variables <- names(train_model)[c(1:14)]
model1 <- paste("churn", str_c(variables, collapse = "+"), sep = "~")

Step5.reg <- stepAIC(mod.null, direction = "both", scope = model1, trace = TRUE)
summary(Step5.reg)

mod2.final <- glm(Step5.reg$formula, data = train_model, family = binomial(link = "logit"))
summary(mod2.final)

pre <- predict(mod2.final, newdata = test_model, type = "response")
pred_class <- ifelse(pre > 0.5, 1, 0)
confusionMatrix(as.factor(pred_class), as.factor(sub$churn))

# Distribuzione churn nel test set finale
table(sub$churn) / nrow(sub)

# Re-leveling della variabile churn
train_model$churn <- relevel(train_model$churn, ref = "1")

# Primo modello di regressione logistica: Tutti i charge sono statisticamente significativi
model1 <- glm(churn ~ ., data = train_model, family = binomial(link = "logit"))
summary(model1)

# Test di significatività per le variabili
anova(model1, test = "Chisq")

# Indici di Pseudo R2 per bontà di adattamento del modello
PseudoR2(model1, c("McFadden", "Nagel", "CoxSnell"))

# Test di multicollinearità
blr_vif_tol(model1)
blr_coll_diag(model1)

# Effetto marginale
pred1 <- predict(model1, newdata = test_model, type = "response")

# IN-SAMPLE: Calcolo accuracy e sensibilità
ranking <- predict(model1, type = "response")
predicted.classes <- ifelse(ranking > 0.5, 1, 0)
confusionMatrix(as.factor(predicted.classes), as.factor(train$churn), positive = "1", mode = "everything")

# AUC
roc.curve(train_model$churn, ranking)

# OUT-OF-SAMPLE: Calcolo accuracy e sensibilità
ranki <- predict(model1, newdata = test_model, type = "response")
predicti <- ifelse(ranki > 0.5, 1, 0)
confusionMatrix(as.factor(predicti), as.factor(sub$churn), positive = "1", mode = "everything")

roc.curve(sub$churn, predicti)

# Secondo modello logit (esclusione di variabili non significative)
model2 <- glm(churn ~ state + international_plan + voice_mail_plan + total_day_charge + total_eve_charge +
                total_night_charge + total_intl_charge + number_customer_service_calls + total_intl_calls,
              data = train_model, family = binomial(link = "logit"))
summary(model2)

anova(model2, test = "Chisq")
PseudoR2(model2, c("McFadden", "Nagel", "CoxSnell"))

# IN-SAMPLE accuracy e sensibilità
ranking0 <- predict(model2, type = "response")
predicted.classes0 <- ifelse(ranking0 > 0.5, 1, 0)
confusionMatrix(as.factor(predicted.classes0), as.factor(train$churn), positive = "1", mode = "everything")

# OUT-OF-SAMPLE accuracy e sensibilità
ranki0 <- predict(model2, newdata = test_model, type = "response")
predicti0 <- ifelse(ranki0 > 0.5, 1, 0)
confusionMatrix(as.factor(predicti0), as.factor(sub$churn), positive = "1", mode = "everything")

blr_vif_tol(model2)
blr_coll_diag(model2)

# Modello Probit IN-SAMPLE
model_probit <- glm(churn ~ ., data = train_model, family = binomial(link = "probit"))
summary(model_probit)
anova(model_probit, test = "Chisq")

pred2 <- predict(model_probit, newdata = test_model, type = "response")
ranking2 <- predict(model_probit, type = "response")
predicted.classes2 <- ifelse(ranking2 > 0.5, 1, 0)
confusionMatrix(as.factor(predicted.classes2), as.factor(train$churn), positive = "1", mode = "everything")

# Modello Complementary Log-Log (CLogLog)
model_clog <- glm(churn ~ . -state, data = train_model, family = binomial(link = "cloglog"))
summary(model_clog)
anova(model_clog, test = "Chisq")

pred3 <- predict(model_clog, newdata = test_model, type = "response")
predicted.classes3 <- ifelse(pred3 > 0.5, 1, 0)
confusionMatrix(as.factor(predicted.classes3), as.factor(sub$churn), positive = "1", mode = "everything")

# CLogLog con variabili selezionate
model2_clog <- glm(churn ~ state + international_plan + voice_mail_plan + total_day_charge + total_eve_charge +
                     total_night_charge + total_intl_charge + number_customer_service_calls + total_intl_calls,
                   data = train_model, family = binomial(link = "cloglog"))
summary(model2_clog)
anova(model2_clog, test = "Chisq")

pred_clog <- predict(model2_clog, newdata = test_model, type = "response")
predicted.classes_clog <- ifelse(pred_clog > 0.5, 1, 0)
confusionMatrix(as.factor(predicted.classes_clog), as.factor(sub$churn), positive = "1", mode = "everything")

# Metodo ROSE per riequilibrare il dataset
library(rpart)

# Randomico
treeimb <- rpart(churn ~ ., data = train_model)
pred.treeimb <- predict(treeimb, newdata = test_model)
accuracy.meas(sub$churn, pred.treeimb[, 2])
roc.curve(sub$churn, pred.treeimb[, 2])

# Oversampling
data_balanced_over <- ovun.sample(churn ~ ., data = train_model, method = "over", N = 7304)$data
tree.over <- rpart(churn ~ ., data = data_balanced_over)
pred.tree.over <- predict(tree.over, newdata = test_model)
roc.curve(sub$churn, pred.tree.over[, 2])

# Undersampling
data_balanced_under <- ovun.sample(churn ~ ., data = train_model, method = "under", N = 1196, seed = 1)$data
tree.under <- rpart(churn ~ ., data = data_balanced_under)
pred.tree.under <- predict(tree.under, newdata = test_model)
roc.curve(sub$churn, pred.tree.under[, 2])

# Both Oversampling and Undersampling
data_balanced_both <- ovun.sample(churn ~ ., data = train_model, method = "both", p = 0.5, N = 1000, seed = 1)$data
tree.both <- rpart(churn ~ ., data = data_balanced_both)
pred.tree.both <- predict(tree.both, newdata = test_model)
roc.curve(sub$churn, pred.tree.both[, 2])

# MODELLI ADDESTRATI SU TRAIN E CON PREDICT SU TEST 
set.seed(1)
model1 <- glm(churn ~ . - state, data = train_model, family = binomial(link = "logit"))
summary(model1)

# Predizione e valutazione in sample
ranking <- predict(model1, type = "response")
predicted.classes <- ifelse(ranking > 0.14, 1, 0)
confusionMatrix(as.factor(predicted.classes), as.factor(train$churn), positive = "1", mode = "everything")

# AUC in sample
roc1 <- rocit(score = ranking, class = train$churn)       
plot(roc1)

# Predizione e valutazione out-of-sample
ranki <- predict(model1, newdata = test_model, type = "response")
predicti <- ifelse(ranki > 0.14, 1, 0)
confusionMatrix(as.factor(predicti), as.factor(sub$churn), positive = "1", mode = "everything")

# AUC out-of-sample
roc2 <- rocit(score = ranki, class = sub$churn)    
plot(roc2)


# CLOGLOG Model
set.seed(1)
model_clog <- glm(churn ~ . - state, data = train_model, family = binomial(link = "cloglog"))
summary(model_clog)

# Predizione e valutazione in sample
pred_clog <- predict(model_clog, type = "response")
class_clog <- ifelse(pred_clog > 0.14, 1, 0)
confusionMatrix(as.factor(class_clog), as.factor(train$churn), positive = "1", mode = "everything")

# AUC in sample
roc5 <- rocit(score = pred_clog, class = train$churn) 
plot(roc5)

# Predizione e valutazione out-of-sample
pred3 <- predict(model_clog, newdata = test_model, type = "response")
predicted.classes3 <- ifelse(pred3 > 0.14, 1, 0)
confusionMatrix(as.factor(predicted.classes3), as.factor(sub$churn), positive = "1", mode = "everything")

# AUC out-of-sample
roc6 <- rocit(score = pred3, class = sub$churn) 
plot(roc6)


# MODELLI CON SOLO LE VARIABILI SIGNIFICATIVE 
set.seed(1)
s_mod <- glm(churn ~ international_plan + voice_mail_plan + total_day_charge + total_eve_charge + 
               total_night_charge + total_intl_calls + total_intl_charge + number_customer_service_calls,
             data = train_model, family = binomial(link = "logit"))
summary(s_mod)

s_ranking <- predict(s_mod, newdata = test_model, type = "response")
s_prediction <- ifelse(s_ranking > 0.14, 1, 0) 
confusionMatrix(as.factor(s_prediction), as.factor(sub$churn), positive = "1", mode = "everything")

# AUC per modello logit
roc9 <- rocit(score = s_ranking, class = sub$churn)    
plot(roc9)

# CLOGLOG con variabili significative
set.seed(1)
s_mod2 <- glm(churn ~ international_plan + voice_mail_plan + total_day_charge + total_eve_charge + 
                total_night_charge + total_intl_calls + total_intl_charge + number_customer_service_calls,
              data = train_model, family = binomial(link = "cloglog"))
summary(s_mod2)

s_ranking2 <- predict(s_mod2, newdata = test_model, type = "response")
s_prediction2 <- ifelse(s_ranking2 > 0.14, 1, 0)
confusionMatrix(as.factor(s_prediction2), as.factor(sub$churn), positive = "1", mode = "everything")

# AUC per modello cloglog
roc10 <- rocit(score = s_ranking2, class = sub$churn)    
plot(roc10)


# OVERSAMPLING
set.seed(1)
data_balanced_over <- ovun.sample(churn ~ ., data = train_model, method = "over", N = 7304)$data

model_over <- glm(churn ~ . - state, data = data_balanced_over, family = binomial(link = "cloglog"))
summary(model_over)

pred_over <- predict(model_over, newdata = test_model, type = "response")
prediction_over <- ifelse(pred_over > 0.5, 1, 0)
confusionMatrix(as.factor(prediction_over), as.factor(sub$churn), positive = "1", mode = "everything")

roc11 <- rocit(score = pred_over, class = sub$churn)    
plot(roc11)


# UNDERSAMPLING
set.seed(1)
data_balanced_under <- ovun.sample(churn ~ ., data = train_model, method = "under", N = 1196, seed = 1)$data

model_under <- glm(churn ~ . - state, data = data_balanced_under, family = binomial(link = "cloglog"))
summary(model_under)

pred_under <- predict(model_under, newdata = test_model, type = "response")
prediction_under <- ifelse(pred_under > 0.5, 1, 0)
confusionMatrix(as.factor(prediction_under), as.factor(sub$churn), positive = "1", mode = "everything")

roc12 <- rocit(score = pred_under, class = sub$churn)    
plot(roc12)


# ANALISI DISCRIMINANTE
# LDA
set.seed(1)
model_lda <- lda(churn ~ . - state, data = train_model)
pred_lda <- predict(model_lda, newdata = test_model)
confusionMatrix(as.factor(pred_lda$class), as.factor(sub$churn), positive = "1", mode = "everything")

roc13 <- rocit(pred_lda$posterior[,2], sub$churn)
plot(roc13)

# QDA
set.seed(1)
model_qda <- qda(churn ~ . - state, data = train_model)
pred_qda <- predict(model_qda, newdata = test_model)
confusionMatrix(as.factor(pred_qda$class), as.factor(sub$churn), positive = "1", mode = "everything")

roc14 <- rocit(pred_qda$posterior[,2], sub$churn)
plot(roc14)


# CART (Alberi di classificazione)
set.seed(1)
fit.cart <- train(churn ~ . - state, data = train_model, method = "rpart")
pred.cart <- predict(fit.cart, newdata = test_model)
accuracy.meas(sub$churn, pred.cart)
roc.curve(as.factor(sub$churn), pred.cart)

# Random Forest
set.seed(1)
fit.rf <- train(churn ~ . - state, data = train_model, method = "rf")
pred.rf <- predict(fit.rf, newdata = test_model)
confusionMatrix(as.factor(pred.rf), as.factor(sub$churn), positive = "1", mode = "everything")
roc.curve(as.factor(sub$churn), pred.rf)


# PARTIZIONAMENTO (VARIABILE TARGET) PER NUOVO TEST SET
set.seed(1)
idx_train <- createDataPartition(train_model$churn, p = 0.8, times = 1, list = TRUE)[[1]]
dat_train <- train_model[idx_train,]
dat_test <- train_model[-idx_train, ]

# CART con nuovo test set
set.seed(1)
mod.cart <- train(churn ~ . - state, data = dat_train, method = "rpart")
prediction.cart <- predict(mod.cart, newdata = dat_test)
accuracy.meas(dat_test$churn, prediction.cart)
roc.curve(as.factor(dat_test$churn), prediction.cart)

# Random Forest con nuovo test set
set.seed(1)
mod.rf <- train(churn ~ . - state, data = dat_train, method = "rf")
prediction.rf <- predict(mod.rf, newdata = dat_test)
confusionMatrix(as.factor(prediction.rf), as.factor(dat_test$churn), positive = "1", mode = "everything")
roc.curve(as.factor(dat_test$churn), prediction.rf)

# QDA con nuovo test set
set.seed(1)
mod_qda <- qda(churn ~ . - state, data = dat_train)
prediction_qda <- predict(mod_qda, newdata = dat_test)
confusionMatrix(as.factor(prediction_qda$class), as.factor(dat_test$churn), positive = "1", mode = "everything")

roc15 <- rocit(prediction_qda$posterior[,2], dat_test$churn)
plot(roc15)

# LDA con nuovo test set
set.seed(1)
mod_lda <- lda(churn ~ . - state, data = dat_train)
prediction_lda <- predict(mod_lda, newdata = dat_test)
confusionMatrix(as.factor(prediction_lda$class), as.factor(dat_test$churn), positive = "1", mode = "everything")

roc16 <- rocit(prediction_lda$posterior[,2], dat_test$churn)
plot(roc16)


# CLOGLOG con nuovo test set
set.seed(1)
model_clog <- glm(churn ~ . - state, data = dat_train, family = binomial(link = "cloglog"))
prediction_clog <- predict(model_clog, newdata = dat_test, type = "response")

# UNDERSAMPLING
set.seed(1)
data_balanced_under2 <- ovun.sample(churn ~ ., data = dat_train, method = "under", N = 1196)$data
model_under2 <- glm(churn ~ . -state, data = data_balanced_under2, family = binomial(link = "cloglog"))
summary(model_under2)

pred_under2 <- predict(model_under2, newdata = dat_test, type = "response")
prediction_under2 <- ifelse(pred_under2 > 0.5, 1, 0)
confusionMatrix(as.factor(prediction_under2), as.factor(dat_test$churn), positive = "1", mode = "everything")

roc20 <- rocit(score = pred_under2, class = dat_test$churn)
roc20
plot(roc20)

########################
# ANALISI DISCRIMINANTE SUL TEST SET GENERATO
# LDA
set.seed(1)
mod_lda2 <- lda(churn ~ . -state, data = dat_train)
pred_lda2 <- predict(mod_lda2, newdata = dat_test)
confusionMatrix(as.factor(pred_lda2$class), as.factor(dat_test$churn), positive = "1", mode = "everything")

roc21 <- rocit(pred_lda2$posterior[, 2], dat_test$churn)
roc21
plot(roc21)

# QDA
set.seed(1)
mod_qda2 <- qda(churn ~ . -state, data = dat_train)
pred_qda2 <- predict(mod_qda2, newdata = dat_test)
confusionMatrix(as.factor(pred_qda2$class), as.factor(dat_test$churn), positive = "1", mode = "everything")

roc22 <- rocit(pred_qda2$posterior[, 2], dat_test$churn)
roc22
plot(roc22)

#######################################
# ALBERI DI CLASSIFICAZIONE (CART) E RANDOM FOREST
# CART
set.seed(1)
mod_cart2 <- train(churn ~ . -state, data = dat_train, method = "rpart")
prediction_cart2 <- predict(mod_cart2, newdata = dat_test)
accuracy.meas(dat_test$churn, prediction_cart2)
roc.curve(as.factor(dat_test$churn), prediction_cart2)

# RANDOM FOREST
set.seed(1)
mod_rf2 <- train(churn ~ . -state, data = dat_train, method = "rf")
prediction_rf2 <- predict(mod_rf2, newdata = dat_test)
confusionMatrix(as.factor(prediction_rf2), as.factor(dat_test$churn), positive = "1", mode = "everything")
roc.curve(as.factor(dat_test$churn), prediction_rf2)


                   


