#AI code:
# Read the dataset
df <- read.csv("Data/copenhagen_rentals.csv")

# Fit the linear regression model
model <- lm(rent_dkk ~ size_m2, data = df)

# Output the coefficients and R-squared
summary(model)

#My Code:
plot(x= df$size_m2, y = df$rent_dkk)

#cleaning outliers
clean_df <- subset(df, rent_dkk<50000)

model_clean <- lm(rent_dkk ~size_m2, data = clean_df)
summary(model_clean)

#opgave 2

#AI code:
# Model 1: Rent on floor
model_floor <- lm(rent_dkk ~ floor, data = df)
summary(model_floor)

#Coefs:  og -228.0 : t-værdier: 17.801 og -1.396

# Model 2: Rent on floor + district
model_district <- lm(rent_dkk ~ floor + district, data = df)
summary(model_district)


#My Code


table(df$district)
tapply(df$floor, df$district, mean)



clean_df$district[clean_df$district=='Norrebro'] <-"Nørrebro"

#model1
model_c_floor <- lm(rent_dkk ~ floor, data = clean_df)
summary(model_c_floor)
table(clean_df$district)
#floor coeff: -162.69 og t-stat = -4.342
#model2 
model_c_district <- lm(rent_dkk ~ floor + district, data = clean_df)
summary(model_c_district)

#floor coeff: 216 og t-stat = 4.071

tapply(clean_df$floor, clean_df$district, mean)
View(clean_df)
tapply(clean_df$rent_dkk, clean_df$district, mean)



#opgave 3 

multi_model <- lm(rent_dkk ~floor +metro_min+ furnished+ energy_label+ district+ size_m2, data = clean_df)
summary(multi_model)


nrow(clean_df)
nrow(model.frame(multi_model))
View(model.frame(multi_model))
table(clean_df$energy_label)

colSums(is.na(clean_df))
#Check: ved at tjekke nrows kan vi se at der kun er 454 ud af 517 rækker i modellen.
# vi kan se at der er et tomt energilabel

#opgave 4
new_multi_model <- lm(rent_dkk ~floor + metro_min + furnished + energy_label +district + size_m2 + year_built+ rooms, data = clean_df)
summary(new_multi_model)

cor(clean_df$size_m2, clean_df$rooms)
nrow(model.frame(multi_model))
nrow(model.frame(new_multi_model))

room_model <- lm(rent_dkk~rooms, data = clean_df)
summary(room_model)

rm(list =ls())

#PART B
#opgave 5

Streaming <- read.csv("Data/streaming_campaign.csv")
View(Streaming)

#check:
cor(Streaming[,2:4])
model <- lm(signups_k ~social_kdkk + search_kdkk +influencer_kdkk, data = Streaming)
summary(model)

simple_model <- lm(signups_k~influencer_kdkk,data = Streaming)
summary(simple_model)

#opgave 6

model_social_search = lm(signups_k ~ search_kdkk + social_kdkk, data = Streaming)
plot(fitted(model_social_search),resid(model_social_search),
     main= 'Residuals vs fitted',
     xlab ='Fitted Values (signups_k)',
     ylab = 'Residuals',
     pch =16,
     col = 'darkgrey')
abline(h=0,col="red")

plot(Streaming$search_kdkk, resid(model_social_search),
     main = 'Residuals vs Search Spend',
     xlab = 'Search Spend (kDKK)',
     ylab = 'residuals',
     pch=16,
     col = 'darkgrey')

abline(h=0,col='red')
