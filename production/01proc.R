# Code 0: Preparation -----------------------------------------------------
# Valentina Andrade


# 1. Cargar librerias -----------------------------------------------------
pacman::p_load(tidyverse, haven)


# 2. Cargar bases de datos ------------------------------------------------
## ISSP Module Social Inequlities
issp99 <- read_dta("input/data/original/ISSP1999.dta")
issp09 <- read_dta("input/data/original/ISSP2009.dta")
issp19 <- read_dta("input/data/original/ISSP2019.dta")


#  3. Explorar ------------------------------------------------------------
names(issp99)
issp99 <- issp99 %>% filter(v3 == 30) %>% mutate(year = 1999,factor = weight)
issp09 <- issp09 %>% mutate(year = 2009, factor = WEIGHT)
issp19 <- issp19 %>% mutate(year = 2019, factor = FACTOR)

# 4. Substancial variables ------------------------------------------------

# 4.1 Rich people pay more taxes (tax) ------------------------------------------
## 1999: Variable v36
table(issp99$v36)
issp99$tax<- as.numeric(issp99$v36)
issp99$tax <- car::recode(issp99$tax, recodes = c("1='Una proporción mucho mayor';2='Una proporción mayor';3='La misma proporción';4='Una menor proporción';5='Una proporción mucho menor';c(8,9)=NA"), as.factor = T, 
                          levels =c('Una proporción mucho mayor','Una proporción mayor','La misma proporción','Una menor proporción','Una proporción mucho menor'))
table(issp99$tax)  
## 2009: Variable V36
table(issp09$V36)
issp09$tax<- as.numeric(issp09$V36)
issp09$tax <- car::recode(issp09$tax, recodes = c("1='Una proporción mucho mayor';2='Una proporción mayor';3='La misma proporción';4='Una menor proporción';5='Una proporción mucho menor'"), as.factor = T, 
                          levels =c('Una proporción mucho mayor','Una proporción mayor','La misma proporción','Una menor proporción','Una proporción mucho menor'))
table(issp09$tax)  

## 2019: Variable M2_P8A
sjmisc::find_var(issp19, "8A")
table(issp19$M2_P8A)
issp19$tax<- as.numeric(issp19$M2_P8A)
issp19$tax <- car::recode(issp19$tax, recodes = c("1='Una proporción mucho mayor';2='Una proporción mayor';3='La misma proporción';4='Una menor proporción';5='Una proporción mucho menor';c(88,99)=NA"), as.factor = T, 
                          levels =c('Una proporción mucho mayor','Una proporción mayor','La misma proporción','Una menor proporción','Una proporción mucho menor'))
table(issp19$tax)  

# 4.2 Percepcion Tax  -----------------------------------------------------
## 2009
# Variable V37: Q7b Tax: Generally, how would you describe taxes in [Rs country] for those with high incomes? 
## 2019
# En general, ¿cómo describiría Ud. los impuestos en Chile hoy en día para las personas con altos ingresos? Los impuestos son…

# 4.3 Redistribution (red) ----------------------------------------------------------

## 1999: Variable v35
table(issp99$v35)
issp99$red<- as.numeric(issp99$v35)
issp99$red <- car::recode(issp99$red, recodes = c("1='Muy de acuerdo';2='De acuerdo';3='Ni de acuerdo ni desacuerdo';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T, 
                          levels =c('Muy de acuerdo','De acuerdo','Ni de acuerdo ni desacuerdo','En desacuerdo','Muy en desacuerdo'))
table(issp99$red)  
## 2009: Variable V33
table(issp09$V33)
issp09$red<- as.numeric(issp09$V33)
issp09$red <- car::recode(issp09$red, recodes = c("1='Muy de acuerdo';2='De acuerdo';3='Ni de acuerdo ni desacuerdo';4='En desacuerdo';5='Muy en desacuerdo'"), as.factor = T, 
                          levels =c('Muy de acuerdo','De acuerdo','Ni de acuerdo ni desacuerdo','En desacuerdo','Muy en desacuerdo'))
table(issp09$red)  

## 2019: Variable M2_P4_1
sjmisc::find_var(issp19, "diferencias")
table(issp19$M2_P4_1)
issp19$red<- as.numeric(issp19$M2_P4_1)
issp19$red <- car::recode(issp19$red, recodes = c("1='Muy de acuerdo';2='De acuerdo';3='Ni de acuerdo ni desacuerdo';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T, 
                          levels =c('Muy de acuerdo','De acuerdo','Ni de acuerdo ni desacuerdo','En desacuerdo','Muy en desacuerdo'))
table(issp19$red)  

# 4.5 Meritocracy -------------------------------------------------------------

# A. wealthy --------------------------------------------------------------
#1999 - v4
#2009 - V6
#2019 - 

# B. well-educated parents ------------------------------------------------
#1999 - v5
#2009 - V7
#2019 - 

# C. education yourself --------------------------------------------------------------
#1999 - 
#2009 - V8
#2019 - 

# D. ambition ------------------------------------------------
#1999 -
#2009 - V9
#2019 - 
# E. hard work --------------------------------------------------------------
#1999 -
#2009 - V10
#2019 - 

# F. know right people ------------------------------------------------
#1999 -
#2009 - V11
#2019 - 
# G. political connections  --------------------------------------------------------------
#1999 -
#2009 - V12
#2019 - 

# H. giving bribes ------------------------------------------------
#1999 -
#2009 - V13
#2019 - 

# I. person's race ------------------------------------------------
#1999 -
#2009 - V14
#2019 - 

# J. religion ------------------------------------------------
#1999 -
#2009 - V15
#2019 - 

# K. sex ------------------------------------------------
#1999 -
#2009 - V16
#2019 - 

# L. corrupt ------------------------------------------------
#1999 -
#2009 - V17
#2019 - 

# M. best school ------------------------------------------------
#1999 -
#2009 - V18
#2019 - 

# N. rich university ------------------------------------------------
#1999 -
#2009 - V19
#2019 - 

# O. same chances uni ------------------------------------------------
#1999 -
#2009 - V20
#2019 - 


# 5.1 Income (pchhinc y pchhinc_a) --------------------------------------------------------------

## 1999
### Variable hompop: How many persons in household 
### rincome (respondent income) and incomer (Family income by decile 1-10)
issp99$hompop <- as.numeric(issp99$hompop)
issp99$hompop[issp99$hompop == 99] <- NA
issp99$hompop[issp99$hompop == 0] <- 1

sjmisc::find_var(issp99,"income")
issp99$income <- as.numeric(issp99$incomer)
issp99 <- issp99 %>% mutate(income = case_when(incomer == 1~45000,
                                   incomer == 2~105000,
                                   incomer == 3~135000,
                                   incomer == 4~165000,
                                   incomer == 5~195000,
                                   incomer == 6~225000,
                                   incomer == 7~265000,
                                   incomer == 8~340000,
                                   incomer == 9~495000,
                                   incomer == 10~800000,
                                   incomer == 11~1250000,
                                   incomer == 12~1750000,
                                   incomer == 13~2500000,
                                   incomer == 14~3000000,
                                   incomer == 97 ~NA_real_,
                                   incomer == 98 ~NA_real_,
                                   incomer == 99 ~NA_real_),
                            pchhinc=income/hompop,
                            pchhinc_a = pchhinc*142.85/100) #Art. Politicas Publicas UC (linea 149 dofile)


summary(issp99$pchhinc)
## 2009
### Variable HOMPOP: How many persons in household 
### CL_RINC:income specific in Chile
issp09$HOMPOP <- as.numeric(issp09$HOMPOP)
issp09$HOMPOP[issp09$HOMPOP == 99] <- NA
issp09$HOMPOP[issp09$HOMPOP == 0] <- 1

sjmisc::find_var(issp09,"income")
issp09$income <- as.numeric(issp09$CL_RINC)
issp09 <- issp09 %>% mutate(income =  case_when(income == 2000~17500,
                                                        income == 45000~45500,
                                                        income == 66000~67000,
                                                        income == 90000~89500,
                                                        income == 122000~117500,
                                                        income == 155000~158500,
                                                        income == 200000~201500,
                                                        income == 250000~257000,
                                                        income == 310000~324500,
                                                        income == 400000~403000,
                                                        income == 750000~724500,
                                                        income == 1500000~1500000,
                                                        income == 2500000~2500000,
                                                        income == 3100000~3500000,
                                                        income == 9999990 ~NA_real_,
                                                        income == 9999998 ~NA_real_,
                                                        income == 9999999 ~NA_real_),
                            pchhinc=income/HOMPOP,
                            pchhinc_a = pchhinc) 


summary(issp09$pchhinc)
## 2019
### DS_P34 Numero personas en hogar
### DS_P38. 	De los siguientes tramos de ingresos mensuales
issp19$HOMPOP <- as.numeric(issp19$DS_P34)
issp19$HOMPOP[issp19$HOMPOP == 99] <- NA
issp19$HOMPOP[issp19$HOMPOP == 0] <- 1

issp19$income <- as.numeric(issp19$DS_P38)
issp19 <- issp19 %>% mutate(income = case_when(income == 1~17500,
                                               income == 2~45500,
                                               income == 3~67000,
                                               income == 4~89500,
                                               income == 5~117500,
                                               income == 6~158500,
                                               income == 7~201500,
                                               income == 8~257000,
                                               income == 9~324500,
                                               income == 10~403000,
                                               income == 11~724500,
                                               income == 12~1500000,
                                               income == 13~2500000,
                                               income == 14~3500000,
                                               income == 98 ~NA_real_,
                                               income == 99 ~NA_real_),
                            pchhinc=income/HOMPOP,
                            pchhinc_a = pchhinc) 
summary(issp19$pchhinc)

# 5. Educ. Level (educ) ---------------------------------------------------------

## 1999: Variable x_degr
table(issp99$x_degr)
issp99$educ<- as.numeric(issp99$x_degr)
issp99$educ <- car::recode(issp99$educ, recodes = c("c(3001,3002)='No estudió';c(3003,3004)='Básica completa';c(3005,3006, 3008)='Media completa';3010='Superior no universitaria';3007='Universitaria completa';3009=NA"), as.factor = T, 
                          levels =c('No estudió','Básica completa','Media completa','Superior no universitaria','Universitaria completa'))
table(issp99$educ)  
## 2009: Variable CL_DGR
table(issp09$CL_DEGR)
issp09$educ<- as.numeric(issp09$CL_DEGR)
issp09$educ <- car::recode(issp09$educ, recodes = c("c(1,2)='No estudió';c(3,4)='Básica completa';c(5,6,8)='Media completa';9='Superior no universitaria';7='Universitaria completa'"), as.factor = T, 
                          levels =c('No estudió','Básica completa','Media completa','Superior no universitaria','Universitaria completa'))
table(issp09$educ)  

## 2019: Variable DS_P4
sjmisc::find_var(issp19, "DS_P4")
table(issp19$DS_P4)
issp19$educ<- as.numeric(issp19$DS_P4)
issp19$educ <- car::recode(issp19$educ, recodes = c("c(0,1)='No estudió';c(2,3)='Básica completa';c(4,5,7)='Media completa';6='Superior no universitaria';c(8,9)='Universitaria completa';99=NA"), as.factor = T, 
                          levels =c('No estudió','Básica completa','Media completa','Superior no universitaria','Universitaria completa'))
table(issp19$educ)  

# 5. Age (age) ------------------------------------------------------------------

## 1999: Variable age
table(issp99$age)
issp99$age<- as.numeric(issp99$age)
issp99$age <- car::recode(issp99$age, recodes = c("18:29='18-29';30:44='30-44';45:54='45-54';55:64='55-64';65:94='65 o más'"),
                          as.factor = T, 
                           levels =c('18-29','30-44','45-54','55-64','65 o más'))
table(issp99$age)  
## 2009: Variable CL_DGR
table(issp09$AGE)
issp09$age<- as.numeric(issp09$AGE)
issp09$age <- car::recode(issp09$age, recodes = c("18:29='18-29';30:44='30-44';45:54='45-54';55:64='55-64';65:94='65 o más'"),
                          as.factor = T, 
                          levels =c('18-29','30-44','45-54','55-64','65 o más'))
table(issp09$age)  

## 2019: Variable DS_P2
sjmisc::find_var(issp19, "edad")
table(issp19$DS_P2_EXACTA)
issp19$age<- as.numeric(issp19$DS_P2_EXACTA)
issp19$age <- car::recode(issp19$age, recodes = c("18:29='18-29';30:44='30-44';45:54='45-54';55:64='55-64';65:96='65 o más'"),
                          as.factor = T, 
                          levels =c('18-29','30-44','45-54','55-64','65 o más'))
table(issp19$age)  

# 5. Sex (sex) ------------------------------------------------------------------

## 1999: Variable sex
table(issp99$sex)
issp99$sex<- as.numeric(issp99$sex)
issp99$sex <- car::recode(issp99$sex, recodes = c("1='Hombre';2='Mujer'"),
                          as.factor = T, 
                          levels =c('Hombre','Mujer'))
table(issp99$sex)  
## 2009: Variable CL_DGR
table(issp09$SEX)
issp09$sex<- as.numeric(issp09$SEX)
issp09$sex <- car::recode(issp09$sex, recodes = c("1='Hombre';2='Mujer'"),
                          as.factor = T, 
                          levels =c('Hombre','Mujer'))
table(issp09$sex)  

## 2019: Variable DS_P1
sjmisc::find_var(issp19, "DS_P1")
table(issp19$DS_P1)
issp19$sex<- as.numeric(issp19$DS_P1)
issp19$sex <- car::recode(issp19$sex, recodes = c("1='Hombre';2='Mujer'"),
                          as.factor = T, 
                          levels =c('Hombre','Mujer'))
table(issp19$sex)  


# 6. Merge ISSP 99-09-19 --------------------------------------------------

issp <- bind_rows(issp99,issp09,issp19)

issp <- issp %>% 
  select(year, sex, age, educ, pchhinc, pchhinc_a, tax, red, factor)
