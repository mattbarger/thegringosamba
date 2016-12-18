rm(list = ls())
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

url1 <- "http://www.football-data.co.uk/mmz4281/"
url2 <- ".csv"
seas <- c("9596","9697","9798","9899","9900","0001","0102","0203","0304","0405","0506","0607","0708","0809","0910","1011","1112","1213","1314","1415","1516")
leag <- c("E0","SC0","D1","B1","F1","I1","N1","P1","SP1","T1")
lgut <- c("E0" = "England","SC0" = "Scotland","D1" = "Germany","B1" = "Belgium","F1" = "France","I1" = "Italy","N1" = "Holland","P1" = "Portugal","SP1" = "Spain","T1" = "Turkey")
yr <- data.frame(Ssn = seas, Season = 1996:2016,stringsAsFactors = FALSE)
EUR <- data.frame()
df <- data.frame()
for (L in seq_along(leag)) {
  #SCRAPE MATCH DATA FROM URLs
for(S in seq_along(seas)) {
  tmp <- read.csv(paste(url1, paste(seas[S], leag[L], sep = "/"), url2, sep = ""), stringsAsFactors = FALSE)
  tmp <- tmp %>% select(1:6) %>% mutate(Season = seas[S])
  colnames(tmp) <- c("Div","Date","Home","Away","HG","AG","SEAS")
  df <- bind_rows(df, tmp)
  rm(tmp)
  }
}
df <- df %>% separate(Date, c("day", "month", "year"))
df$Date <- as.Date(paste(df$day, df$month, str_sub(df$year, -2), sep = "/"), "%d/%m/%y")
todelete <- which(is.na(df$Date))
df <- df[-todelete,]
rm(todelete)
df <- left_join(df, yr, by = c("SEAS" = "Ssn"))

df$SEAS <- NULL
df$day <- NULL
df$month <- NULL
df$year <- NULL
df$Country <- lgut[df$Div]
df$Home <- str_trim(df$Home)
df$Away <- str_trim(df$Away)
df$Home <- replace(df$Home, which(df$Home == "Middlesboro"), "Middlesbrough")
df$Away <- replace(df$Away, which(df$Away == "Middlesboro"), "Middlesbrough")
df$Home <- replace(df$Home, which(df$Home == "Villareal"), "Villarreal")
df$Away <- replace(df$Away, which(df$Away == "Villareal"), "Villarreal")
df$Home <- replace(df$Home, which(df$Home == "Roda"), "Roda JC")
df$Away <- replace(df$Away, which(df$Away == "Roda"), "Roda JC")
df$Home <- replace(df$Home, which(df$Home == "Chaves"), "Desp. Chaves")
df$Away <- replace(df$Away, which(df$Away == "Chaves"), "Desp. Chaves")
df$Home <- replace(df$Home, which(df$Home == "Campomaior"), "Campomaiorense")
df$Away <- replace(df$Away, which(df$Away == "Campomaior"), "Campomaiorense")

#CREATE DOUBLE ENTRY MATCH DATA
hm <- df %>% select(Country, Season, Date, Team = Home, Opp = Away, GF = HG, GA = AG) %>% mutate(GD = GF - GA, H = 1)
aw <- df %>% select(Country, Season, Date, Team = Away, Opp = Home, GF = AG, GA = HG) %>% mutate(GD = GF - GA, H = 0)
euro <- bind_rows(hm, aw)
rm(hm, aw)
euro <- euro %>% arrange(Date) %>% group_by(Country, Season, Team) %>% mutate(Mct = 1:n())

#IDENTIFY RELEGATED TEAMS
rel <- euro %>% group_by(Country, Season, Team) %>% distinct() %>% mutate(TMP = 1)
rel <- rel %>% spread(Season, TMP, fill = 0) %>% gather(Season, InLeague, -Country, -Team)
rel$Season <- as.integer(rel$Season)
rel <- rel %>% group_by(Country, Season) %>% mutate(InLeague.T2 = lead(InLeague))
rel <- rel %>% group_by(Country, Team) %>% mutate(InLeague.T2 = lead(InLeague, by = Season),
                                                  relegated = ifelse(InLeague == 1 & InLeague.T2 == 0, 1, 0))

#BUILD SHELL DATAFRAMES
lg_index <- unique(euro$Country)

for(L in seq_along(lg_index)) {
eur <- euro %>% filter(Country == lg_index[L])
#CREATE PARAMETERS
home_adv <- 100
k_factor <- 20
mv_multi <- function(i) ifelse(i == 0, 1, sqrt(abs(i)))
sn_index <- unique(euro$Season)
sn_index <- sn_index[order(sn_index)]

#LOOP FOR ALL SEASONS
  for(s in seq_along(sn_index)) {
    dt_index <- unique(eur$Date[eur$Season == sn_index[s]])
    dt_index <- dt_index[order(dt_index)]
    ##ASSIGN INITIAL VALUES
    n <- data.frame(Season = sn_index[1], Team = unique(eur$Team[eur$Season == sn_index[1]]), new.elo.i = 1500, stringsAsFactors = FALSE)
    n <- left_join(n, rel[,c("Season", "Team", "relegated")], by = c("Season", "Team"))
    new.elo <- data.frame(Season = sn_index[s], Team = unique(eur$Team[eur$Season == sn_index[s]]),
                          stringsAsFactors = FALSE)
    new.elo <- left_join(new.elo, n[,c("Team","new.elo.i")], by = "Team", all.x = TRUE)
    new.elo <- replace(new.elo, is.na(new.elo), ifelse(is.na(round(mean(n$new.elo.i[n$relegated == 1]))),
                                                       round(mean(n$new.elo.i)),
                                                       round(mean(n$new.elo.i[n$relegated == 1]))))
    ##CREATE SEASON ELO SHELL
    E <- data.frame()
    for(d in 1:length(dt_index)) {
      tmp <- eur %>% filter(Date == dt_index[d])
      tmp$elo.i <- sapply(seq_len(nrow(tmp)),
                          function(i) with(tmp, ifelse(Mct[i]==1,
                                                       sum(new.elo[new.elo$Team == Team[i],"new.elo.i"]),
                                                       sum(E[E$Season == Season[i] & E$Team == Team[i] & E$Mct == Mct[i] - 1,"elo.n"])
                            )
                          )
      )
      tmp$elo.o  <- sapply(seq_len(nrow(tmp)), function(i) with(tmp, sum(elo.i[Team == Opp[i]])))
      tmp$home_adv <- home_adv
      tmp$result <- with(tmp, ifelse(GD == 0, 0.5, ifelse(GD > 0, 1, 0)))
      tmp$ex_res <- with(tmp, 1/(1+ 10^(-(elo.i - elo.o + 2*home_adv*H - home_adv)/400)))
      tmp$elo.n  <- with(tmp, elo.i + round(k_factor * mv_multi(GD) * (result - ex_res)))
      tmp$pt.gn  <- with(tmp, elo.n - elo.i)
      home_adv <- home_adv + 0.075*sum(tmp$pt.gn[tmp$H == 1])
      E <- bind_rows(E,tmp)
      rm(tmp)
    }
    #END OF SEASON CALCULATIONS
    E <- E %>% mutate(PTS = ifelse(result == 1, 3, ifelse(result == 0, 0, 1)))
    E <- E %>% group_by(Country, Team) %>% mutate(PTS.S = cumsum(PTS),
                                                  GF.S  = cumsum(GF),
                                                  GA.S  = cumsum(GA),
                                                  GD.S  = cumsum(GD))
  
    #CREATE RANKINGS ON POINTS
    E <- E %>% group_by(Country, Mct) %>% arrange(-PTS.S, -GD.S, -GF.S) %>% mutate(RANK.S = row_number())
  
    #CREATE END TABLE
    n <- E %>% ungroup() %>% group_by(Country) %>% filter(Mct == max(Mct)) %>% select(Country, Season, RANK.S, Team, elo.n)
    n <- left_join(n, rel[,c("Country","Season","Team", "relegated")], by = c("Country","Season","Team"))
    n <- n %>% mutate(new.elo.i = round(elo.n * 0.9 + mean(elo.n) * 0.1))
  
    #STORE DATA 
    EUR <- bind_rows(EUR, E)
    rm(E)
  }
}