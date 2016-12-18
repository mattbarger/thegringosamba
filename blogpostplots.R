EOY <- EUR %>% group_by(Country, Season) %>% filter(Mct == max(Mct))
EOY <- left_join(EOY, rel[,c("Season","Team","relegated")], by = c("Season","Team"))
EOY$Country <- factor(EOY$Country, levels = c("England", "Germany","Italy","Spain","France","Holland","Portugal","Turkey","Belgium","Scotland"))
EOY16 <- EOY %>% filter(Season == 2016)
EOY16 <- EOY16 %>% mutate(relegated = ifelse(Country == "Scotland" & RANK.S == max(RANK.S) |
                                               Country == "Belgium" & RANK.S == max(RANK.S), 1,
                                             ifelse(Country == "Germany" & RANK.S >= max(RANK.S) - 1 |
                                                      Country == "Holland" & RANK.S >= max(RANK.S) - 1 |
                                                      Country == "Portugal" & RANK.S >= max(RANK.S) - 1, 1,
                                                    ifelse(Country == "England" & RANK.S >= max(RANK.S) - 2 |
                                                             Country == "Italy" & RANK.S >= max(RANK.S) - 2 |
                                                             Country == "Spain" & RANK.S >= max(RANK.S) - 2 |
                                                             Country == "France" & RANK.S >= max(RANK.S) - 2 |
                                                             Country == "Turkey" & RANK.S >= max(RANK.S) - 2, 1,0)
                                             )
)
)
EOY <- bind_rows(EOY, EOY16)
todelete <- which(is.na(EOY$relegated))
EOY <- EOY[-todelete,]

EOYPL <- EOY %>% filter(Country == "England")
EOYPL12 <- EOYPL %>% filter(Season == 2012)
posn.j <- position_jitter(width = 0.2)

ggplot(EOYPL, aes(x = Season, y = elo.n)) +
  geom_point(color = 'navyblue', 
             alpha = 0.5, 
             data = subset(EOYPL, RANK.S != 1 | relegated == 0), position = posn.j) +
  geom_point(color = 'purple', 
             alpha = 0.5, 
             data = subset(EOYPL, relegated == 1), position = posn.j) +
  geom_point(color = 'red', 
             data = subset(EOYPL, RANK.S == 1), position = posn.j) +
  geom_hline(yintercept = 1500, color = 'darkgreen', alpha = 0.4, size = 2) +
  scale_y_continuous(position = "right", breaks = seq(1200,1800, by = 100)) +
  labs(title = "ELO disparity across English Premier League",
       subtitle = "by end-of-season ELO Rating, 1996-2016") +
  theme(plot.title = element_text(size = 16, hjust = 0, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0, face = "italic"),
        axis.title = element_blank()) +
  labs(caption = "League champion denoted in red. Relegated teams in purple. Source: football-data.co.uk", hjust = 0)


ggplot(EOYPL, aes(x = Season, y = elo.n)) +
  geom_boxplot(aes(group = Season), color = 'navyblue', alpha = 0.5, outlier.shape = NA) +
  geom_point(color = 'navyblue', 
             alpha = 0.5, 
             data = subset(EOYPL, RANK.S != 1), position = posn.j) +
  geom_point(color = 'red', 
             data = subset(EOYPL, RANK.S == 1), position = posn.j) +
  geom_hline(yintercept = 1500, color = 'darkgreen', alpha = 0.7, size = 1) +
  scale_y_continuous(position = "right", breaks = seq(1200,1800, by = 100)) +
  labs(title = "ELO disparity across English Premier League",
       subtitle = "by end-of-season ELO Rating, 1996-2016") +
  theme(plot.title = element_text(size = 16, hjust = 0, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0, face = "italic"),
        axis.title = element_blank()) +
  labs(caption = "League champion denoted in red. Source: football-data.co.uk", size = 8, hjust = 0)

ggplot(EOY16, aes(x = Country, y = elo.n)) +
  geom_boxplot(color = 'navyblue', alpha = 0.5, outlier.shape = NA) +
  geom_point(color = 'navyblue', 
             alpha = 0.5, 
             data = subset(EOY16, RANK.S != 1), position = posn.j) +
  geom_text(color = 'red', 
            data = subset(EOY16, RANK.S == 1), 
            aes(x = Country, y = elo.n, label = Team), size = 3) +
  geom_hline(yintercept = 1500, color = 'darkgreen', alpha = 0.4, size = 1) +
  scale_y_continuous(position = "right", breaks = seq(1200,1800, by = 100)) +
  labs(title = "ELO disparity across UEFA domestic first division leagues",
       subtitle = "by end-of-season ELO Rating, 2016") +
  theme(plot.title = element_text(size = 16, hjust = 0, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0, face = "italic"),
        axis.title = element_blank()) +
  labs(caption = "League champion denoted in red. Source: football-data.co.uk", hjust = 0)

ggplot(EOY, aes(x = Season, y = elo.n)) +
  geom_boxplot(aes(group = Season), color = 'navyblue', alpha = 0.5) + 
  geom_point(color = 'navyblue', alpha = 0.2, data = subset(EOY, RANK.S != 1), position = posn.j) + 
  geom_point(color = 'red', data = subset(EOY, RANK.S == 1)) +
  geom_hline(yintercept = 1500, color = 'darkgreen', alpha = 0.4, size = 1) +
  facet_wrap(~ Country, ncol = 2) +
  scale_y_continuous(position = "right") +
  labs(title = "ELO disparity across UEFA domestic first division leagues",
       subtitle = "by end-of-season ELO Rating, 1996-2016") +
  theme(plot.title = element_text(size = 16, hjust = 0, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0, face = "italic"),
        axis.title = element_blank()) +
  labs(caption = "League champions denoted in red. Source: football-data.co.uk", hjust = 0)
  
