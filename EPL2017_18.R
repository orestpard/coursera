head(EPL2017_18)
tail(EPL2017_18)
EPL2017_18[,'hwinvalue']=ifelse(EPL2017_18$FTR=='H',1,ifelse(EPL2017_18$FTR=='D',0.5,0))
EPL2017_18[,'awinvalue']=ifelse(EPL2017_18$FTR=='A',1,ifelse(EPL2017_18$FTR=='D',0.5,0))
EPL2017_18[,'count'] = 1
View(EPL2017_18)
install.packages("dplyr")
library(dplyr)
Enghome <- EPL2017_18 %>% 
  group_by(HomeTeam) %>% 
  dplyr::summarise(
    count = sum(count),
    hwinvalue = sum(hwinvalue),
    FTHG = sum(FTHG),
    FTAG = sum(FTAG)
  ) %>% 
  ungroup() %>% 
  rename(
    team = HomeTeam,
    Ph = count,
    FTHGh = FTHG,
    FTAGh = FTAG
  ) %>% 
  arrange(team)

head(Enghome)

Engaway <- EPL2017_18 %>% 
  group_by(AwayTeam) %>% 
  dplyr::summarise(
    count = sum(count),
    awinvalue = sum(awinvalue),
    FTHG = sum(FTHG),
    FTAG = sum(FTAG)
  ) %>% 
  ungroup() %>% 
  rename(
    team = AwayTeam,
    Pa = count,
    FTHGa = FTHG,
    FTAGa = FTAG
  ) %>% 
  arrange(team)

head(Engaway)

Engtotal <- merge(Enghome, Engaway, by = "team")

Engaway <- EPL2017_18 %>% 
  group_by(AwayTeam) %>% 
  dplyr::summarise(
    count = sum(count),         # πλήθος εκτός έδρας αγώνων
    awinvalue = sum(awinvalue), # πόντοι/νίκες εκτός έδρας
    FTHG = sum(FTHG),           # γκολ που έβαλαν οι αντίπαλοι (δηλαδή γκολ κατά)
    FTAG = sum(FTAG)            # γκολ που έβαλε η ομάδα εκτός έδρας
  ) %>% 
  ungroup() %>% 
  rename(
    team = AwayTeam,
    Pa = count,     # πλήθος εκτός έδρας αγώνων
    FTHGa = FTHG,   # γκολ που δέχτηκε εκτός έδρας
    FTAGa = FTAG    # γκολ που έβαλε εκτός έδρας
  ) %>% 
  arrange(team)

head(Engaway)

Engtotal <- merge(Enghome, Engaway, by = "team")

# Ενώνουμε home και away data frames
Engtotal <- merge(Enghome, Engaway, by = "team")

# Υπολογισμός συνολικών μετρικών
Engtotal <- Engtotal %>%
  mutate(
    Games = Ph + Pa,                        # συνολικά παιχνίδια
    Points = hwinvalue + awinvalue,         # συνολικοί πόντοι (εντός + εκτός)
    GoalsFor = FTHGh + FTAGa,               # γκολ υπέρ (εντός + εκτός)
    GoalsAgainst = FTAGh + FTHGa,           # γκολ κατά (εντός + εκτός)
    GoalDiff = GoalsFor - GoalsAgainst      # διαφορά τερμάτων
  ) %>%
  arrange(desc(Points), desc(GoalDiff), team) # ταξινόμηση βαθμολογίας

head(EPL2017_18)
View(EPL2017_18)

Engtotal <- Engtotal %>%
  mutate(
    wpc = Points / Games   # win percentage
  ) %>%
  arrange(desc(wpc), team)

head(Engtotal)

Engtotal <- Engtotal %>%
  mutate(
    PythExp = (GoalsFor^2) / (GoalsFor^2 + GoalsAgainst^2)
  )

Engtotal <- Engtotal %>%
  mutate(
    PythExp = (GoalsFor^2) / (GoalsFor^2 + GoalsAgainst^2),
    wpc = Points / Games
  )







library(ggplot2)

ggplot(Engtotal, aes(x = PythExp, y = wpc, label = team)) +
  geom_point(color = "blue", size = 3) +          # σημεία για κάθε ομάδα
  geom_text(vjust = -0.5, size = 3) +             # ετικέτες ομάδων
  geom_abline(intercept = 0, slope = 1,           # γραμμή y=x
              color = "red", linetype = "dashed") +
  labs(
    title = "Win Percentage vs Pythagorean Expectation (EPL 2017-18)",
    x = "Pythagorean Expectation",
    y = "Win Percentage"
  ) +
  theme_minimal()
