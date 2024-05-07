library(baseballr)
library(dplyr)
library(ggplot2)

# Real

baseball_dates <- vector("list", 32234)
for(i in 716352:748585) {
  baseball_dates[[i-716351]] <- i
}

baseball_indi <- lapply(baseball_dates, mlb_pbp)

sac_initial <- bind_rows(baseball_indi)

# Test

sac_test_baseball_dates <- vector("list", 1000)
for(i in 716352:717351) {
  sac_test_baseball_dates[[i-716351]] <- i
}

sac_test_baseball_indi <- lapply(sac_test_baseball_dates, mlb_pbp)

sac_test_initial <- bind_rows(sac_test_baseball_indi)

# All Pitches

sac_test_all_pitch <- sac_test_initial %>%
  filter(result.eventType %in% c("single","double","triple","home_run","strikeout","field_out","field_error","force_out","grounded_into_double_play","walk","hit_by_pitch","intent_walk","sac_bunt","sac_fly") &
           isPitch == "TRUE" &
           details.code %in% c("B","*B","C","H","D","X","E","S","W","F","T") &
           details.type.description %in% c("Changeup","Curveball","Cutter","Four-Seam Fastball","Knuckle Curve","Sinker","Slider","Splitter","Sweeper")) %>%
  mutate(Slugging = if_else(result.eventType == "single", 1, 
                            if_else(result.eventType == "double", 2, 
                                    if_else(result.eventType == "triple", 3, 
                                            if_else(result.eventType == "home_run", 4, 0)))),
         AB = if_else(result.eventType %in% c("single","double","triple","home_run","strikeout","field_out","field_error","force_out","grounded_into_double_play"), 1, 0),
         OB = if_else(result.eventType %in% c("single","double","triple","home_run","walk","hit_by_pitch","intent_walk"), 1, 0),
         PA = if_else(result.eventType %in% c("single","double","triple","home_run","strikeout","field_out","field_error","force_out","grounded_into_double_play","walk","hit_by_pitch","intent_walk","sac_bunt","sac_fly"), 1, 0),
         SluggingPercent = Slugging/AB,
         AvgSlugging = if_else(SluggingPercent %in% c(0,1,2,3,4), SluggingPercent, sum(Slugging)/ sum(AB)),
         OBPercent = OB/PA,
         OPS = AvgSlugging+OBPercent,
         balls = if_else(last.pitch.of.ab == "true" & result.eventType %in% c("single","double","triple","home_run","field_out","field_error","force_out","grounded_into_double_play","sac_bunt","sac_fly","strikeout"), count.balls.start,
                         if_else(details.isStrike == "TRUE", count.balls.start, count.balls.start - 1)),
         strikes = if_else(last.pitch.of.ab == "true" & result.eventType %in% c("single","double","triple","home_run","field_out","field_error","force_out","grounded_into_double_play","sac_bunt","sac_fly","walk","hit_by_pitch","intent_walk"), count.strikes.start,
                           if_else(details.isBall == "TRUE", count.strikes.start, count.strikes.start - 1)),
         Count = paste(balls,"-",strikes),
         foul = if_else(details.call.code %in% c("F","T"), 1, 0))
       
# Overall Pitch Type, Spin, and Speed Distribution Graphs

sac_test_all_pitch2 <- sac_test_all_pitch %>%
  group_by(details.type.description) %>%
  summarize(number_of_pitches = n(),
            foul_rate = mean(foul))

ggplot(sac_test_all_pitch2, aes(x = details.type.description, y = number_of_pitches)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Pitch Type", y = "Number of Pitches", title = "Number of Pitches by Pitch Type") +
  geom_text(aes(label = number_of_pitches, vjust = -.8)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 7))

ggplot(sac_test_all_pitch, aes(x = pitchData.breaks.spinRate)) +
  geom_density(alpha = 0.5, aes(fill=details.type.description)) +
  labs(x = "Spin Rate", y = "Frequency",  fill = "Pitch Type", title = "Spin Rate by Pitch Type") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sac_test_all_pitch, aes(x = pitchData.startSpeed)) +
  geom_density(alpha = 0.5, aes(fill=details.type.description)) +
  labs(x = "Pitch Speed", y = "Frequency",  fill = "Pitch Type", title = "Pitch Speed by Pitch Type") +
  theme(plot.title = element_text(hjust = 0.5))

# Foul Ball Rate

table(sac_test_all_pitch2$details.type.description, sac_test_all_pitch2$foul_rate)

ggplot(sac_test_all_pitch2, aes(x = details.type.description, y = foul_rate)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Pitch Type", y = "Foul Ball Rate", title = "Foul Ball Rate by Pitch Type") +
  geom_text(aes(label = number_of_pitches, vjust = -.8)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 7))

# Data set for the last pitch of each at bat

sac_test_last_pitch <- sac_test_initial %>%
  filter(last.pitch.of.ab == "true" &
           result.eventType %in% c("single","double","triple","home_run","strikeout","field_out","field_error","force_out","grounded_into_double_play","walk","hit_by_pitch","intent_walk","sac_bunt","sac_fly") &
           isPitch == "TRUE" &
           details.type.description %in% c("Changeup","Curveball","Cutter","Four-Seam Fastball","Knuckle Curve","Sinker","Slider","Splitter","Sweeper")) %>%
  mutate(Slugging = if_else(result.eventType == "single", 1, 
                            if_else(result.eventType == "double", 2, 
                                    if_else(result.eventType == "triple", 3, 
                                            if_else(result.eventType == "home_run", 4, 0)))),
         AB = if_else(result.eventType %in% c("single","double","triple","home_run","strikeout","field_out","field_error","force_out","grounded_into_double_play"), 1, 0),
         OB = if_else(result.eventType %in% c("single","double","triple","home_run","walk","hit_by_pitch","intent_walk"), 1, 0),
         PA = if_else(result.eventType %in% c("single","double","triple","home_run","strikeout","field_out","field_error","force_out","grounded_into_double_play","walk","hit_by_pitch","intent_walk","sac_bunt","sac_fly"), 1, 0),
         SluggingPercent = Slugging/AB,
         AvgSlugging = if_else(SluggingPercent %in% c(0,1,2,3,4), SluggingPercent, sum(Slugging)/ sum(AB)),
         OBPercent = OB/PA,
         OPS = AvgSlugging+OBPercent)

# New data set for the last pitch to group by and add summary variables

sac_test_last_pitch2 <- sac_test_last_pitch %>%
  group_by(details.type.description) %>%
  summarize(ops = mean(OPS),
            number_of_pitches = n(),
            foul_rate = mean(foul))

# Pitch Count by Pitch Type on the Last Pitch of Each At Bat

ggplot(sac_test_last_pitch2, aes(x = details.type.description, y = number_of_pitches)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Pitch Type", y = "Number of Pitches", title = "Number of Pitches by Pitch Type on the Last Pitch of At Bats") +
  geom_text((aes(label = number_of_pitches, vjust = -.8))) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 7))

# OPS by Pitch Type

ggplot(sac_test_last_pitch2, aes(x = details.type.description, y = ops)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Pitch Type", y = "OPS", title = "OPS by Pitch Type") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 7))

# OPS by Spin Rate

sac_spin_ops_corr <- cor(sac_test_last_pitch$pitchData.breaks.spinRate, sac_test_last_pitch$OPS)
ggplot(sac_test_last_pitch, aes(x = pitchData.breaks.spinRate, y = OPS)) +
  geom_point(stat = "identity", fill = "darkred") +
  labs(x = "Spin Rate", y = "OPS", title = "OPS by Spin Rate") +
  geom_text((aes(label = number_of_pitches, vjust = -1))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = lm, col = "black") +
  geom_text(x = 40, y = 200, size = 6,
            label = paste0("Correlation = ", round(sac_spin_ops_corr, 2)))

sac_spin_ops_corr <- cor(sac_test_last_pitch$pitchData.breaks.spinRate, sac_test_last_pitch$OPS)
ggplot(sac_test_last_pitch, aes(x = pitchData.breaks.spinRate, y = OPS)) +
  geom_point(stat = "identity", fill = "darkred") +
  labs(x = "Spin Rate", y = "OPS", title = "OPS by Spin Rate") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = lm, col = "darkred")

ggplot(sac_test_last_pitch, aes(x = details.type.description, fill = OPS)) +
  geom_bar(position = "fill")

# OPS by Pitch Speed

sac_speed_ops_corr <- cor(sac_test_last_pitch$pitchData.startSpeed, sac_test_last_pitch$OPS)
ggplot(sac_test_last_pitch, aes(x = pitchData.startSpeed, y = OPS)) +
  geom_point(stat = "identity", fill = "darkred") +
  labs(x = "Pitch Speed", y = "OPS", title = "OPS by Pitch Rate") +
  geom_text((aes(label = number_of_pitches, vjust = -1))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = lm, col = "black") +
  geom_text(x = 40, y = 200, size = 6,
            label = paste0("Correlation = ", round(sac_speed_ops_corr, 2)))

ggplot(sac_test_last_pitch, aes(x = pitchData.startSpeed, y = OPS)) +
  geom_point(stat = "identity", fill = "darkred") +
  labs(x = "Pitch Speed", y = "OPS", title = "OPS by Pitch Speed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = lm, col = "darkred")
