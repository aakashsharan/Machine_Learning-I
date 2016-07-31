library(ggplot2)
library(scales)

# read teams data
teams <- read.csv('team.csv')
summary(teams$year)

#data set for moneyball
moneyball = subset(teams, year >= 1985 & year < 2002)
summary(moneyball)
sort(names(moneyball))

# creating a new variable rd, run difference
moneyball$rd = moneyball$r - moneyball$ra
#relationship between wins and run difference.
ggplot(data = moneyball) + geom_smooth(mapping = aes(x = w, y = rd), color="#000099") + ggtitle('Relationship b/w Wins and Run Difference') + labs(x="Wins", y="Runs Difference")
#plot(moneyball$w, moneyball$rd)

#linear regression model for w and rd.
wins_reg = lm(w ~ rd, data = moneyball)
summary(wins_reg)

#W = 78.957265 + (0.102115 * rd)
# 91.75 was playoff cutoff.

runs_reg1 = lm(r ~ ab + h + double + triple + hr + bb + dp + sb + cs + so, data = moneyball)
summary(runs_reg1)

# we will use this model for runs scored.
runs_reg2 = lm(r ~ ab + h + double + triple + hr + bb, data = moneyball)
summary(runs_reg2)

# runs
# r = 5.448891 + (-0.104795 * ab) + (0.626636 * h) + (0.195651 * double) + (0.694044 * triple) + (0.899972 * hr) + (0.356295 * bb)

# r = 5.448891 + (-0.104795 * ab) + (0.626636 * h) + (0.195651 * double) + (0.694044 * triple) + (0.899972 * hr) + (0.356295 * bb)

# runs scored
5.448891 + (-0.104795 * 5558) + (0.626636 * 1450) + (0.195651 * 279) + (0.694044 * 28) + (0.899972 * 205) + (0.356295 * 609)

runs_reg3 = lm(r ~ ab + h + double + triple + hr + bb + sb + cs, data = moneyball)
summary(runs_reg3)

# r = 5.177834 + (-0.105735 * ab) + (0.621239 * h) + (0.207906 * double) + (0.544204 * triple) + (0.944925* hr) + (0.341597 * bb) + (0.262176 * sb) + (-0.274041 * cs)

# runs scored using reg3
5.177834 + (-0.105735 * 5558) + (0.621239 * 1450) + (0.207906 * 279) + (0.544204 * 28) + (0.944925* 205) + (0.341597 * 609) + (0.262176 * 46) + (-0.274041 * 20)


pitching = read.csv('pitching.csv')
moneyball_pitching = subset(pitching, year >= 1985 & year < 2002)
summary(moneyball_pitching)
sort(names(moneyball_pitching))



runs_allowed1 = lm(r ~ sho + sv + ipouts + h + er + hr + bb + so + baopp + era + ibb + wp + hbp + bk, data = moneyball_pitching)
summary(runs_allowed1)


runs_allowed2 = lm(r ~ sho + sv + ipouts + h + er + hr + bb + so + wp + hbp + bk, data = moneyball_pitching)
summary(runs_allowed2)

runs_allowed3 = lm(r ~ sv + ipouts + h + er + hr + bb + so + wp + hbp + bk, data = moneyball_pitching)
summary(runs_allowed3)

# runs allowed 2
# ra = 0.029469 + (-0.078456 * sho) + (-0.015500 * sv) + (-0.010860 * ipouts) + (0.101464 * h) + (0.896016 * er) + (0.025095 * hr) + (0.061453 * bb) + (-0.006962 * so) + (0.074793 * wp) + (0.077320 * hbp) + (0.148641 * bk)

#0.029469 + (-0.078456 * 19) + (-0.015500 * 48) + (-0.010860 * 4356) + (0.101464 * 1450) + (0.896016 * 593) + (0.025095 * 205) + (0.061453 * 609) + (-0.006962 * 1008) + (0.074793 * wp) + (0.077320 * 68) + (0.148641 * bk)

0.029469 + (-0.078456 * 19) + (-0.015500 * 48) + (-0.010860 * 4356) + (0.101464 * 1450) + (0.896016 * 593) + (0.025095 * 205) + (0.061453 * 609) + (-0.006962 * 1008) + (0.077320 * 68)

0.029469 + (-0.078456 * 19) + (-0.015500 * 48) + (-0.010860 * 4356) + (0.101464 * 1450) + (0.896016 * 593) + (0.025095 * 205) + (0.061453 * 609) + (-0.006962 * 1008) + (0.074793 * 2.353) + (0.077320 * 68) + (0.148641 * 0.5294)

#runs allowed 3
# r = 0.036056 + (-0.014268 * sv) + (-0.011360 * ipouts) + (0.102181 * h) + (0.896016 * er) + (0.026607 * hr) + (0.062142 * bb) + (-0.007035 * so) + (0.075391 * wp) + (0.078287 * hbp) + (0.149150 * bk)

0.036056 + (-0.014268 * 48) + (-0.011360 * 4356) + (0.102181 * 1450) + (0.896016 * 593) + (0.026607 * 205) + (0.062142 * 609) + (-0.007035 * 1008) + (0.078287 * 68)

0.036056 + (-0.014268 * 48) + (-0.011360 * 4356) + (0.102181 * 1450) + (0.896016 * 593) + (0.026607 * 205) + (0.062142 * 609) + (-0.007035 * 1008) + (0.075391 * 2.353) + (0.078287 * 68) + (0.149150 * 0.5294)

# according to team table
r = 0.036056 + (-0.014268 * sv) + (-0.011360 * ipouts) + (0.102181 * h) + (0.896016 * er) + (0.026607 * hr) + (0.062142 * bb) + (-0.007035 * so) + (0.075391 * wp) + (0.078287 * hbp) + (0.149150 * bk)


test_data = subset(teams, year >= 1985 & year < 2002)
sort(names(test_data))
test_data$obp = (test_data$h + test_data$hbp + test_data$bb)/(test_data$ab + test_data$bb + test_data$hbp + test_data$sf)
summary(test_data)

test_data$slg = ((test_data$bb) + (2 * test_data$double) + (3 * test_data$triple) + (4 * test_data$hr))/(test_data$ab)
summary(test_data)

test_data$ba = (test_data$h)/(test_data$ab)


ggplot(data = test_data) + geom_smooth(mapping = aes(x = w, y = obp), color="#000099") + ggtitle('Relationship b/w Wins and OBP in 2002') + labs(x="OBP", y="Wins")

ggplot(data = test_data) + geom_smooth(mapping = aes(x = slg, y = w), color="#000099") + ggtitle('Relationship b/w Wins and SLG in 2002') + labs(x="SLG", y="Wins")

ggplot(data = test_data) + geom_smooth(mapping = aes(x = ba, y = w), color="#000099") + ggtitle('Relationship b/w Wins and BA in 2002') + labs(x="BA", y="Wins")

runs_reg1 = lm(r ~ ab + h + double + triple + hr + bb + dp + sb + cs + so, data = moneyball)
summary(runs_reg1)


# comparision of obp slg and ba
ggplot(test_data, aes(w)) + geom_smooth(aes(y = obp,  color="obp")) + geom_smooth(aes(y = slg,  color="slg")) + geom_smooth(aes(y = ba,  color="ba")) + labs(x="Wins", y="%")



get_wins = teams
get_wins$win_ratio = get_wins$w/get_wins$g
summary(get_wins)
names(get_wins)
get_oaks = subset(get_wins, team_id == 'OAK' & year >= 2002 & year < 2015)
ggplot(data = get_oaks) + geom_line(mapping = aes(x = year, y = win_ratio), color="#000099") + ggtitle("OAKLAND A's performance during 2002-2015") + labs(x="year", y="win-ratio") + scale_x_continuous(breaks = round(seq(min(get_oaks$year), max(get_oaks$year), by = 1),1))