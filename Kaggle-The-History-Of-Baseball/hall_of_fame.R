



hall_fame = read.csv('hall_of_fame.csv')
summary(hall_fame)

hall_fame_player = subset(hall_fame, category == 'Player')
summary(hall_fame_player)

hall_fame_player = na.omit(hall_fame_player)

players = hall_fame_player$player_id
players


player_data = read.csv('batting.csv')
names(player_data)
players_fame = subset(player_data, player_id == players)
