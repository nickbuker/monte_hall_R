monte_hall <- function(switch_door, n_sim=10000) {
    doors <- c(0, 0, 1)
    results <- c()
    return(mean(play_game(switch_door, doors, results, n_sim)))
}

play_game <- function(switch_door, doors, results, n_sim) {
    for (sim in seq(n_sim)) {
        doors <- sample(doors)
        if (switch_door) {
            if (doors[2] == 0) {
                results <- c(results, doors[3])
            }
            else {
                results <- c(results, doors[2])
            }
        }
        else {
            results <- c(results, doors[1])
        }
    }
    return(results)
}
