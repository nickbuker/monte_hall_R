monte_hall <- function(switch_door, n_sim=10000) {
    doors <- c(0, 0, 1)
    results <- rep(0, n_sim)

    play_game <- function(n) {
        doors <- sample(doors)
        if (switch_door) {
            if (doors[2] == 0) {
                n <- doors[3]
            }
            else {
                n <- doors[2]
            }
        }
        else {
            n <- doors[1]
        }
        return (n)
    }

    results <- sapply(results, play_game)
    return(mean(results))
}
