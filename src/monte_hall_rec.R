monte_hall_rec <- function(switch_door, n_sim=500) {
    # instantiate some data structures
    doors <- c(0, 0, 1)
    results <- c()
    # declare subfunction to run recursively
    play_game <- function(switch_door, results, n_sim, i=0) {
        if (i == n_sim) {
            return(results) # stop condition
        }
        else {
            doors <- sample(doors) # shuffle the doors
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
        i <- i + 1
        play_game(switch_door, results, n_sim, i) # recursive call
        }
    }
    results <- play_game(switch_door, results, n_sim)
    return(mean(results))
}