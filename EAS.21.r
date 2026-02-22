# Set WD
setwd("G:/RAS.Directory/Workspaces") # setwd("C:/Users/woote/Downloads")

# List of required packages
required_packages <- c(
  "dplyr", "igraph", "sf", "purrr", "ggplot2", 
  "tidyr", "segregation", "data.table", "parallel", "reshape2"
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  installed <- packages %in% installed.packages()[, "Package"]
  if (any(!installed)) {
    install.packages(packages[!installed])
  }
}

# Install missing packages
install_if_missing(required_packages)

# Load all packages
lapply(required_packages, library, character.only = TRUE)

# Load Data
load("ACO_21_workspace.rda") 

# Parameters (Set fixed values)
max_district_size <- 124000 
num_ants <- 1200 
num_districts <- 21
num_iterations <- 400
demographic_scale <- 45 # was 15
compactness_scale <- 120 # was 30
penalty_contiguity <- -30 # was 12
bonus_contiguity <- 60 # was 20
ant_cut_score <- -60000
size_scale_factor <- log(10) ## i.e., 1 (moot)


# Supporting functions
# Contiguity Check (No changes needed)
is_contiguous <- function(nodes, adj_matrix) {
  subgraph <- adj_matrix[nodes, nodes]
  components(graph_from_adjacency_matrix(subgraph, mode = "undirected"))$no == 1
}

# Ensure adj_matrix has dimnames
if (is.null(dimnames(adj_matrix))) {
  dimnames(adj_matrix) <- list(1:nrow(adj_matrix), 1:ncol(adj_matrix))
}

# Transform sf.dist to UTM Zone 18N (EPSG:32618) for accurate spatial calculations
sf.dist <- st_transform(sf.dist, crs = 32618) # Replace 32618 with the appropriate CRS for your region

# Polsby-Popper Compactness Function
calculate_polsby_popper <- function(district_geometry) {
  district_area <- st_area(district_geometry)
  district_perimeter <- st_length(st_boundary(district_geometry))
  circle_area <- (district_perimeter / (2 * pi))^2 * pi
  compactness <- district_area / circle_area
  return(as.numeric(compactness)) # Ensure it's numeric
}

# Corrected create_districts_result function (No changes needed)
create_districts_result <- function(best_assignment, sf.dist) {
  # Return empty data.table if all assignments are NA or input is invalid
  if (is.null(best_assignment) || all(is.na(best_assignment))) return(data.table::data.table())
  # Only consider non-NA assignments
  valid_assignments <- best_assignment[!is.na(best_assignment)]
  if (length(valid_assignments) == 0) return(data.table::data.table())
  num_districts <- max(valid_assignments)
  # Check required columns
  required_cols <- c("county", "district", "wa", "hb", "total")
  missing_cols <- setdiff(required_cols, colnames(sf.dist))
  if (length(missing_cols) > 0) stop(paste("Missing columns in sf.dist:", paste(missing_cols, collapse = ", ")))
  demographic_data <- lapply(1:num_districts, function(i) {
    district_nodes <- which(best_assignment == i)
    if (length(district_nodes) == 0) return(NULL) # Skip empty districts
    district_data <- sf.dist[district_nodes, required_cols, drop = FALSE]
    district_data$cluster <- i
    # Remove geometry column if present
    if ("geometry" %in% colnames(district_data)) st_geometry(district_data) <- NULL
    return(district_data)
  })
  demographic_data <- demographic_data[!sapply(demographic_data, is.null)] # Remove NULLs
  if (length(demographic_data) == 0) return(data.table::data.table())
  demographic_data <- data.table::rbindlist(demographic_data)
  return(demographic_data)
}

# Core functions
# Updated calculate_score function
calculate_score <- function(assignment, graph, sf.dist, penalty_contiguity, bonus_contiguity, compactness_scale, demographic_scale, iteration) {
  num_districts <- max(assignment, na.rm = TRUE)
  if (num_districts == -Inf) {
    return(NA)
  }
  score <- 0
  cat("Assignment received (first 20):", head(assignment, 20), "\n")
  
  # Build cluster assignment data
  districts_result <- create_districts_result(assignment, sf.dist)
  if (nrow(districts_result) == 0) {
    return(NA)
  }
  all_district_data_long <- reshape2::melt(
    districts_result,
    id.vars = c("cluster", "district", "county"),
    measure.vars = c("wa", "hb"),
    variable.name = "race",
    value.name = "population"
  )
  
  # Only compute H if all clusters are present
  if (length(unique(all_district_data_long$cluster)) == num_districts) {
    h.cluster <- segregation::mutual_total(all_district_data_long, "race", "cluster", weight = "population")
    if (length(h.cluster) > 0) {
      demographic_difference <- h.cluster[2, 2] # Thiel's H
    } else {
      demographic_difference <- NA
    }
  } else {
    demographic_difference <- NA
  }
  
  # Per-district calculations
  for (i in 1:num_districts) {
    district_nodes <- which(assignment == i)
    if (length(district_nodes) == 0) {
      return(NA)
    }
    
    # Check contiguity and apply penalty if violated
    if (!is_contiguous(district_nodes, graph)) {
      score <- score + penalty_contiguity
    }
    else {
      score <- score + bonus_contiguity
    }
    
    # Check max district size and apply penalty if violated
    district_size <- sum(sf.dist$total[district_nodes])
    if (district_size > max_district_size) {
      penalty <- ((-1) * ((district_size - max_district_size) / 1000)) # Down from -1* (higher ABS)
      score <- score + penalty
    }
    
    # Calculate Polsby-Popper compactness
    district_geometry <- st_union(sf.dist[district_nodes, "geometry"])
    compactness <- calculate_polsby_popper(district_geometry)
    score <- score + (compactness * compactness_scale)
  }
  
  # ...use demographic_difference in your score...
  demographic_difference <- 10 - (demographic_difference * 100)
  if (any(is.na(demographic_difference))) {
    return(NA)
  }
  
  # Add demographic difference to the score ONCE
  score <- score + (demographic_difference * demographic_scale)
  return(score)
}

# Modify process_ant function
process_ant <- function(
    graph, adj_matrix, num_districts, alpha, beta, rho, df.dist_df, max_district_size, 
    compactness_scale, demographic_scale, seed_districts, iteration, search_radius
) {
  num_nodes <- nrow(adj_matrix)
  assignment <- rep(NA, num_nodes) # Initialize assignment
  unassigned_nodes <- 1:num_nodes
  node_degrees <- rowSums(adj_matrix)
  #pheromone <- matrix(1, nrow = num_nodes, ncol = num_nodes)
  cluster_sizes <- rep(0, num_districts)
  
  for (district_index in 1:num_districts) {
    if (length(unassigned_nodes) == 0) break
    
    # Use the seed districts for initialization
    if (district_index <= length(seed_districts)) {
      initial_node <- seed_districts[district_index]
    } else {
      initial_node <- sample(unassigned_nodes, 1) # Fallback to random if fewer seeds
    }
    
    assignment[initial_node] <- district_index
    unassigned_nodes <- setdiff(unassigned_nodes, initial_node)
    district_nodes <- initial_node
    iteration_count <- 0
    
    # Track cluster size
    cluster_sizes[district_index] <- df.dist_df$total[initial_node]
    
    # Get the centerpoint of the seed district
    seed_x <- sf.dist$center_x[initial_node]
    seed_y <- sf.dist$center_y[initial_node]
    
    # Calculate distances from the seed to all other districts
    distances <- sqrt((sf.dist$center_x - seed_x)^2 + (sf.dist$center_y - seed_y)^2)
    
    # Only consider unassigned nodes within the radius
    eligible_nodes <- unassigned_nodes[distances[unassigned_nodes] <= search_radius]
    
    while (length(eligible_nodes) > 0) {
      iteration_count <- iteration_count + 1
      # Find neighbors of the current district
      neighbors <- which(colSums(adj_matrix[district_nodes, , drop = FALSE]) > 0)
      neighbors <- setdiff(neighbors, district_nodes)
      available_neighbors <- intersect(neighbors, eligible_nodes)
      
      # Only keep neighbors that would NOT exceed max_district_size if added
      available_neighbors <- available_neighbors[
        (cluster_sizes[district_index] + df.dist_df$total[available_neighbors]) <= max_district_size
      ]
      
      if (length(available_neighbors) == 0) break
      
      probabilities <- (pheromone[district_nodes[1], available_neighbors]^alpha) * (node_degrees[available_neighbors]^beta)
      if (length(available_neighbors) > 1) {
        if (sum(probabilities) == 0) probabilities <- rep(1 / length(probabilities), length(probabilities))
        else probabilities <- probabilities / sum(probabilities)
        next_node <- sample(available_neighbors, 1, prob = probabilities)
      } else {
        next_node <- available_neighbors[1]
      }
      temp_district <- c(district_nodes, next_node)
      
      assignment[next_node] <- district_index
      unassigned_nodes <- setdiff(unassigned_nodes, next_node)
      district_nodes <- c(district_nodes, next_node)
      cluster_sizes[district_index] <- cluster_sizes[district_index] + df.dist_df$total[next_node]  # <-- add this line
      
      # Update eligible_nodes for the next iteration
      eligible_nodes <- unassigned_nodes[distances[unassigned_nodes] <= search_radius]
      
      if (iteration_count > 500000) break 
    }
    
  }
  
  # Recycling: keep running the main ACO logic on unassigned nodes
  recycle_count <- 0
  max_recycles <- 500000 # or whatever value you want
  
  while (length(unassigned_nodes) > 0 && recycle_count < max_recycles) {
    recycle_count <- recycle_count + 1
    
    for (district_index in 1:num_districts) {
      # Only proceed if there are unassigned nodes left
      if (length(unassigned_nodes) == 0) break
      
      # Pick a new seed for this district from unassigned nodes
      # (could be random, or based on some heuristic)
      initial_node <- sample(unassigned_nodes, 1)
      assignment[initial_node] <- district_index
      unassigned_nodes <- setdiff(unassigned_nodes, initial_node)
      district_nodes <- initial_node
      iteration_count <- 0
      
      # Track cluster size
      cluster_sizes[district_index] <- cluster_sizes[district_index] + df.dist_df$total[initial_node]
      
      # Get the centerpoint of the seed district
      seed_x <- sf.dist$center_x[initial_node]
      seed_y <- sf.dist$center_y[initial_node]
      
      # Calculate distances from the seed to all other districts
      distances <- sqrt((sf.dist$center_x - seed_x)^2 + (sf.dist$center_y - seed_y)^2)
      
      # Only consider unassigned nodes within the radius
      eligible_nodes <- unassigned_nodes[distances[unassigned_nodes] <= search_radius]
      
      while (length(eligible_nodes) > 0) {
        iteration_count <- iteration_count + 1
        # Find neighbors of the current district
        neighbors <- which(colSums(adj_matrix[district_nodes, , drop = FALSE]) > 0)
        neighbors <- setdiff(neighbors, district_nodes)
        available_neighbors <- intersect(neighbors, eligible_nodes)
        
        # Only keep neighbors that would NOT exceed max_district_size if added
        available_neighbors <- available_neighbors[
          (cluster_sizes[district_index] + df.dist_df$total[available_neighbors]) <= max_district_size
        ]
        
        if (length(available_neighbors) == 0) break
        
        probabilities <- (pheromone[district_nodes[1], available_neighbors]^alpha) * (node_degrees[available_neighbors]^beta)
        if (length(available_neighbors) > 1) {
          if (sum(probabilities) == 0) probabilities <- rep(1 / length(probabilities), length(probabilities))
          else probabilities <- probabilities / sum(probabilities)
          next_node <- sample(available_neighbors, 1, prob = probabilities)
        } else {
          next_node <- available_neighbors[1]
        }
        temp_district <- c(district_nodes, next_node)
        
        assignment[next_node] <- district_index
        unassigned_nodes <- setdiff(unassigned_nodes, next_node)
        district_nodes <- c(district_nodes, next_node)
        cluster_sizes[district_index] <- cluster_sizes[district_index] + df.dist_df$total[next_node]
        
        # Update eligible_nodes for the next iteration
        eligible_nodes <- unassigned_nodes[distances[unassigned_nodes] <= search_radius]
        
        if (iteration_count > 500000) break
      }
    }
  }
  
  cat("Assignment in process_ant (head):", head(assignment), "\n")
  cat("Number of assigned nodes:", sum(!is.na(assignment)), "\n")
  return(assignment)
}

# ACO Districting Function 
aco_districting <- function(
    graph, adj_matrix, num_ants, num_districts, num_iterations, alpha, beta, rho,
    df.dist_df, sf.dist, max_district_size, penalty_contiguity, bonus_contiguity,
    compactness_scale, demographic_scale, size_scale_factor, ant_cut_score,
    search_radius,
    num_cores = detectCores() - 1, num_elite = 5 
) {
  num_nodes <- nrow(adj_matrix)
  best_assignment <- NULL
  best_score <- -Inf
  last_best_score <- -Inf
  demographic_history <- list()
  h_values <- numeric(num_iterations)
  score_history <- numeric(num_iterations)
  pheromone <- matrix(1, nrow = num_nodes, ncol = num_nodes) # Initialize pheromone matrix
  elite_assignments <- list() # Store elite solutions
  elite_scores <- rep(-Inf, num_elite) # Initialize elite scores to -Inf
  pre_elite_assignments <- list() # Store pre-elite solutions
  pre_elite_scores <- numeric() # Store pre-elite scores
  
  # --- Theoretical score bounds (update these as needed for your scoring logic) ---
  # These should reflect the max/min possible values for your scoring function
  score_min <- penalty_contiguity * num_districts + (0 * compactness_scale) + (0 * demographic_scale) # worst possible
  score_max <- bonus_contiguity * num_districts + (max_district_size * 0) + (10 * demographic_scale) + (1 * compactness_scale * num_districts) # best possible
  
  # Adaptive elite scheduling parameters
  n_elite <- num_elite # initial value
  n_elite_min <- 1
  n_elite_max <- 20
  elite_boost_factor <- 10
  gap_scaling_factor <- 20
  gap_baseline_low <- 0.2 ## Try 0.2 
  gap_baseline_high <- 0.25 ## Try 0.275
  
  # Create the cluster
  cl <- makeCluster(num_cores)
  
  # Export static variables and functions ONCE before the loop
  clusterExport(cl, c(
    "df.dist_df", "sf.dist", "process_ant", "calculate_score", "calculate_polsby_popper", "graph", "adj_matrix",
    "num_districts", "max_district_size", "penalty_contiguity", "bonus_contiguity", "compactness_scale", 
    "demographic_scale", "ant_cut_score", "is_contiguous", "st_area", "st_union", "st_drop_geometry", 
    "neighborhood", "induced_subgraph", "components", "sample", "degree", "vcount", "setdiff", "st_geometry", 
    "st_length", "melt", "rbindlist", "setDT", "graph_from_adjacency_matrix", "size_scale_factor", "create_districts_result"
  ))
  
  # Ensure required libraries are loaded in each worker
  clusterEvalQ(cl, library(igraph))
  clusterEvalQ(cl, library(sf))
  clusterEvalQ(cl, library(reshape2))
  
  # ---- ADD THESE LINES HERE ----
  alpha_start <- 0.8
  alpha_end <- 4.5
  beta_start <- 10
  beta_end <- 4.5
  rho_start <- 1
  rho_end <- 0.1
  search_radius_start <- 1250
  search_radius_end <- 12500 
  # -----------------------------
  
  iteration <- 1
  if (iteration == 1) {
    # Print Ant Score standardized once
    score_min <- penalty_contiguity * num_districts + (0 * compactness_scale) + (0 * demographic_scale)
    score_max <- bonus_contiguity * num_districts + (max_district_size * 0) + (10 * demographic_scale) + (1 * compactness_scale * num_districts)
    ant_cut_score_raw <- ant_cut_score
    ant_cut_score_std <- (ant_cut_score_raw - score_min) / (score_max - score_min)
    ant_cut_score_std <- max(0, min(1, ant_cut_score_std))
    cat("ANT CUT SCORE (STANDARDIZED):", sprintf("%.3f", ant_cut_score_std), "\n")
  }
  while (iteration <= num_iterations) {
    ## Modify Burn in
    burn_in <- as.integer(round(num_iterations / 4))
    if (iteration <= burn_in) {
      # Fixed values
      rho <- rho_start
      alpha <- alpha_start
      beta <- beta_start
      search_radius <- search_radius_start
    } else {
      # Log scale over remaining iterations
      scaled_iter <- (iteration - burn_in) / (num_iterations - burn_in)
      scale_fac <- log10(1 + 9 * scaled_iter)
      rho   <- rho_start   + ((rho_end   - rho_start)   * scale_fac)
      alpha <- alpha_start + ((alpha_end - alpha_start) * scale_fac)
      beta  <- beta_start  + ((beta_end  - beta_start)  * scale_fac)
      search_radius <- search_radius_start + ((search_radius_end - search_radius_start) * scale_fac)
    }
    
    # Randomly select seed districts for this iteration
    seed_districts <- sample(1:nrow(sf.dist), num_districts, replace = FALSE)

    # Export updated scalar params AND pheromone to workers 
    clusterExport(cl, c("rho", "alpha", "beta", "search_radius", "pheromone"), envir = environment())    
    
    # Run the ants in parallel with error handling
    ant_results <- parLapply(cl, 1:num_ants, function(ant) {
      assignment <- process_ant(graph, adj_matrix, num_districts, alpha, beta, rho, df.dist_df, max_district_size, 
                                compactness_scale, demographic_scale, 
                                seed_districts, iteration, search_radius)
      score <- calculate_score(assignment, graph, sf.dist, penalty_contiguity, bonus_contiguity, compactness_scale, 
                               demographic_scale, iteration)
      return(list(assignment = assignment, score = score, iteration = iteration))
    })

    # Export the updated pheromone matrix and current scalar params to workers (once per iteration)
    clusterExport(cl, c("pheromone"), envir = environment())
    
    # Process results
    ant_assignments <- lapply(ant_results, function(x) x$assignment)
    ant_scores <- unlist(lapply(ant_results, function(x) x$score))
    
    # Filter out NA scores
    valid_scores <- !is.na(ant_scores)
    ant_assignments <- ant_assignments[valid_scores]
    ant_scores <- ant_scores[valid_scores]
    
    # --- Standardize ant scores between 0 and 1 ---
    ant_scores_std <- (ant_scores - score_min) / (score_max - score_min)
    ant_scores_std[ant_scores_std < 0] <- 0
    ant_scores_std[ant_scores_std > 1] <- 1
    
    # --- Standardize elite scores ---
    elite_scores_std <- (elite_scores - score_min) / (score_max - score_min)
    elite_scores_std[elite_scores_std < 0] <- 0
    elite_scores_std[elite_scores_std > 1] <- 1
    
    # --- Adaptive scheduling: calculate gap and update n_elite only after burn-in ---
    if (iteration > (burn_in)) {
      if (length(elite_scores_std[!is.infinite(elite_scores_std)]) > 0 && length(ant_scores_std) > 0) {
        median_elite <- median(elite_scores_std[!is.infinite(elite_scores_std)], na.rm = TRUE)
        median_ants <- median(ant_scores_std, na.rm = TRUE)
        gap <- median_elite - median_ants
        # Logarithmic scaling for n_elite
        if (gap < gap_baseline_low) {
          n_elite <- min(n_elite_max, round(n_elite + (gap_baseline_high - gap) * elite_boost_factor))
        } else if (gap < gap_baseline_high) {
          n_elite <- min(n_elite_max, round(n_elite + (gap_baseline_high - gap) * elite_boost_factor))
        } else {
          n_elite <- max(n_elite_min, round(n_elite * exp(- (gap - gap_baseline_high) * gap_scaling_factor)))
        }
        n_elite <- min(n_elite_max, max(n_elite_min, n_elite))
        cat(sprintf("ADAPTIVE N_ELITE: %d (GAP=%.5f, MEDIAN_ELITE=%.5f, MEDIAN_ANTS=%.5f)\n", n_elite, gap, median_elite, median_ants))
        
        # If n_elite changes, keep only the top n_elite elite ants
        if (length(elite_scores) != n_elite) {
          if (length(elite_scores) > n_elite) {
            # Keep only the top n_elite elites
            keep_indices <- order(elite_scores, decreasing = TRUE)[1:n_elite]
            elite_scores <- elite_scores[keep_indices]
            elite_assignments <- elite_assignments[keep_indices]
            elite_scores_std <- elite_scores_std[keep_indices]
          } else if (length(elite_scores) < n_elite) {
            # Pad with -Inf and empty lists
            elite_scores <- c(elite_scores, rep(-Inf, n_elite - length(elite_scores)))
            elite_assignments <- c(elite_assignments, vector("list", n_elite - length(elite_assignments)))
            elite_scores_std <- c(elite_scores_std, rep(0, n_elite - length(elite_scores_std)))
          }
        }
        
        # Now, for new candidates, only replace if better than the worst elite
        if (length(ant_scores_std) >= n_elite) {
          elite_indices <- order(ant_scores_std, decreasing = TRUE)[1:n_elite]
          candidate_scores <- ant_scores[elite_indices]
          candidate_assignments <- ant_assignments[elite_indices]
          candidate_scores_std <- ant_scores_std[elite_indices]
          for (i in seq_along(candidate_scores)) {
            min_elite_score <- min(elite_scores, na.rm = TRUE)
            if (!is.na(candidate_scores[i]) && candidate_scores[i] > min_elite_score) {
              min_elite_index <- which.min(elite_scores)
              elite_scores[min_elite_index] <- candidate_scores[i]
              elite_assignments[[min_elite_index]] <- candidate_assignments[[i]]
              elite_scores_std[min_elite_index] <- candidate_scores_std[i]
            }
          }
        }
        cat(sprintf("elite_scores length: %d, elite_assignments length: %d\n", length(elite_scores), length(elite_assignments)))
      }
    } else {
      n_elite <- 5
    }
    
    cat(toupper(sprintf("Iteration %d Summary:\n", iteration)))
    # Range of Ant scores (standardized)
    cat("ANT RANGE (RAW):", sprintf("%.3f - %.3f", min(ant_scores, na.rm = TRUE), max(ant_scores, na.rm = TRUE)), "\n")
    cat("ANT RANGE (STANDARDIZED):", sprintf("%.3f - %.3f", min(ant_scores_std, na.rm = TRUE), max(ant_scores_std, na.rm = TRUE)), "\n")
    
    # If all ants are invalid (i.e., all scores are NA)
    if (length(ant_scores) == 0) {
      cat("All ant scores are NA, skipping iteration.\n")
      next
    }
    
    best_ant <- which.max(ant_scores)
    
    # Debugging: Check contents of elite_scores (raw and standardized)
    cat("Elite Scores before update (raw):\n")
    print(elite_scores)
    cat("Elite Scores before update (standardized):\n")
    print(elite_scores_std)
    
    # Update best solution
    if (!is.na(best_ant) && ant_scores[best_ant] > best_score) {
      best_assignment <- ant_assignments[[best_ant]]
      best_score <- ant_scores[best_ant]
    }
    
    
    
    # Store pre-elite ants during the first 1/4 iterations
    if (iteration <= (burn_in)) {
      for (i in 1:length(ant_scores)) {
        if (!is.na(ant_scores[i]) && ant_scores[i] > ant_cut_score) {
          pre_elite_assignments <- c(pre_elite_assignments, list(ant_assignments[[i]]))
          pre_elite_scores <- c(pre_elite_scores, ant_scores[i])
        }
      }
    }
    
    # Initialize elite ants using pre-elite ants at end of burn_in
    if (iteration == (burn_in)) {
      for (i in 1:length(pre_elite_scores)) {
        if (!is.na(pre_elite_scores[i]) && pre_elite_scores[i] > min(elite_scores, na.rm = TRUE)) {
          min_elite_index <- which.min(elite_scores)
          elite_scores[min_elite_index] <- pre_elite_scores[i]
          elite_assignments[[min_elite_index]] <- pre_elite_assignments[[i]]
          
          # Debugging: Log pre-elite score updates
          cat("Initialized elite score at index", min_elite_index, "with pre-elite score", pre_elite_scores[i], "\n")
        }
      }
    }
    
    cat("Elite Scores after update (raw):\n")
    print(elite_scores)
    cat("ELITE SCORES AFTER UPDATE (STANDARDIZED):\n")
    elite_scores_std <- (elite_scores - score_min) / (score_max - score_min)
    elite_scores_std[elite_scores_std < 0] <- 0
    elite_scores_std[elite_scores_std > 1] <- 1
    print(round(elite_scores_std, 5))
    districts_result <- create_districts_result(ant_assignments[[best_ant]], sf.dist)
    demographic_history[[iteration]] <- districts_result
    
    all_district_data <- demographic_history[[iteration]]
    setDT(all_district_data)
    
    
    all_district_data_long <- melt(all_district_data,
                                   id.vars = c("cluster", "district", "county"),
                                   measure.vars = c("wa", "hb"),
                                   variable.name = "race",
                                   value.name = "population"
    )
    
    
    # Check for empty districts before running segregation calculations
    if (length(unique(all_district_data_long$cluster)) == num_districts) {
      
      h.cluster <- mutual_total(all_district_data_long, "race", "cluster", weight = "population")
      if (length(h.cluster) > 0) {
        h_values[iteration] <- h.cluster[2, 2] # Added to fix the H_values error
        print(toupper(sprintf("ITERATION: %d H VALUE: %.5f", iteration, h_values[iteration])))
      } else {
        h_values[iteration] <- NA
        warning(paste("Entropy calculation failed at iteration", iteration, ". Setting H to NA."))
      }
    } else {
      h_values[iteration] <- NA
      warning(paste("Segregation calculations skipped at iteration", iteration, ". Empty Districts exist."))
    }
    
    
    score_history[iteration] <- best_score
    last_best_score <- best_score
    iteration <- iteration + 1
    
    # Global size pheromone adjustment
    size_slide_threshold <- 0.935 * max_district_size #down from 0.995
    
    # Pheromone Update (Corrected and Logarithmically Scaled)
    pheromone <- (1 - rho) * pheromone # Evaporation
    pheromone[pheromone < 0] <- 0 # Ensure no negative pheromone values
    
    # Regular Pheromone Update
    if (!is.nan(best_score) && best_score > 0) {
      for (i in 1:(num_nodes - 1)) {
        for (j in (i + 1):num_nodes) {
          if (!is.na(best_assignment[i]) && !is.na(best_assignment[j]) && best_assignment[i] == best_assignment[j] && best_assignment[i] != 0) {
            scaled_score <- log(best_score + 1)
            pheromone[i, j] <- pheromone[i, j] + scaled_score
            pheromone[j, i] <- pheromone[i, j]
          }
        }
      }
    }
    
    # Size-based pheromone adjustment (with sliding scale)
    for (i in 1:num_districts) {
      district_nodes <- which(best_assignment == i)
      df.dist_no_geometry <- st_drop_geometry(sf.dist)
      df.dist_no_geometry$total <- as.numeric(df.dist_no_geometry$total)
      district_size <- sum(df.dist_no_geometry[district_nodes, "total"])
      size_factor <- if (district_size > size_slide_threshold) {
        min(1, (((district_size - size_slide_threshold) * size_scale_factor) / (max_district_size - size_slide_threshold)))
      } else {
        0
      }
      if (district_size > max_district_size) {
        rho <- 1 # Set evaporation rate to maximum if size exceeds max_district_size
      } else {
        rho <- min(1, rho + size_factor) # Increase evaporation rate
      }
    }
    
    # Elitist Pheromone Update (only after burn-in period)
    if (iteration > (burn_in)) {
      for (elite_assignment in elite_assignments) {
        if (!is.null(elite_assignment)) {
          for (i in 1:(num_nodes - 1)) {
            for (j in (i + 1):num_nodes) {
              if (!is.na(elite_assignment[i]) && !is.na(elite_assignment[j]) && elite_assignment[i] == elite_assignment[j] && elite_assignment[i] != 0) {
                elite_score_idx <- which(sapply(elite_assignments, identical, elite_assignment))
                elite_score <- elite_scores[elite_score_idx[1]] # Always use first match (scalar)
                scaled_elite_score <- ifelse(is.nan(elite_score) || elite_score <= 0, 0, log(elite_score + 1) * 1.5) # Boost elite pheromone
                pheromone[i, j] <- pheromone[i, j] + scaled_elite_score
                pheromone[j, i] <- pheromone[i, j]
              }
            }
          }
        }
      }
    }
    
    cat("Number of valid ants:", sum(valid_scores), "\n")
    cat("Number of invalid ants:", sum(!valid_scores), "\n")
    # Output best score so far (raw and standardized)
    best_score_std <- (best_score - score_min) / (score_max - score_min)
    best_score_std <- max(0, min(1, best_score_std))
    cat("Best score so far (raw):", best_score, "\n")
    cat("Best score so far (standardized):", sprintf("%.3f", best_score_std), "\n")
    cat("Number of unassigned nodes in best assignment:", sum(is.na(best_assignment)), "\n")
  }
  
  stopCluster(cl)
  return(list(best_assignment = best_assignment, score_history = score_history[1:(iteration - 1)], h_values = h_values[1:(iteration - 1)], demographic_history = demographic_history[1:(iteration - 1)], pre_elite_assignments = pre_elite_assignments, pre_elite_scores = pre_elite_scores))
}

# Measure execution time
start_time <- Sys.time()

# Run the ACO algorithm
aco_result <- aco_districting(
  graph, adj_matrix, num_ants, num_districts, num_iterations, alpha, beta, rho,
  df.dist_df, sf.dist, max_district_size, penalty_contiguity, bonus_contiguity,
  compactness_scale, demographic_scale, size_scale_factor, ant_cut_score,
  search_radius
)

end_time <- Sys.time()

# Calculate and print the elapsed time
elapsed_time <- end_time - start_time
cat("ACO algorithm execution time:", elapsed_time, "\n")

# Print or inspect the results
print("Best Assignment:")
print(aco_result$best_assignment)
print("Score History:")
print(aco_result$score_history)
print("Final Best Score:")
print(tail(aco_result$score_history, 1))

#Output
# Ensure sf.dist is an sf object
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

# Add the best assignment to sf.dist
sf.dist$cluster <- aco_result$best_assignment

# Plot the clusters
ggplot(data = sf.dist) +
  geom_sf(aes(fill = factor(cluster)), color = "black", size = 0.1) +
  scale_fill_viridis_d(name = "Cluster", option = "turbo", direction = 1) +
  theme_minimal() +
  labs(
    title = "Best Cluster Assignments",
    subtitle = "Visualized using ggplot2",
    caption = "Generated by ACO Algorithm"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )


# Reshape the data using pivot_longer
df_long <- sf.dist %>%
  select(cluster, district, county, wa, hb) %>% # Select relevant columns
  pivot_longer(
    cols = c(wa, hb),
    names_to = "race",
    values_to = "population"
  )

# View the reshaped data
head(df_long)

# Calculate segregation measures
library(segregation)
dissimilarity(df_long,"race","cluster",weight="population")
mutual_total(df_long,"race","cluster",weight="population")

#Cluster demo summary
library(dplyr)

# Aggregate data by cluster
cluster_summary <- sf.dist %>%
  st_drop_geometry() %>% # Drop the geometry column
  group_by(cluster) %>%
  summarize(
    sum_wa = sum(wa, na.rm = TRUE),
    sum_hb = sum(hb, na.rm = TRUE),
    sum_total = sum(total, na.rm = TRUE),
    wapct = sum_wa / sum_total * 100, # Percentage of wa
    hbpct = sum_hb / sum_total * 100  # Percentage of hb
  )

# View the cluster summary
print(cluster_summary)

library(dplyr)

# Cluster congituity, drop the geometry column before summarizing
clusters <- unique(sf.dist$cluster)
contiguity_results <- sapply(clusters, function(cl) {
  nodes <- which(sf.dist$cluster == cl)
  is_contiguous(nodes, adj_matrix)
})
names(contiguity_results) <- clusters
print(contiguity_results)

# Save the ACO Results
# Generate a dynamic filename with the current date and time
timestamp <- format(Sys.time(), "%m-%d_%H-%M") # Format: MM-DD_HH-MM
filename <- paste0("ACO.21.run", timestamp, ".rda") # Change prefix as needed

# Save the results (save the aco_result object)
save(aco_result, file = filename)

# Print the filename to confirm
cat("ACO run results saved to:", filename, "\n")
