library(R6)
Toad <- R6Class("toad",
                # Define attributes
                public = list(
                  pos=NULL,
                  energy=NULL,
                  id=NULL,
                  initialize = function(pos, energy, id) {
                    self$pos <- pos
                    self$energy <- energy
                    self$id <- id
                  },
                  # Define behaviours
                  move = function() {
                    indices <- adjacent(australia_raster, cell = self$pos, directions = 8, pairs=TRUE)[,2]
                    indices <- c(indices, self$pos) # makes staying still an option
                    neighboring_cells <- (australia_raster - fixed_cells)[indices] #
                    names(neighboring_cells) <- indices
                    move_options <- as.numeric(names(neighboring_cells[which(neighboring_cells == 1)])) # list of cells that are still on land
                    
                    if (length(move_options) > 0) {
                      self$pos <- sample(move_options, 1)
                    }
                  },
                  
                  update_energy = function() {
                    self$energy <- self$energy + max_abs_delta_energy*affinities[self$pos]
                    if (self$energy > 1) {self$energy <- 1} # don't change if energy < 0, since then we want toad to die
                  },
                  die = function() {
                    remove_toad <- \(any_toad) any_toad$id != self$id
                    alive_toads <- Filter(remove_toad, alive_toads)
                  }
                )
)

update_tracker<-function(tracker, toad_list) {
  tracker[]<-0
  tracker[australia_raster == 0]<-NA
  for (toad in toad_list) {
    tracker[toad$pos]<-tracker[toad$pos] + 1
  }
  return(tracker)
}

flatten<-function(tracker_raster) { # for graphing
  flattened_tracker<-tracker_raster
  flattened_tracker[]<-0
  flattened_tracker[tracker_raster > 1]<-1
  flattened_tracker[is.na(tracker_raster)]<-NA
  return(flattened_tracker) 
}

speed<-\(long) 1+2*(long < 138)

is_fixed<-function(cell, max_toads) {
  any(!is.na(cell)) && all(cell >= max_toads | is.na(cell))
}

remove_excess<-function(toads_list, max_toads=6) {
  pos_counts<-table(sapply(toads_list, function(t) t$pos))
  filtered_toads<-list()
  
  for (pos in names(pos_counts)) {
    toads_for_pos<-toads_list[sapply(toads_list, function(t) t$pos == pos)][1:min(max_toads, pos_counts[pos])]
    filtered_toads<-c(filtered_toads, toads_for_pos)
  }
  return(filtered_toads)
}