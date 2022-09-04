


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a zone generator and modulator list for an instrument
#' 
#' @param sf SoundFont object as read by \code{read_sf2()}
#' @param inst name or index of instrument
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_instrument <- function(sf, inst) {
  # Get instrument generators
  
  if (is.numeric(inst)) {
    inst <- sf$pdta$inst[inst,]
  } else {
    inst <- subset(sf$pdta$inst, sf$pdta$inst$name == inst)
  }
  
  if (nrow(inst) == 0) {
    stop("No instrument found")
  }
  
  inst
  ibag <- sf$pdta$ibag[1 + inst$bag_idx:inst$bag_end,]
  ibag
  
  igen <- list()
  for (i in seq.int(nrow(ibag))) {
    row <- ibag[i,]
    igen[[i]] <- sf$pdta$igen[1 + row$gen_idx:row$gen_end,]
  }
  igen
  
  first_gen_is_global <- !53 %in% igen[[1]]$oper
  if (first_gen_is_global) {
    global <- igen[[1]]
    igen <- igen[-1]
    
    # Merge global with zone generator
    for (i in seq_along(igen)) {
      # keep only things in global that aren't overridden local
      local <- igen[[i]]
      this_global <- global[!global$oper %in% local$oper,]
      igen[[i]] <- rbind(this_global, local)
    }
    
  }
  
  list(igen = igen)
  
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create instruments
#' 
#' Unpack the inst, ibag, imod and igen structures into a (slightly)
#' more interesting nested list structure.  
#'
#' Definitely a work in progress
#' 
#' @param sf SoundFont object as read by \code{read_sf2()}
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_instruments <- function(sf) {
  
  # Add 'shdr' sample names to 'inst'
  sf$pdta$igen$inst <- ifelse(sf$pdta$igen$oper == 53, 
                              sf$pdta$shdr$name[sf$pdta$igen$amount + 1],
                              "")
  
  insts <- sf$pdta$inst$name
  insts <- insts[-length(insts)] # Remove the EOI marker
  names(insts) <- insts
  lapply(insts, function(x) {
    create_instrument(sf, x)
  })
}


if (FALSE) {
  filename <- '/Users/mike/projectsdata/soundfonts/fluidr3_gm.sf2'
  
  sf <- read_sf2(filename)
  
  insts <- create_instruments(sf)
  
}












