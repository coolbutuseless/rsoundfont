
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read from file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint32 <- function(con, n = 1) readBin(con, 'integer', n, size=4, endian = 'little')                 # DWORD. unsigned! TODO: Mayb read as float?
read_uint16 <- function(con, n = 1) readBin(con, 'integer', n, size=2, endian = 'little', signed = FALSE) # WORD
read_sint16 <- function(con, n = 1) readBin(con, 'integer', n, size=2, endian = 'little', signed = TRUE)  # SHORT (signed 16 bits)
read_uint8  <- function(con, n = 1) readBin(con, 'integer', n, size=1, endian = 'little', signed = FALSE) # BYTE
read_sint8  <- function(con, n = 1) readBin(con, 'integer', n, size=1, endian = 'little', signed = TRUE)  # CHAR (signed 8bits

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Avoid warnings about embedded NULLs by reading as raw bytes
# and doing my own converstion.
# Bytes are truncated at first NULL value if necessary.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_char  <- function(con, n = 1) {
  # suppressWarnings(readChar(con, n))
  bytes <- readBin(con, 'raw', n)
  first_null <- which(as.integer(bytes) == 0)
  if (length(first_null) == 0) {
    rawToChar(bytes)
  } else {
    first_null <- first_null[1]
    rawToChar(bytes[seq_len(first_null -1)])
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# These are actually 16bit ENUMS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_sfmodulator  <- read_uint16
read_sfgenerator  <- read_uint16
read_sfsamplelink <- read_uint16

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is a C UNION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_genamounttype <- read_uint16

# RIFF [size] sfbk
#   INFO
#   LIST [size] sdta
#     smpl - upper 16bits
#     sm24 - lower 8 bits
#   LIST [size] pdta
#     phdr - preset headers
#     pbag - preset index list
#     pmod - preset modulator list
#     pgen - preset generator list
#     inst - instrument names and indices
#     ibag - instrument index list
#     imod - instrument modulator list
#     igen - instrument generator list
#     shdr - sample headers


# TODO Advance 1 byte if size is ODD



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'phdr' preset header information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_phdr <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      name    = trimws(read_char(con, 20)),
      preset  = read_uint16(con),
      bank    = read_uint16(con),
      bag_idx = read_uint16(con),
      library = read_uint32(con),
      genre   = read_uint32(con),
      morpho  = read_uint32(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'phdr'
  len  <- 38
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  # Set names
  nms <- vapply(res, function(x) {x$name}, character(1))
  names(res) <- nms
  
  
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'pbag' preset bag
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_pbag <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      gen_idx  = read_uint16(con),
      mod_idx  = read_uint16(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'pbag'
  len  <- 4
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'pmod' preset modulator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_pmod <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      mod_src_oper       = read_sfmodulator(con),
      mod_dst_oper       = read_sfgenerator(con),
      amount             = read_sint16(con),
      mod_amt_src_oper   = read_sfmodulator(con),
      mod_amt_trans_oper = read_sfgenerator(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'pmod'
  len  <- 10
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'pgen'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_pgen <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      oper   = read_sfgenerator(con),
      amount = read_genamounttype(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'pgen'
  len  <- 4
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'inst' 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_inst <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      name    = trimws(read_char(con, 20)),
      bag_idx = read_uint16(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'inst'
  len  <- 22
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  # Set names
  nms <- vapply(res, function(x) {x$name}, character(1))
  names(res) <- nms
  
  
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'ibag' 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_ibag <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      gen_idx  = read_uint16(con),
      mod_idx  = read_uint16(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'ibag'
  len  <- 4
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'imod'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_imod <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      src_oper       = read_sfmodulator(con),
      dst_oper       = read_sfgenerator(con),
      amount         = read_sint16(con),
      amt_src_oper   = read_sfmodulator(con),
      amt_trans_oper = read_sfgenerator(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'imod'
  len  <- 10
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'igen'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_igen <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      oper   = read_sfgenerator(con),
      amount = read_genamounttype(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'igen'
  len  <- 4
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'shdr'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_shdr <- function(con) {
  
  # Parse a single instance of this chunk
  parse_single <- function(...) {
    list(
      name       = read_char(con, 20),
      start      = read_uint32(con),
      end        = read_uint32(con),
      loop_start = read_uint32(con),
      loop_end   = read_uint32(con),
      rate       = read_uint32(con),
      key        = read_uint8(con),
      correction = read_sint8(con),
      link       = read_uint16(con),
      type       = read_sfsamplelink(con)
    )
  }
  
  # Chunk name and length  
  nm   <- 'shdr'
  len  <- 46
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Parse all
  N   <- size / len
  res <- lapply(seq_len(N), parse_single)
  
  # Set names
  nms <- vapply(res, function(x) {x$name}, character(1))
  names(res) <- nms
  
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse the PRESETS and INSTRUMENTS
#
# According to the specification, all these chunks
#  - must appear
#  - must appear in this order
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_pdta <- function(con) {
  magic <- readChar(con, 4) #LIST
  stopifnot(magic == 'LIST')
  size <- read_uint32(con)
  
  magic <- readChar(con, 4) #pdta 
  stopifnot(magic == 'pdta')
  
  magic <- readChar(con, 4) #phdr 
  stopifnot(magic == 'phdr')
  phdr <- parse_phdr(con)
  
  magic <- readChar(con, 4) #pbag
  stopifnot(magic == 'pbag')
  pbag <- parse_pbag(con)
  
  magic <- readChar(con, 4) #pmod
  stopifnot(magic == 'pmod')
  pmod <- parse_pmod(con)
  
  magic <- readChar(con, 4) #pgen
  stopifnot(magic == 'pgen')
  pgen <- parse_pgen(con)
  
  magic <- readChar(con, 4) #inst
  stopifnot(magic == 'inst')
  inst <- parse_inst(con)
  
  magic <- readChar(con, 4) #ibag
  stopifnot(magic == 'ibag')
  ibag <- parse_ibag(con)
  
  magic <- readChar(con, 4) #imod
  stopifnot(magic == 'imod')
  imod <- parse_imod(con)
  
  magic <- readChar(con, 4) #igen
  stopifnot(magic == 'igen')
  igen <- parse_igen(con)
  
  magic <- readChar(con, 4) #shdr
  stopifnot(magic == 'shdr')
  shdr <- parse_shdr(con)
  # size <- read_uint32(con)
  # shdr <- readBin(con, 'raw', size)
  
  
  list(
    phdr =  phdr, 
    pbag =  pbag, 
    pmod =  pmod, 
    pgen =  pgen, 
    inst =  inst, 
    ibag =  ibag, 
    imod =  imod, 
    igen =  igen, 
    shdr =  shdr 
  )
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse the INFO block at the start
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_sfbk <- function(con) {
  
  magic <- readChar(con, 4)  # RIFF
  stopifnot(magic == 'RIFF')
  size <- read_uint32(con)
  magic <- readChar(con, 4)  # sfbk
  stopifnot(magic == 'sfbk')
  magic <- readChar(con, 4)  # LIST
  stopifnot(magic == 'LIST')
  size <- read_uint32(con)
  
  magic <- read_char(con, 4)  # INFO
  size <- size - 4
  
  stopifnot(magic == 'INFO')
  
  sfbk <- list()
  
  while (size > 0) {
    magic <- readChar(con, 4); size <- size - 4
    # cat(magic, "\n")
    if (length(magic) == 0) break;
    this_size <- read_uint32(con)
    size <- size - 4
    size <- size - this_size
    if (magic == 'ifil') {
      major <- read_uint16(con)
      minor <- read_uint16(con)
      this_res <- list(list(major = major, minor = minor))
      names(this_res) <- magic
    } else {
      this_res <- read_char(con, this_size)
      this_res <- list(this_res)
      names(this_res) <- magic
    }
    sfbk <- append(sfbk, this_res)
  }
  
  sfbk
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse the raw sound data from the connection
#' 
#' Data is a sequence of 2-byte word values.   For ease of use, these 
#' values are rescaled into the range [-1, 1]
#' 
#' @param con connection 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_sdta <- function(con) {
  magic <- readChar(con, 4) #LIST
  stopifnot(magic == 'LIST')
  size <- read_uint32(con)
  
  magic <- readChar(con, 4) #sdta
  stopifnot(magic == 'sdta')
  magic <- readChar(con, 4) #smpl
  stopifnot(magic == 'smpl')
  size <- read_uint32(con)
  
  sdta <- read_sint16(con, n = size/2) 
  sdta[sdta < 0] <- sdta[sdta < 0] / 32767
  sdta[sdta > 0] <- sdta[sdta > 0] / 32768
  
  sdta
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read the data in a SoundFont \code{sf2} file
#' 
#' @param filename sf2 filename
#'
#' @return named list of SoundFont data:
#' \describe{
#' \item{\code{sfbk}}{Information about the SoundFont file from the \code{sfbk} chunk}
#' \item{\code{sdta}}{All audio data as a single numeric vector of floating point data 
#'      in the range [-1,1]}
#' \item{\code{pdta}}{Information about the presets, instruments and sample locations
#'       within the \code{sdta} data}
#' }
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_sf2 <- function(filename) {
  con <- file(filename, 'rb')
  on.exit(close(con))
  
  sfbk <- parse_sfbk(con)
  sdta <- parse_sdta(con)
  pdta <- parse_pdta(con)
  
  # Reamining bytes in stream should be ZERO
  rem_bytes <- readBin(con, 'raw', 1e4)
  if (length(rem_bytes) != 0) {
    warning("There are ", length(rem_bytes), " unparsed bytes at the end of this file.  SoundFont may be corrupt")
  }

  
  list(
    sfbk = sfbk,
    sdta = sdta,
    pdta = pdta
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an \code{audioSample} sample for an instrument from a SoundFont
#' 
#' @param sf SoundFont object as returned by \code{read_sf2()}
#' @param inst instrument index (integer) or name (string).  
#'        See \code{names(sf$pdta$shdr)} for the names of all instruments
#' @param dur note duration in seconds. Default value of NULL indicates to 
#'        play the sample as is.  If a non-NULL duration is given, then 
#' \itemize{
#' \item{If \code{dur} is less than the natural length of the sample, the full 
#'       sample will be returned and no truncation will occur}
#' \item{If \code{dur} is longer than the natural length of the sample, the 
#'       sample will be looped using the looping start/end points given in the
#'       \code{sf$pdta$shdr} data for the instrument.}
#' }
#' 
#' @return \code{audioSample} object compatible with the \code{audio} package
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_sample <- function(sf, inst, dur) {
  
  hdr <- sf$pdta$shdr[[inst]]
  if (is.null(hdr)) {
    stop("Instrument '", inst, "' not found. See 'names(sf$pdta$shdr)'")
  }
  
  # cat(sprintf("%03i", i), hdr$rate, hdr$key, hdr$link, hdr$type, hdr$name, "\n")
  natural_dur <- (hdr$end - hdr$start) / hdr$rate
  
  if (is.null(dur) || dur <= natural_dur) {
    data <- sf$sdta[1 + (hdr$start:hdr$end)]
  } else {
    
    pre_loop_dur  <- (hdr$loop_start - hdr$start)      / hdr$rate
    loop_dur      <- (hdr$loop_end   - hdr$loop_start) / hdr$rate
    post_loop_dur <- (hdr$end        - hdr$loop_end)   / hdr$rate
    
    Nloops <- ceiling((dur - pre_loop_dur - post_loop_dur) / loop_dur)
    
    pre_data <- sf$sdta[(hdr$start +1):hdr$loop_start]
    loop     <- sf$sdta[(hdr$loop_start + 1):hdr$loop_end]
    loop     <- rep(loop, Nloops)
    post_data <- sf$sdta[1 + (hdr$loop_end:hdr$end)]
    
    data <- c(pre_data, loop, post_data)
  }
  
  # Turn into an audio sample
  attr(data, 'rate') <- hdr$rate
  attr(data, 'bits') <- 16L
  class(data) <- 'audioSample'
  
  data
}




if (FALSE) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse the bytes from a soundfont
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  filename <- 'working/AWE ROM gm.sf2'
  # filename <- '/Users/mike/projectsdata/soundfonts/jnsgm2/Jnsgm2.sf2'
  
  filename <- '/Users/mike/projectsdata/soundfonts/weedsgm4_update.sf2'
  
  # https://github.com/bradhowes/SoundFonts
  # License MIT
  # filename <- '/Users/mike/projectsdata/soundfonts/fluidr3_gm.sf2'
  
  sf <- read_sf2(filename)
  
  # Names of all instruments
  names(sf$pdta$shdr)
  
  sf$pdta$shdr$`U-banjog2`
  
  
  samp <- create_sample(sf, 'Mandolin Trem E5', 1)
  audio::play(samp)
  
  samp <- create_sample(sf, 'Mandolin Trem C5', 1)
  audio::play(samp)
  
  samp <- create_sample(sf, 'Mandolin Trem A4', 1)
  audio::play(samp)
  
  
  if (FALSE) {
    con <- file(filename, 'rb')
    
    sfbk <- parse_sfbk(con)
    sdta <- parse_sdta(con)
    pdta <- parse_pdta(con)
    
    # Reamining bytes in stream should be ZERO
    rem_bytes <- readBin(con, 'raw', 1e4)
    stopifnot(length(rem_bytes) == 0)
  }
  
  
}












