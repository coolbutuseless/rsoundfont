
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
  
  # Chunk name and length  
  nm   <- 'phdr'
  len  <- 38
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len
  
  # Pre-allocate
  name    <- character(N)
  preset  <- integer(N)
  bank    <- integer(N)
  bag_idx <- integer(N)
  library <- integer(N)
  genre   <- integer(N)
  morpho  <- integer(N)
  
  # read in
  for (i in seq.int(N)) {
    name    [i] = trimws(read_char(con, 20))
    preset  [i] = read_uint16(con)
    bank    [i] = read_uint16(con)
    bag_idx [i] = read_uint16(con)
    library [i] = read_uint32(con)
    genre   [i] = read_uint32(con)
    morpho  [i] = read_uint32(con)
  }
  
  data.frame(
    name    ,
    preset  ,
    bank    ,
    bag_idx ,
    library ,
    genre   ,
    morpho  ,
    stringsAsFactors = FALSE
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'pbag' preset bag
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_pbag <- function(con) {
  
  # Chunk name and length  
  nm   <- 'pbag'
  len  <- 4
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len
  
  # Pre-allocate
  gen_idx <- integer(N)
  mod_idx <- integer(N)
  
  # Read in
  for (i in seq.int(N)) {
    gen_idx [i] = read_uint16(con)
    mod_idx [i] = read_uint16(con)
  }
  
  data.frame(
    gen_idx,
    mod_idx,
    stringsAsFactors = FALSE
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'pmod' preset modulator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_pmod <- function(con) {
  
  # Chunk name and length  
  nm   <- 'pmod'
  len  <- 10
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len
  
  # Pre-allocate
  mod_src_oper       <- integer(N)
  mod_dst_oper       <- integer(N)
  amount             <- integer(N)
  mod_amt_src_oper   <- integer(N)
  mod_amt_trans_oper <- integer(N)
  
  # Read in
  for (i in seq.int(N)) {
    mod_src_oper       [i] = read_sfmodulator(con)
    mod_dst_oper       [i] = read_sfgenerator(con)
    amount             [i] = read_sint16(con)
    mod_amt_src_oper   [i] = read_sfmodulator(con)
    mod_amt_trans_oper [i] = read_sfgenerator(con)
  }
  
  data.frame(
    mod_src_oper      ,
    mod_dst_oper      ,
    amount            ,
    mod_amt_src_oper  ,
    mod_amt_trans_oper,
    stringsAsFactors = TRUE
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'pgen'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_pgen <- function(con) {
  
  # Chunk name and length  
  nm   <- 'pgen'
  len  <- 4
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len
  
  # Pre-allocate
  oper   <- integer(N)
  amount <- integer(N)
  
  # Read in
  for (i in seq.int(N)) {
    oper   [i] = read_sfgenerator(con)
    amount [i] = read_genamounttype(con)
  }
  
  data.frame(
    oper   ,
    amount ,
    stringsAsFactors = FALSE
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'inst' 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_inst <- function(con) {
  
  
  # Chunk name and length  
  nm   <- 'inst'
  len  <- 22
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len
  
  # Pre-allocate
  name    <- character(N)
  bag_idx <- integer(N)
  
  # Read-in
  for (i in seq.int(N)) {
    name    [i] = trimws(read_char(con, 20))
    bag_idx [i] = read_uint16(con)
  }
  
  data.frame(
    name    ,
    bag_idx ,
    stringsAsFactors = FALSE
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'ibag' 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_ibag <- function(con) {
  
  # Chunk name and length  
  nm   <- 'ibag'
  len  <- 4
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len

  # Preallocate
  gen_idx <- integer(N)
  mod_idx <- integer(N)
  
  # Read in
  for (i in seq.int(N)) {
    gen_idx  [i] = read_uint16(con)
    mod_idx  [i] = read_uint16(con)
  }
  
  data.frame(
    gen_idx,
    mod_idx,
    stringsAsFactors = FALSE
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'imod'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_imod <- function(con) {
  
  # Chunk name and length  
  nm   <- 'imod'
  len  <- 10
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len
  
  # Pre-allocate
  src_oper       <- integer(N)
  dst_oper       <- integer(N)
  amount         <- integer(N)
  amt_src_oper   <- integer(N)
  amt_trans_oper <- integer(N)
  
  # Read in
  for (i in seq.int(N)) {
    src_oper       [i] = read_sfmodulator(con)
    dst_oper       [i] = read_sfgenerator(con)
    amount         [i] = read_sint16(con)
    amt_src_oper   [i] = read_sfmodulator(con)
    amt_trans_oper [i] = read_sfgenerator(con)
  }
  
  data.frame(
    src_oper      ,
    dst_oper      ,
    amount        ,
    amt_src_oper  ,
    amt_trans_oper,
    stringsAsFactors = FALSE
  )
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'igen'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_igen <- function(con) {
  
  # Chunk name and length  
  nm   <- 'igen'
  len  <- 4
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len
  
  # Pre-allocate space
  oper   = integer(N)
  amount = integer(N)
  
  # Read in
  for (i in seq.int(N)) {
    oper   [i] = read_sfgenerator(con)
    amount [i] = read_genamounttype(con)
  }
  
  data.frame(
    oper,
    amount,
    stringsAsFactors = FALSE
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parse 'shdr'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_shdr <- function(con) {
  
  # Chunk name and length  
  nm   <- 'shdr'
  len  <- 46
  
  # Assert size is sane
  size <- read_uint32(con)
  if (size %% len != 0) {
    stop(nm, " size should be a multiple of ", len, ". Got: ", size)
  }
  
  # Number of records to read
  N   <- size / len
  
  # pre-allocate space
  name       <- character(N)
  start      <- integer(N)
  end        <- integer(N)
  loop_start <- integer(N)
  loop_end   <- integer(N)
  rate       <- integer(N)
  key        <- integer(N)
  correction <- integer(N)
  link       <- integer(N)
  type       <- integer(N)
  
  # Read into pre-allocated space
  for (i in seq.int(N)) {
    name      [i] = read_char(con, 20)
    start     [i] = read_uint32(con)
    end       [i] = read_uint32(con)
    loop_start[i] = read_uint32(con)
    loop_end  [i] = read_uint32(con)
    rate      [i] = read_uint32(con)
    key       [i] = read_uint8(con)
    correction[i] = read_sint8(con)
    link      [i] = read_uint16(con)
    type      [i] = read_sfsamplelink(con)
  }
  
  data.frame(
    name      ,
    start     ,
    end       ,
    loop_start,
    loop_end  ,
    rate      ,
    key       ,
    correction,
    link      ,
    type      ,
    stringsAsFactors = FALSE
  )
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
  
  addr <- seek(con, where = NA)
  message("sdta offset: ", addr)
  
  if (TRUE) {
    # Lazy loading of sample data.
    # just make a note of where the sample data starts and its size.
    # skip over all this data and read on to the next chunk.
    sdta <- NA
    seek(con, where = size, origin = 'current')
  } else {
    # Greedy loading of sample data.
    stop("no longer support greedy")
    sdta <- read_sint16(con, n = size/2) 
    sdta[sdta < 0] <- sdta[sdta < 0] / 32767
    sdta[sdta > 0] <- sdta[sdta > 0] / 32768
  }
  
  message("sdta end: ", seek(con, where = NA))
  
  
  list(
    addr = addr,
    n    = size/2,
    data = sdta
  )
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
    file = filename,
    sfbk = sfbk,
    sdta = sdta,
    pdta = pdta
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper function to read 16 bit sample data from a soundfont file
#' @param con connection
#' @param nsamples number of 16 bit samples to read
#' @param sample_offset how many 16bit samples to skip over to get to the start
#' @param sdta_addr the byte offset of the start of the SDTA sample data.
#'
#' @return Numeric vector of data in range [-1, 1]
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_sdta_sample <- function(con, nsamples, sample_offset, sdta_addr) {
  seek(con, where = sdta_addr, origin = 'start')
  seek(con, where = sample_offset * 2, origin = 'current')
  data <- readBin(con, 'integer', nsamples, size=2, endian = 'little', signed = TRUE) 
  data[data < 0] <- data[data < 0] / 32767
  data[data > 0] <- data[data > 0] / 32768
  
  data
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an \code{audioSample} sample for an instrument from a SoundFont
#' 
#' @param sf SoundFont object as returned by \code{read_sf2()}
#' @param inst instrument index (integer) or name (string).  
#'        See \code{sf$pdta$shdr$name} for the names of all instruments
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
create_sample <- function(sf, inst, dur = NULL) {
  
  if (is.numeric(inst)) {
    hdr <- sf$pdta$shdr[inst, ]
  } else {
    hdr <- subset(sf$pdta$shdr, sf$pdta$shdr$name == inst)
  }
  
  if (nrow(hdr) != 1) {
    stop("Coudn't find instrument")
  }
  
  if (is.null(hdr)) {
    stop("Instrument '", inst, "' not found. See 'names(sf$pdta$shdr)'")
  }
  
  # cat(sprintf("%03i", i), hdr$rate, hdr$key, hdr$link, hdr$type, hdr$name, "\n")
  natural_dur <- (hdr$end - hdr$start) / hdr$rate
  
  con <- file(sf$file, 'rb')
  on.exit(close(con))
  sdta_addr <- sf$sdta$addr
  
  if (!is.null(dur) && dur <= natural_dur) {
    message(sprintf(
      "Natural duration (%.1fs) is longer than requested duration (%.1fs). Not truncating",
      natural_dur, dur
    ))
  }
  
  
  if (is.null(dur) || dur <= natural_dur) {
    data <- read_sdta_sample(
      con           = con,
      nsamples      = hdr$end - hdr$start + 1L,
      sample_offset = hdr$start,
      sdta_addr     = sdta_addr 
    )
  } else {
    
    pre_loop_dur  <- (hdr$loop_start - hdr$start)      / hdr$rate
    loop_dur      <- (hdr$loop_end   - hdr$loop_start) / hdr$rate
    post_loop_dur <- (hdr$end        - hdr$loop_end)   / hdr$rate
    Nloops <- ceiling((dur - pre_loop_dur - post_loop_dur) / loop_dur)
    
    pre_data <- read_sdta_sample(
      con           = con,
      nsamples      = hdr$loop_start - hdr$start,
      sample_offset = hdr$start,
      sdta_addr     = sdta_addr 
    )
    loop     <- read_sdta_sample(
      con           = con,
      nsamples      = hdr$loop_end - hdr$loop_start,
      sample_offset = hdr$loop_start,
      sdta_addr     = sdta_addr 
    )
    loop     <- rep(loop, Nloops)
    post_data <- read_sdta_sample(
      con           = con,
      nsamples      = hdr$end - hdr$loop_end + 1,
      sample_offset = hdr$loop_end,
      sdta_addr     = sdta_addr 
    )
    
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
  
  filename <- '/Users/mike/projectsdata/soundfonts/Essential Keys-sforzando-v9.6.sf2'
  filename <- '/Users/mike/projectsdata/soundfonts/weedsgm4_update.sf2'
  
  # https://github.com/bradhowes/SoundFonts
  # License MIT
  filename <- '/Users/mike/projectsdata/soundfonts/fluidr3_gm.sf2'
  
  start <- Sys.time()
  sf <- read_sf2(filename)
  print(Sys.time() - start)
  
  # Names of all instruments
  sf$pdta$shdr$name
  
  # sf$pdta$shdr$`U-banjog2`
  
  
  # samp <- create_sample(sf, inst = 'Mandolin Trem E5', 1)
  # audio::play(samp)
  
  samp2 <- create_sample(sf, 'Gun', NULL)
  audio::play(samp2)
  
  
  samp <- create_sample(sf, 5, 1)
  audio::play(samp)
  
  samp <- create_sample_lazy(sf, 'Mandolin Trem A4', 1)
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












