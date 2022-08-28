


test_that("multiplication works", {
  
  # Florestan Subset Soundfont
  # From the TinySoundFont repository.
  # MIT license
  # https://github.com/schellingb/TinySoundFont
  sf <- read_sf2("sf2/florestan-subset.sf2")
  
  expect_true(is.list(sf))
  expect_identical(names(sf), c("sfbk", "sdta", "pdta"))
  
  expect_identical(
    names(sf$pdta$shdr),
    c("ACCFR68A", "ACCFR96A", "BAGPP64A", "BAGPP74A", "BAGPP84A", 
      "BAGPP93A", "BRASS64", "BRASS72A", "BRASS84A", "BS_LD48", "BS_LD72", 
      "CELSTA1", "C_ORG46A", "C_ORG56A", "C_ORG66A", "JAZGTA1A", "MRMBA50", 
      "MRMBA62", "M_BOX82", "NYLON48A", "NYLON57A", "NYLON70A", "NYLON79A", 
      "NYLON97A", "O_HIT68", "PIANO36", "PIANO41", "PIANO48", "PIANO56", 
      "PIANO64", "PIANO73", "PIANO78", "PIANO84", "PIANO92", "PIANOA3", 
      "PIZZH84", "PIZZ_71", "PNFLT72", "POLY_48A", "POLY_60A", "POLY_72A", 
      "SOUND60", "SOUND84", "SURDO60", "SYBS133", "SYBS148A", "SYBS172A", 
      "S_SAXA2", "S_SAXA8", "VIOLN68", "VIOLN70", "VIOLN76", "VIOLN80", 
      "VIOLN86", "VIOLNA4", "EOS")
  )
})
