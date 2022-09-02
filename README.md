
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsoundfont

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
<!-- badges: end -->

`{rsoundfont}` is a package for parsing
[soundfont](https://en.wikipedia.org/wiki/SoundFont) files which contain
audio data representing multiple musical instruments.

Soundfont files are a common way of distributing a set of music
instruments as audio samples, and are often used for
[MIDI](https://en.wikipedia.org/wiki/MIDI) playback support on various
computers and mobile devices.

In addition to a structured way for storing multi-instrument data in a
single file, the Soundfont format also includes looping information for
each sample which allows for a sample playtime to be extended by
repeating pre-determined sections of the audio. This is useful when
simulating (for example) a piano note held for a number of seconds, even
though the actual sample is of a much shorter period.

## What’s in the box

-   `read_sf2(sf2_filename)` read a SoundFont file into a nested list of
    R objects.
    -   This parses the meta-information about the samples only.
    -   Audio data is only read later, as required, when calling
        `create_sample()`
-   `create_sample(sf, inst, dur)` create a playable sample of the given
    instrument. Sample will be looped to the given duration.
    -   This will return an `audioSample` object compatible with the
        [`{audio}`](https://cran.r-project.org/package=audio) package.
    -   Once created, you can play the sample with `audio::play()`

## SoundFont Technical Specification

The technical specification for SoundFont v2.04 is available
[online](https://www.synthfont.com/sfspec24.pdf)

This package currently uses the chunk naming conventions from the
specification.

## Lazy loading of sound data

Audio data for samples can be large - quality SoundFont files can easily
contain data for hundreds of instruments with a gigbyte of audio
samples. Many of these instruments will never be used in a particular
session.

So rather than loading all this audio data up-front, a reference to the
soundfont file is kept, and audio data is loaded from the file as
instruments areneeded e.g. in `create_sample()`

## Limitations

-   This package should load most SoundFont files with version v2.00 or
    greater
-   This package currently uses the chunk naming conventions from the
    specification. This is not always very easy to read/parse, and users
    are directed to the specification for interpretation of the data
    structure.
-   Unsupported for now as I couldn’t find any test files
    -   24bit Samples (If you know of any 24bit Soundfont files, please
        let me know in the github issues)
    -   Chunks may have odd lengths - in which case a padding byte
        should be added so that subsequent chunks are word-aligned. I
        haven’t seen any of these in the wild, and this current package
        will throw an error if this is the case. Please let me know in
        the github issues if this occurs.
    -   Stereo samples may be possible in a soundfont, but I haven’t
        seen any just yet.
-   No support for `*.sfArk` or `*.sfz` compressed files.
    -   Is there an open source convertor/decompressor for these?

\*\* Please let me know of any `*.sf2` files which this package cannot
parse \*\*

## Future possibilities

### Pitch-shifting & time-stretching

Audio samples within a SoundFont are supplied at one (or maybe a few)
different pitches. In order to be general useful as an instrument for
MIDI playback, some facility will have to be added for pitch-shifting.

## Installation

You can install from
[GitHub](https://github.com/coolbutuseless/rsoundfont) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/rsoundfont')
```

## Example

1.  Obtain a soundfont.
    -   This example uses `weedgm4_update` SoundFont from
        <https://www.philscomputerlab.com/general-midi-and-soundfonts.html>
2.  Load the SoundFont data with `read_sf2()`
3.  Examine the soundfont object
4.  Create and play samples from the Soundfont

``` r
library(rsoundfont)

# Source: https://www.philscomputerlab.com/general-midi-and-soundfonts.html
filename <- '/Users/mike/projectsdata/soundfonts/weedsgm4_update.sf2'

sf <- read_sf2(filename)
#> sdta offset: 368
#> sdta end: 14908536

# Names of all instruments
head(names(sf$pdta$shdr), 20)
#>  [1] "Leslig5(L)"       "Leslig5(R)"       "Leslid5(L)"       "Leslid5(R)"      
#>  [5] "Leslie4(L)"       "Leslie4(R)"       "Leslifs3(L)"      "Leslifs3(R)"     
#>  [9] "Lesligs2(L)"      "Lesligs2(R)"      "Leslias1(L)"      "Leslias1(R)"     
#> [13] "Leslic1(L)"       "Leslic1(R)"       "Mandolin Trem E5" "Mandolin Trem C5"
#> [17] "Mandolin Trem A4" "Mandolin Trem G4" "Mandolin Trem E4" "Mandolin Trem C4"

# Some Sample/looping information about an instrument
sf$pdta$shdr[['Mandolin Trem E5']]
#> $name
#> [1] "Mandolin Trem E5"
#> 
#> $start
#> [1] 1246934
#> 
#> $end
#> [1] 1289001
#> 
#> $loop_start
#> [1] 1266182
#> 
#> $loop_end
#> [1] 1288993
#> 
#> $rate
#> [1] 44100
#> 
#> $key
#> [1] 60
#> 
#> $correction
#> [1] 0
#> 
#> $link
#> [1] 0
#> 
#> $type
#> [1] 1
```

``` r
samp <- create_sample(sf, 'Mandolin Trem E5', 1)
audio::play(samp)
```

<a href="https://raw.githubusercontent.com/coolbutuseless/rsoundfont/main/man/samples/01.wav">
Wave file </a>

``` r
samp <- create_sample(sf, 'Mandolin Trem C5', 1)
audio::play(samp)
```

<a href="https://raw.githubusercontent.com/coolbutuseless/rsoundfont/main/man/samples/02.wav">
Wave file </a>

``` r
samp <- create_sample(sf, 'Bagpipe Drone 3', 1)
audio::play(samp)
```

<a href="https://raw.githubusercontent.com/coolbutuseless/rsoundfont/main/man/samples/03.wav">
Wave file </a>

## Acknowledgements

-   R Core for developing and maintaining the language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository
