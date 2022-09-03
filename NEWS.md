
# rsoundfont 0.1.2  2022-09-03

* Parse information into `data.frames` instead of nested lists

# rsoundfont 0.1.1  2022-09-02

* Sound sample data is now only loaded on-demand when `create_sample()` is used.
    * This means very large soundfont files can be used without having to 
      load in the entire sample data into R (a waste of memory)
    * It does mean however that the file should be generally available and seekable
      on the local filesystem.

# rsoundfont 0.1.0  2022-08-22

* Initial release
