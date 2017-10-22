# beat-detection

Prints to the CLI when beats are detected in the track given.

By default it tries to open tamborine.ogg, which is in the source directory.

You'll need to be in the source directory for it to load it that way.

Alternatively, you can supply a path on the command line, for example

`beat-detection somefile.ogg`

It uses libsndfile which I believe doesn't currently support mp3, but it supports most other formats.

It can do OK on some genres of music, but others it struggles. 

Also, because threadDelay isn't accurate, it gets a little out of sync with the music.

If you wrote your own play routine and just printed the value every n frames it would work.

### Prerequisites

stack, hsndfile, sox

### Installation

`stack install`
