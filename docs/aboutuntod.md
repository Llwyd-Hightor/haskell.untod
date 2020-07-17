# About *untod*

I'm not sure when *untod* was born, but I'd guess somewhere around 2012-13,
when I was involved in a project that needed accurate timing statistics,
and was the occasion of my first needing to deal with the IBM Time-Of-Day, 
or TOD, clock -- a gnarly bumch of 13 or so hex digits representing 
the number of microseconds since midnight at the start 
of January 1st, 1900.

The first version was written in Assembler, and was very simple: 
it took in a TOD clock value and spat out a character timestamp, 
something one-way like this:

`CA084AD7D4012` => `20120817112347321874`

As time went by, I wanted more sophistication, 
not as a subroutine (remember those?), 
but as a useful tool I could pull out of my hat 
whenever I wanted to do things involving mainframe clocks.

And so it was that the idea moved to the PC, 
and started to sprout features.
First in Go; later in Rust; and now in Haskell.

*untod* has been a vehicle for me to acquire some skill 
in all these languages -- to learn them to a point at which I could, stretching a bit, say "I know how to program in *x.*

That's enough. For the curious, 
here's the output from the latest release's (0.1.1) `untod --help` 

```
untod 0.1.1.0 - a Swiss Army knife for TOD and other clocks

Usage: untod.EXE [(-o|--tod) | (-d|--date) | (-m|--pmc) | (-u|--unix) | 
                   (-s|--seconds)] [-c|--clipboard] [--csv] [--headers] 
                 [-a|--annot] [-z|--zulu] [(-l|--loran) | (-t|--tai)] 
                 [--lpad | --rpad] [-i|--input <filename>] [--lzone <offset>] 
                 [--azone <offset>] [<value...>] [-v|--version]
  Converts among TOD, Date/Time, PARS Perpetual Minute Tick, Unix seconds, and
  20th century seconds for UTC, TAI or LORAN/IBM

Available options:
  -o,--tod                 Convert from TOD (default)
  -d,--date                Convert from Date/Time
  -m,--pmc                 Convert from PMC
  -u,--unix                Convert from Unix seconds
  -s,--seconds             Convert from 20th Century seconds
  -c,--clipboard           Input values from clipboard
  --csv                    Output in CSV format
  --headers                Output column headers
  -a,--annot               Annotate plain output with run mode
  -z,--zulu                Suppress Zulu offset result if others given
  -l,--loran               Ignore leap-seconds -- LORAN/IBM
  -t,--tai                 Ignore leap-seconds -- TAI (International Atomic Tick
  --lpad                   Pad TOD with zeroes on left
  --rpad                   Pad TOD with zeroes on right (default is intelligent
                           padding)
  -i,--input <filename>    Input values from a file ( - for STDIN )
  --lzone <offset>         Override local time offset ([-+]n.n) [env:
                           UNTOD_LZONE=]
  --azone <offset>         Alternative time offset ([-+]n.n) [env: UNTOD_AZONE=]
  <value...>               Values for conversion
  -v,--version             Show version; more -v flags, more info
  -h,--help                Show this help text

```
