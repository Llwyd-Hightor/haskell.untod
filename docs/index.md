# A Programmer's Journey with Haskell

**Work in progress**

<!-- ------------------------------------------------------------------ -->
## Introduction

> **"Your mileage may vary"** *(common consumer warning)*

I'm a mainframe assembly language systems programmer, 
but as part of the job I've learned other languages to keep my hand in.

I have a program, *untod*, which has developed over the years, 
and which I use for a porting exercise whenever I want 
to learn a new language.

I use MS Windows.
<!-- ------------------------------------------------------------------ -->
* [More about *untod*](aboutuntod.md)
* [More about me](aboutme.md)
* [A Rust starting point](https://www.rust-lang.org/)
* [A Haskell starting point](https://www.haskell.org/)

<!-- ------------------------------------------------------------------ -->
Having just come to the end of a contract, 
I decided to learn another language.
I had previously (2009) taken a quick look at Haskell 
and given up, mainly because I just couldn't get my head round the IO Monad.
I thought another try would be appropriate
(and, as it turned out, I was so right).

And so the journey begins: porting Untod from Rust to Haskell
(and adding a few features on the way).
<!-- ------------------------------------------------------------------ -->
## Getting started -- the Haskell environment
> **"To begin, at the beginning"** *(Dylan Thomas, Under Milk Wood)*

I had an old (2009) version of the Haskell Platform installed,
and thought it might be a good idea to bring it up-to-date. 
This took a number of steps, after I uninstalled it:

<!-- ------------------------------------------------------------------ -->
### Chocolatey + `haskell-dev` (No)
The first try was as recommended on the
[Haskell Platform page](https://www.haskell.org/platform/), 
which suggests, in my case, 
using [Chocolatey](https://chocolatey.org/) 
to install `haskell-dev`.

Now I confess, I do not like Chocolatey. 
(You'll find this whole document heavily larded with my prejudices;
just live with it -- I'm old and grumpy.) 
But I thought in all good conscience I should give it a go.
However, the end result was not what I hoped for: problems with environment variables; unable to import; 
files stuck in obscure places in hitherto unused parts of my file system; 
and even files that I wasted a lot of time on when cleaning up,
because of obscure access restrictions.

So I uninstalled, and moved on.

<!-- ------------------------------------------------------------------ -->
### Reinstalling my original Haskell Platform (No)
Briefly, that now didn't work, either.
I really didn't think it was worth investing time trying to fix this,  
so I just uninstalled and moved on.

<!-- ------------------------------------------------------------------ -->
### Haskell Stack (No)
Then I discovered 
[Haskell Stack](https://docs.haskellstack.org/en/stable/README/),
downloaded and ran the installer. 
Wanting to keep my C:-drive clean, I (foolishly) didn't install Stack
in the default location, but somewhere on my D:-drive. I think that may have been a mistake.

I then followed the Quick Start Guide.
Installing the compiler and stuff takes a while, 
so I got a cup of tea...

When I got back, imagine my horror to find that 
_**Stack had afforded itself the right to add a new folder -- C:\SR -- 
in the root of my C-drive.**_

I later discovered how to fix this. 
But for now I didn't know, 
and just uninstalled or deleted everything, again.

<!-- ------------------------------------------------------------------ -->
### Chocolatey again (No)
To quote Gerard Hoffnung, 
"At this point, I must have lost my presence of mind".
I know it's a cliché, 
but "Insanity is doing the same thing over and over again 
while expecting different results".
But I was fast despairing that I would ever get 
a satisfactory platform on which to learn and refactor,
so I tried, once more, the path of Chocolatey.

And I got the same results as before, 
and just uninstalled or deleted everything, again.

<!-- ------------------------------------------------------------------ -->
### Haskell Stack again (Yes!)
So, it's getting dark (this nonsense has consumed a whole day),
and still no Haskell development platform.
Should I, then, allow Stack to have its dirty way with my file systrm?
Well, no. I found the answer to my question in an article by   
[Arvind Devarajan: 
_Installing Haskell stack (in Windows)_](https://blog.ramdoot.in/installing-haskell-stack-in-windows-7c8fd2c79f)

I altered my environment:
```
STACK_ROOT=D:\HaskellStack\Root
``` 
and added this to `D:\HaskellStack\Root\config.yaml`: 
```
local-programs-path: D:\HaskellStack\programs
```
The exact point at which I did this last modification 
is now unclear to me.
I vaguely remember that `config.yaml` hadn't been created 
at the time I thought I needed to change it, 
and that I may have needed to repeat some of the 
initialisation steps after the change. 
You'll need to experiment.

And so, we are almost there

### Rounding out the development environment

To be continued


----
A Programmer's Journey with Haskell  
Copyright **©** 2020, Brent Longborough  
[Licence: Creative Commons CC BY-SA (4.0)](https://creativecommons.org/licenses/by-sa/4.0/)
