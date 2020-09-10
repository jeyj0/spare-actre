# Spare Actre

Spare Actre is a SPAced REpition, ACTive REcall learning tool for the command-line.

This tool is for you if you have plain-text notes, ideally org-mode. With a little bit of syntax you can create flashcards inside your notes, which the tool will automatically schedule for you to review.

## What does it do?

Spare Actre is basically a prompts system. It will scan your notes directory for prompts (see below for what you have to do to make it understand what a prompt it). It will then show you the prompt's question, then the answer when you tell it to. At last it will ask you whether you knew the answer. If you did, it will schedule the prompt for some time in the future. This time will continuously increase until you indicated you didn't know the answer. 

If you didn't know the answer, it'll schedule the prompt with the shortest interval it knows (1 hour) and you start over with that prompt. If you are on a streak, you will always be shown the prompt at least once a year.

## Usage

Spare Actre assumes you have all your notes in one flat directory. It also assumes there's no non-plaintext files in that directory. It will attempt to open each file inside your notes directory, so beware of that. No guarantees are made towards keeping your data instact.

The first time you use Spare Actre, you will have to create a `learning-data.dhall` file in your notes directory, containing the following:

``` dhall
let Review : Type =
{ time : Text
, wasKnown : Bool
}
let Prompt : Type =
{ question : Text
, answer : Text
, reviews : List Review
}
let MapEntry : Type =
{ mapKey : Text
, mapValue : Prompt
}
in
{ prompts = [] : List Prompt }
```

This file is where Spare Actre saves its data to.

Then just execute `$ spare-actre /PATH/TO/YOUR/NOTES/DIRECTORY`, assuming you've added `spare-actre` to your `PATH`.

### Creating Prompts (aka. flashcards)

You can create prompts by including a block like the following in a plaintext file in your notes directory:

``` org
#+begin_prompt
PUT THE FRONT OF THE CARD HERE
-----
PUT THE BACK OF THE CARD HERE
#+end_prompt
```

## Installation

Currently spare-actre is not packaged for any distribution. Instead, you will have to compile it yourself:

1. Download and install the `nix` package manager (https://nixos.org/download.html)
2. In the nix-shell, execute `$ ./full-build.sh`
   - Warning for mac users: it appears that nix will want to recompile ghc and cabal when doing this on MacOS. This can take somewhere around 16h (sample size 1).
3. The resulting binary will be in `./result/bin/spare-actre`. Copy this wherever you want and add it to your `PATH`. Or just call it where it is by running: `$ ./result/bin/spare-actre` in the project directory.

