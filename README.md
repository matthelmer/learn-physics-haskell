
# Learn Physics With Functional Programming
### Personal Study Guide

![Book Cover](book_front.png)

This repository contains my solutions and notes for exercises from the book "Learn Physics With Functional Programming" by Scott N. Walck.

## About This Project

I'm working through this book to learn both Haskell and physics simultaneously. The code here aims to be clear and educational, using simple Haskell patterns.


## Repository Structure

The solutions are organized by chapters, corresponding to the book's structure. Each chapter has its own module in the `src/LPFP/` directory.

```
learn-physics-haskell/
├── app/              # Executables for each chapter
│   ├── lpfp-ch02.hs
│   ├── lpfp-ch03.hs
│   └── ...
├── src/
│   └── LPFP/        # Exercise solutions by chapter
│       ├── Ch02.hs
│       ├── Ch03.hs
│       └── ...
└── learn-physics-haskell.cabal
```

## Getting Started

### Prerequisites
- GHC (Glasgow Haskell Compiler) 
- Cabal build tool

### Running the Code

```bash
# Build all chapters
cabal build

# Run a specific chapter's exercises
cabal run lpfp-ch02  # Replace with ch03, ch04, etc.
```

## Disclaimer and Purpose

This is not an official solution guide or affiliated with the book's author or publisher. All content is my own interpretation and work, created for educational purposes.

As this repository represents my personal learning journey, I'm not actively seeking contributions. However, if you spot an error or have a suggestion, feel free to open an issue for discussion.

Thanks to my friend Travis Cardwell for all the Haskell help.
