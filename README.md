# gcwdr

## Demo

```
λ> tree test-data
test-data
├── content
│   └── post
│       ├── test2.md
│       └── test.md
└── theme
    ├── layout
    │   ├── index.html
    │   └── post.html
    └── static
        ├── css
        │   ├── custom.css
        │   └── skeleton.css
        └── js

7 directories, 6 files

λ> stack ghci
Using main module: 1. Package `gcwdr' component gcwdr:exe:gcwdr-exe with main-is file: /home/nutr1t07/gcwdr/app/Main.hs
gcwdr> configure (lib + exe)
Configuring gcwdr-0.1.0.0...
...
*Main> trans "./test-data"
*Main>
Leaving GHCi.

λ> tree test-data
tree test-data
test-data
├── content
│   └── post
│       ├── test2.md
│       └── test.md
├── public
│   ├── css
│   │   ├── custom.css
│   │   ├── skeleton
│   │   └── skeleton.css
│   ├── index.html
│   └── post
│       ├── test
│       │   └── index.html
│       └── test2
│           └── index.html
└── theme
    ├── layout
    │   ├── index.html
    │   └── post.html
    └── static
        ├── css
        │   ├── custom.css
        │   └── skeleton.css
        └── js

13 directories, 11 files
```
