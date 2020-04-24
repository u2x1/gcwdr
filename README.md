# gcwdr

A static blog generator. The idea of template steals from [Hugo](https://github.com/gohugoio/hugo).

The generator is at a very early stage. The template function now supports:

1. dot
   ```
   [- this.content -]
   ```

2. foreach

   ```
   [- foreach x in global.post -]
     <li>[- x.title -]<li>
   [- end -]
   ```

3. partial

   ```
   [- partial header.html -]
   ```
   Partial files should be placed under `theme/layout/partial/`. This is a __fix__ path.

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
