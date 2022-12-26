# gcwdr

*(partially implemented)*

for each markdown article object, you are able to access these properties in template:

```
global {
  config {
    siteTitle: "xxxxx",
    siteUrl: "https://xxx.com/",
    siteMenus: [{ menuName: "xx", menuLoc: "/xx/xxx" }]
  },
  partials {
    xx1.html: "xxx<xx>xxxx</xx>xxx",
    xx2.html: "xxxx"
  }
}
this {
    title: "xxx",
    date: "xxxx-xx-xx",
    category: "yy",
    template: "xx.html",
    relLink: "/xx/xx.html",
    content: "blah blah blah..."
    // and something else in the meta data of the post markdown file
}
```

1. The `partials` field in `global` is the raw partial file content. You normally use `[- partial xx.html -]` to employ them instead of `[- global.partials.xx.html -]` because the latter one wouldn't work.
2. The `template` field in `this` is designed for choosing its corresponding template. If there is no `template` specified in Markdown metadata, it will be default to `post.html`.
3. Any entry that extra-added to Markdown metadata will be available to template accessing.

as for the `index.html` template, it has some different properties:
```
global: {
  config: {
    siteTitle: "xxxxx",
    siteUrl: "https://xxx.com/",
    siteMenus: [{ name: "xx", loc: "/xx/xxx" }]
  },
  partials: {
    xx1.html: "xxx<xx>xxxx</xx>xxx",
    xx2.html: "xxxx"
  }
}
this: {
  posts: [
    // each post object has complete properties just as accessings in `post.html`
    {
      title: "xxx",
      date: "xxxx-xx-xx",
      category: "yy",
      template: "xx.html",
      relLink: "/xx/xx.html",
      content: "blah blah blah..."
    },
    {
      title: "xxxx",
      date: "xxxx-xx-xx",
      category: "yyyy",
      template: "xx.html",
      relLink: "/xx/xx2.html",
      content: "blah blah blah..."
    }
  ],
  categories: [
    {
      cateName: "yy" // name of the category
      posts [
        {
          title: "xxx",
          date: "xxxx-xx-xx",
          category: "yy",
          template: "xx.html",
          relLink: "/xx/xx.html",
          content: "blah blah blah..."
        }
      ]
    },
    {
      cateName: "yyyy"
      posts: [
        {
          title: "xxxx",
          date: "xxxx-xx-xx",
          category: "yy",
          template: "xx.html",
          relLink: "/xx/xx2.html",
          content: "blah blah blah..."
        }
      ]
    }
  ]
}

```

# A template example

1. `index.html`

```
<!DOCTYPE html>
<html>
<head>
  [- partial head.html -]
  <title>[- global.config.siteTitle -]</title>
</head>

<body>
  <div class="container">
    [- partial sidebar.html -]

    <div class="post-list content">
     [- foreach y in this.categories -]
      <h3>[- y.cateName -]</h3>
       [- foreach x in y.posts -]
        <li>
          <a href="[- x.relLink -]">[- x.title -]</a>
          <span class="date"> - [- x.date -]</span>
        </li>
       [- end -]
     [- end -]
    </div>
  </div>
</body>
</html>
```

2. `post.html`

```
<!DOCTYPE html>
<html lang="en">
  <head>
    [- partial head.html -]
    <title>[- this.title -] | [- global.config.siteTitle -]</title>
  </head>

  <body>
    <div class="container">
      [- partial sidebar.html -]
      <div class="content">
        <h3>[- this.title -]</h3>
        [- this.content -]
      </div>
    </div>
  </body>
</html>
```
