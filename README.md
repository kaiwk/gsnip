[![MELPA](https://melpa.org/packages/gsnip-badge.svg)](https://melpa.org/#/gsnip)

# Introduction

It's inspired by [gist.el](https://github.com/defunkt/gist.el) but for [Gitlab
Snippet](https://docs.gitlab.com/ee/api/snippets.html).

# Usage

``` elisp
(setq gsnip-private-token "<your-private-token>")
(setq gsnip-url "<your-gitlab-url>")
```

1. Select a region, then invoke `gsnip-region`.
2. Invoke `gsnip`, show the snippets list.

| keymap | command                |
|--------|------------------------|
| n      | cursor move down       |
| p      | cursor move up         |
| q      | quit window            |
| G      | refresh                |
| d      | delete snippet         |
| e      | edit snippet meta info |
| y      | yank snippet link      |
| Y      | yank snippet           |
| RET    | show current snippet   |

3. After pressing `RET`, you can edit the snippet with:

- `C-x C-s`: save the snippet.
- `C-x C-w`: rename the snippet, note that the suffix will affect highlighting.

# Why you may need it

- For people who prefer to use *Gitlab*.
- Some companies may deploy *Gitlab* as their private code repository, in that
  case, using *gist* won't be appropriate any more.
