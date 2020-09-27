# Introduction

It's inspired by [gist.el](https://github.com/defunkt/gist.el) but for [Gitlab
Snippet](https://docs.gitlab.com/ee/api/snippets.html).

# Usage

``` elisp
(setq gsnip-private-token "<your-private-token>")
(setq gsnip-url "<your-gitlab-url>")
```

# Why you may need it

- For people who prefer to use *Gitlab*.
- Some companies may deploy *Gitlab* as their private code repository, in that
  case, using *gist* won't be appropriate any more.
