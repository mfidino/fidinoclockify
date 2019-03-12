language: r
cache: packages
pandoc_version: 1.17.2

branches:
  only: source

script:
  - Rscript -e 'servr:::knit_maybe(c(".", "_source", "_posts"), c(".", "_posts", "_posts"), "build.R", "jekyll")'

deploy:
  provider: pages
skip_cleanup: true
github_token: $78c34c578a584d4f0534858be705ebecc84fe755
on:
  branch: source
target_branch: master
