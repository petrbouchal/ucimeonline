on:
  push:
    branches: src

name: blogdown

jobs:
  build:
    runs-on: macOS-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      RENV_PATHS_ROOT: ~/Library/Application Support/renv
    steps:
      - name: Checkout repo
        uses: actions/checkout@v2

      - name: Setup R
        uses: r-lib/actions/setup-r@master

      - name: Install XQuartz on MacOS
        if: runner.os == 'macOS'
        run: brew cask install xquartz

      - name: Install cairo
        if: runner.os == 'macOS'
        run: brew install cairo

      - name: Install pandoc and pandoc citeproc
        run: |
          brew install pandoc
          brew install pandoc-citeproc

      - name: Cache packages
        uses: actions/cache@v1
        with:
          path: ${{ env.RENV_PATHS_ROOT }}
          key: ${{ runner.os }}-renv-${{ hashFiles('**/renv.lock') }}
          restore-keys: |
            ${{ runner.os }}-renv-

      - name: Restore packages
        shell: Rscript {0}
        run: |
          if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
          renv::restore()

      - name: Install rmarkdown
        run: |
          R -e 'install.packages('rmarkdown')'

      - name: Build site
        env:
          UO_SHEETS_PASSWORD: ${{ secrets.UO_SHEETS_PASSWORD }}
        run: |
          R -e 'rmarkdown::render_site()'

      - name: Deploy
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: public
          publish_branch: master