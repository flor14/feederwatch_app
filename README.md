# Feederwatch App

[![shiny-deploy](https://github.com/flor14/feederwatch_app/actions/workflows/deploy-app.yaml/badge.svg)](https://github.com/flor14/feederwatch_app/actions/workflows/deploy-app.yaml) [![Test app w/ {renv}](https://github.com/flor14/feederwatch_app/actions/workflows/testing.yaml/badge.svg)](https://github.com/flor14/feederwatch_app/actions/workflows/testing.yaml)

## Run FeederWatch App in your computer using a Dockerfile

```bash
git clone git@github.com:flor14/feederwatch_app.git
cd feederwatch_app
```

```bash
docker build -t feederwatch_app .
```

```bash
docker run --rm -p 3838:3838 feederwatch_app
```
