# Feederwatch App


## Run FeederWatch App in your computer using a Dockerfile

```bash
git clone https://github.com/flor14/feederwatch_app.git
cd feederwatch-app
```

```bash
docker build -t feederwatch-app .
```

```bash
docker run --rm -p 3838:3838 feederwatch-app
```