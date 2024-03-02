FROM haskell:9-slim

WORKDIR /opt/warframing

RUN cabal update

COPY ./warframing.cabal /opt/warframing/warframing.cabal

RUN cabal build --only-dependencies -j4

COPY . /opt/warframing
RUN cabal install

CMD ["warframing"]