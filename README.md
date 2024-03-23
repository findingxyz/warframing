# warframing

Right now, this searches for mods. It can also display a single page per mod.

`./api-server/` contains a Haskell application that does all the mod searching.

`./website/` provides an interface (website) to the api server. It also contains a guide on search syntax.

## Running

### API

Go into `./api-server/` and run `cabal run`. Or `cabal build` followed by `cabal install`.

### Website

Go into `./website/`, run `npm install` followed by `npm run dev`, or `npm run build`. Or use those SvelteKit adapters.

