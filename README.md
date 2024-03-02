# warframing

Right now, this searches for mods.

`./api-server` contains a Haskell application that does all the mod searching.

`./website` provides an interface (website) to the api server. It also contains a guide on search syntax.

## Future features/definitely not bugs

- Display mod at a specific level is searched for.
    - Right now `pistol pestilence d:"+15% status chance"` displays the same result as `pistol pestilence d:"+60% status chance"`, the Pistol Pestilence mod. The max value is displayed by default, despite the query being for the base version in the former case.
- Click a mod to visit a dedicated page for it.
    - `uniqueName` can be used as a URL path.
    - Increasing/decreasing mod levels is already implemented, just commented out.
    - A nice design to display text alternatively (maybe?) and drop locations would be nice.

## Running

How to run???

### API

Go into `./api-server` and run `cabal run`. Or `cabal build` followed by `cabal install`.

### Website

Go into `./website`, run `npm install` followed by `npm run dev`, or `npm run build`. Or use those SvelteKit adapters.
