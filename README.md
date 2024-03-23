# warframing

Right now, this searches for mods.

`./api-server/` contains a Haskell application that does all the mod searching.

`./website/` provides an interface (website) to the api server. It also contains a guide on search syntax.

## Future features

- A better guide. Examples are clear.
    - In a search, tell the user what the query is doing. e.g. `
- If somehow possible, an even better looking website.
- `AND` and `OR`. This would also require parentheses parsing.
    - `AND` is redudant and can be skipped over in parsing.
    - `OR` could be parsed by `orQuery = "OR " *> query`.
- Display mods at a specific level.
    - Right now `pistol pestilence d:"+15% status chance"` displays the same result as `pistol pestilence d:"+60% status chance"`, the Pistol Pestilence mod. The max value is displayed by default, despite the query being for the base version in the former case.
- Dedicated page for a mod with specific information.
    - `uniqueName` can be used as a URL path.
    - Increasing/decreasing mod levels is already implemented, just commented out.
    - A nice design to display text alternatively (maybe?) and drop locations would be nice.
- Sort mods after searching.
    - `QueryParam "sortby" Text` for queries with an order (rarity, base capacity, etc.).
    - `QueryParam "order" Text` for `"asc", "desc"`.

## Running

### API

Go into `./api-server/` and run `cabal run`. Or `cabal build` followed by `cabal install`.

### Website

Go into `./website/`, run `npm install` followed by `npm run dev`, or `npm run build`. Or use those SvelteKit adapters.

