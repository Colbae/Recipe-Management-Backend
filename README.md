# Recipe Manager

A Haskell HTTP/JSON backend for recipes, pantries, menu plans, shopping lists and
USDA-based nutrition estimates. Recipes can also be imported from and exported
to a small plain-text format. Uses Scotty and SQLite.

## Running

Requires GHC 9.6 and Cabal. Run commands from the project root:

```sh
cabal build
cabal run
```

The API listens on port `3000`. SQLite data is stored in
`database/recipes.sqlite3` (the database folder is gitignored). Set `PORT` or
`DATABASE_PATH` to override these defaults. Migrations run automatically at startup.

Set `USDA_API_KEY` before starting the server to enable food lookup and nutrition:

```sh
export USDA_API_KEY=your-key
cabal run
```

To create a key, go to the [USDA Website](https://fdc.nal.usda.gov/api-key-signup) and request one (it is free of charge).

Without a key, the other features remain available. Example JSON and recipe text
are in `examples/`.

Create a profile, then send its returned ID in the `X-User-Id` header for
profile-specific operations (the existing API still uses `/user` URLs):

```sh
curl -X POST http://localhost:3000/user/Alice
curl http://localhost:3000/menuPlans -H 'X-User-Id: 1'
```

Profiles are intended for a family or group of friends on a network with limited
access. **Profile selection is not authentication:** anyone with API access can
select any existing profile. `PROFILE_MODE=header` is the default and reads
`X-User-Id`; `PROFILE_MODE=disabled` rejects operations requiring a profile.
Menu plans and pantries are scoped to the selected profile. Recipe creation uses
that profile as the author; recipe updates and deletions require the author profile.

## Structure

- `src/Domain`: pure types, smart constructors, validation, parsers, rendering,
  unit conversion and calculations. No database or HTTP calls.
- `src/Api`: routes, request/response handling and profile-selection checks.
- `src/Storage`: SQLite operations and migrations.
- `src/Integration`: USDA HTTP requests and nutrition lookup orchestration.
- `sql/`: query and migration files, embedded into the executable at build time.

Nutrition values are estimates: missing nutrient amounts contribute zero, and
reports carry `estimated: true`. Nutrition calculations currently require
ingredient weights in grams or kilograms. Menu-plan nutrition counts one serving
of each recipe entry.

Shopping lists use whole-recipe quantities. Menu-plan shopping lists merge
compatible duplicate ingredients and subtract the pantry once, using either
supplied on-hand ingredients or a saved pantry.

Recipe and menu-plan JSON validation reports independent field errors together
in an `errors` array. Malformed recipe text returns a parser error with a location.
`GET /recipes/:id/export` returns UTF-8 plain text for display or saving by a client.

## Adding SQL queries

1. Add a `.sql` file under the relevant folder, such as `sql/recipes/`.
2. Embed and export it as a `Query` in the corresponding `Storage.Sql.*` module,
   following the existing definitions.
3. Execute it from the matching storage module. Use `?` placeholders and bound
   parameters rather than inserting user input into SQL strings.

Rebuild after SQL changes. If adding a new SQL folder or Haskell module, also
register it in `extra-source-files` or `other-modules` in `Recipe-Manager.cabal`.

## Adding migrations

1. Add the next numbered script in `sql/migrations/`, for example
   `005_description.sql`.
2. Embed and export it as `Text` in `Storage.Sql.Migrations`.
3. Append a `Migration` entry to the list in `Storage.Database`, using the next
   consecutive version and matching filename.

Rebuild and restart the server. Pending migrations and their history are applied
transactionally. Add new migrations rather than editing ones already applied:
existing databases do not rerun old scripts.

## Tests

```sh
cabal test domain
```

The small Hspec suite covers domain parsing/rendering, shopping requirements, ingredient
subtraction, nutrition addition, recipe filtering and validation. It makes no
database or external API calls.

## Possible future work

- A frontend for recipe editing, pantry management and menu planning.
- Real authentication, for example token validation through Keycloak/OIDC,
  replacing header-based profile selection if access beyond the trusted group is needed.
- Support for different nutrition information providers.
