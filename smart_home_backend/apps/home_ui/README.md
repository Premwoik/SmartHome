# HomeUi

Clone this project and rename

```sh
export FROM="home_ui"
export FROM_MODULE="HomeUi"
export TO="my_app"
export TO_MODULE="MyApp"

# Skip these steps if you used GitHub's 'Use this template' feature
git clone git@github.com:zestcreative/home_ui.git "$TO"
cd "$TO"

# Rename the project
git ls-files -z | xargs -0 perl -p -i -e "s/$FROM/$TO/g; s/$FROM_MODULE/$TO_MODULE/g;"
mv "lib/${FROM}" "lib/$TO"
mv "lib/${FROM}.ex" "lib/${TO}.ex"
mv "lib/${FROM}_web" "lib/${TO}_web"
mv "lib/${FROM}_web.ex" "lib/${TO}_web.ex"
mv "test/${FROM}_web" "test/${TO}_web"

# Skip these steps if you used GitHub's 'Use this template' feature
rm -rfv .git
git init .

# Commit Renaming
git add -A
git commit -m "PETAL stack init 🐣🔥"
```

To start your Phoenix server:

  * Setup the project with `mix setup`
  * Start Phoenix endpoint and IEx console with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

Ready to run in production? Please [check our deployment guides](https://hexdocs.pm/phoenix/deployment.html).

## Learn more

  * Official website: https://www.phoenixframework.org/
  * Guides: https://hexdocs.pm/phoenix/overview.html
  * Docs: https://hexdocs.pm/phoenix
  * Forum: https://elixirforum.com/c/phoenix-forum
  * Source: https://github.com/phoenixframework/phoenix
