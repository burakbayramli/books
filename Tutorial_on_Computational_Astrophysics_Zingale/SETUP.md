

# create the initial jupyter-book

```
jupyter-book create comp_astro_tutorial
```

and put it under github contol (on main)

# create the empty `gh-pages` branch:

following: https://gist.github.com/ramnathv/2227408

```
cd /path/to/repo-name
git symbolic-ref HEAD refs/heads/gh-pages
rm .git/index
git clean -fdx
echo "My GitHub Page" > index.html
git add .
git commit -a -m "First pages commit"
git push origin gh-pages
```

# Create the build gitub action.

I based it off of:
https://github.com/choldgraf/deploy_configurations/blob/master/.github/workflows/main.yml


