
Work from gh-pages. Don't touch master unless you really need to. 
1. checkout gh-pages
2. make changes, commit and push
3. 'rake' 

### Rakefile

I have added a boilerplate Rakefile directly from the [jekyll-rake-boilerplate repo](https://github.com/gummesson/jekyll-rake-boilerplate). This saves you a small amount of time by prepending the date on a post name and populated the bare minimum of YAML front matter in the file. Please visit the link to the repo to find out how it runs. One thing to note is that there should be *no* space between the task and the opening bracket of your file name. ```rake post["Title"]``` will work while ```rake post ["Title"]``` will not.

There is another rakefile (UploadtoGithub.Rakefile) included that only has one task in it - an automated upload to a *Github Pages* location of the site. This is necessary because of the plugins used by this theme. It does scary stuff like move your ```_site``` somewhere safe, delete everything, move the ```_site``` back and then do a commit to the ```gh-pages``` branch of your repository. You can read about it [here](http://blog.nitrous.io/2013/08/30/using-jekyll-plugins-on-github-pages.html). You would only need to use this if you are using Github project pages to host your site. Integration with the existing Rakefile is left as an exercise for the reader.
