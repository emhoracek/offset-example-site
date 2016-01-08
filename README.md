# A Personal Website with Fn and Offset

This is a skeleton for a personal website using [Fn](http://www.github.com/dbp/fn) and [Offset](http://www.github.com/dbp/offset). You can see it in action at [daydrea.me](daydrea.me). Feel free to use this to make your own website!

If you have any trouble following the directions below, let me know so I can
improve them!!

# Setup

You'll need [Stack](http://docs.haskellstack.org/en/stable/README.html) to get
started. Install stack by following [these directions](http://docs.haskellstack.org/en/stable/README.html).

Offset uses Redis for caching, so [install Redis](blah), too.

You'll also need a WordPress site! I used DigitalOcean's "One-Click Install" for
mine and it works wonderfully, but you can also just install and run WordPress 
locally for development.

Make sure to install the correct version of the
WP API plugin. Log in to the WP admin, then go to "Plugins" and "Add New". Do
not install the WP API version 2, because it's not finished yet and also it's
incompatible with Offset. Instead, search for "json-rest-api" and install and
activate version 1.2.4. 

Next write up a ".env" file based on "example.env". Add your WordPress username and
password, or create a new user from the WP admin just for Offset.

Finally git clone or fork or whatever this repository and `stack build` to build your first website.
`stack run homepage` and vist [http://localhost:8000](http://localhost:8000) to
see the default site!

# Customization Basics

This website uses the [Heist templating language](blah). You can edit the
templates in the "templates" directory and also add new ones. You can add a
different templating engine if you prefer, but you'll still need to keep
Heist around if you want to use Offset.

You can add or change routes in "src/Site.hs" within the function `site`. By
default it looks like this:

```(Haskell)
site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> homeHandler
             , path "blog" ==> blogHandler
             , path "echo" // segment ==> echoHandler
             , path "static" ==> staticServe "static",
             , anything ==> heistServe ]
             `fallthrough` notFoundText "Not found."
```
`route` is a function that takes a `RequestContext` of some type and a list
of routes. The routes are made up of segments that you can match on divided
by `//`, with an "arrow" `==>` to a handler for the matching requests. You
can read more about routes and handlers in the [Fn documentation](blah).

# Deploying to DigitalOcean with Circle CI and Quay

This is how I'm deploying my website, and you can too, because all the
necessary config files are here!

I used DigitalOcean's ready-made Docker image.

1. Create an account on [Quay](http://quay.io).
2. Create an account on [Circle CI](http://circleci.com) and set up this project with some environment
variables called "QUAY_USERNAME" and "QUAY_PASSWORD" for your Quay credentials.
3. Push to master on Github. Circle will build a tiny little Docker image for
your website and upload it to Quay!
4. Make sure that SSH is working on your DigitalOcean server. Ssh into your
server and `docker run -d --name=redis1 redis` Add you server's address to the Makefile.
5. Now you can run `make deploy`! That will copy your ".env" file to the server,
pull the latest Docker image from Quay, stop the previous Docker Image (if any), and
start the new one. <- Note lots of stuff can go wrong here so watch carefully!!

Repeat steps 3 and 5 as you make changes!

