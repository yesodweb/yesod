A Restful front controller built on Hack.

Below is an overview of how the refactored code will hopefully turn out.

# Terminology

## Verb

HTTP request methods. Possible values: GET, POST, PUT, DELETE. Please see
http://rest.blueoxen.net/cgi-bin/wiki.pl?MinimumMethods. In sum:

GET: Read only.
PUT: Replace data on server.
DELETE: Remove data from server.
POST: Some form of update.

Note: not all clients support PUT and DELETE. Therefore, we need a
workaround. There are two fixes:

1. X-HTTP-Method-Override header.
2. Get parameter _method_override (ie, in the query string). This will be more
useful for web forms.

See MethodOverride middleware.

## Resource

A Resource is a single URL. Each Resource can be addressed with the four verbs,
though does not necesarily support all of them. There are a few different ways
of passing parameters:

1. URL parameter. For example, you can set up your application so that
/articles/my_title/ will hand off to the article resource with a parameter of
my_title. The idea here is to avoid the regexs present in most other
frameworks and provide a uniform interface for all parameters.

2. Get parameter, ie query string.

3. Post parameter, ie content body. This is *not* available for GET requests.
However, it *is* available for PUT and DELETE.

4. Headers, including cookies.

### ResourceName

As a side point to the URL parameter mentioned above: let us say you have
multiple resources like /articles/my_title/ and /articles/other_title/. You
clearly do not want to write code twice to process these requests. Instead,
convert the article name into a URL parameter and then articles will have its
own ResourceName.

NOTE: This has taken on a very central role in the restful library, in addition
to the RestfulApp class. Hopefully I will have a chance to document this soon.
As a result, some of the following documentation is outdated.

### ResourceParser

A ResourceParser converts a Resource (ie, a URL) to a ResourceName and URL
parameters.

## RawRequest

The parsed data sent from the client. Has, for example, GET and POST
parameters, not QUERY_STRING and CONTENT_BODY.

## Request

Request is actually a **type class** for parsing a RawRequest. This allows
putting all form handling code and the like into a single place. Your handler
code (see below) need only deal with your data type.

## Representation

Some method of showing data to the client. These are identified by MIME types.
For the most part, you should be using hierarchichal data (see Data.Object).
There are some exceptions, such as:

* HTML for non-Ajax clients (search engines, old browsers).
* Atom/RSS feeds.
* Sitemaps.

Whenever possible, use Data.Object, as this will automatically handle some
basic representations (JSON, JSONP, Yaml, XML, undecorated HTML).

## Response

Contains basic HTTP response information and something which can be represented
in different ways.

## Handler

There is a single Handler for each combination of ResourceName and Verb. A
Handler takes some instance of Request and returns a Response.

### HandlerMap

Maps a ResourceName/Verb pair to a Handler.

## Application

An application is essentially a ResourceParser and HandlerMap. It also has some
settings involved.

# Static files

All static files should go under the /static/ path. A typical application will
probably have a Javascript file in there which deals directly with the entire
Restful API.

# Non-Ajax clients

Search engines nad older clients should not be ignored. However, it is quite
tedious to write view code twice. Hopefully, in the future there will be a view
component to this framework which can automate some of that process.

# Passing global data

You should use function currying to pass around global information (the list of
entries in a blog, a database connection, etc).
